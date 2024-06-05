### Author: JMZ
### Last modified: 24/06/01
### Purpose: Define a set of helper functions to compute and run the parameter estimation.  The main lifting is done by the function metropolis_estimate, which runs and saves all parameter estims.



forcing_model_ready_input2 <- forcing_model_ready_input |>
  mutate(parameters = pmap(.l=list(parameters,estimated_params,which_parameters),
                           .f=~( ..1 |>
                                   rows_update(..2,by="name") |>
                                   filter(name %in% ..3$name) |>
                                   as_tibble()))) |> ### Now update parameters and filter out only for the ones we need for the model
  select(-which_parameters,-estimated_params) |> # cleanup
  group_by(Year,depth,model,parameters) |>
  nest() |>
  left_join(field_data,by="Year") |>
  rename(modeling_data = data) #|>


# 1) Write code that can do the super quick analysis using a parameter value across all the sites
# This function runs the model at a given site and produces the output
# NOTE: make sure parameters are updated to the correct timescale (perhaps later to a command check here ...)
solve_nest_model_fall <- function(equations,initial_conditions,data,model_parameters) {
  initial_conditions <- initial_conditions(model_parameters)

  assign_signals(data)


  new_parameters <- model_parameters |>
    select(name,value) |>
    deframe()

  ### Times is a data frame that comes from signal_approx
  out_fluxes <-  equations(times,new_parameters,initial_conditions)$respiration |>
    mutate(Date = data$Date) |>
    relocate(Date) |>
    mutate(month = lubridate::month(Date),
           year = lubridate::year(Date))


  return(out_fluxes)
}

run_model_nest_revised <- function(input_model_data,
                                   model_parameters,
                                   field_data,
                                   input_month) {


  input_model_data |>
    mutate(results = pmap(.l=list(equations,initial_conditions,data),.f=~solve_nest_model_fall(equations = ..1,
                                                                                          initial_conditions = ..2,
                                                                                          data = ..3,
                                                                                          model_parameters = model_parameters) ) ) |>
    mutate(medianR = map_dbl(.x=results,.f=~(  .x |>
                                            filter(month %in% input_month) |>
                                            #group_by(year) |>
                                            summarize(medianR = median(soil_R,na.rm=TRUE)) |> pull(medianR)))) |>

    select(ID,results,medianR) |>
    separate(col=ID,into=c("Year","Area")) |>
     left_join(field_data,by=c("Area"))

}

# Test the code
i<-3
output_n <- run_model_nest_revised(
  input_model_data = forcing_model_ready_input2$modeling_data[[i]],
  model_parameters = forcing_model_ready_input2$parameters[[i]],
  field_data = forcing_model_ready_input2$field_data[[i]],
  input_month = 8
)



### Now determine random parameters (does a single sample of a parameter)
update_random_parameters <- function(input_parameters) {

  sensitivity_parameters <- input_parameters |>
    filter(model_forcing_estimate) |>
    sample_n(size=1) |>   ### only select one value
    mutate(value = runif(1,min=min_value,max=max_value))

  ### Now update parameters and filter out only for the ones we need for the model
  site_model_params <- input_parameters |>
    rows_update(sensitivity_parameters,by="name") |>
    as_tibble()

  return(site_model_params)
}# select random parameters


# This function runs the model at a given site and produces the output
run_model_revised_random <- function(input_model_data,
                                     input_field_data,
                                     model_parameters,
                                     input_month=8) {



  new_random_params <- update_random_parameters(model_parameters)


  short_list <- new_random_params

  # Make sure things are on the correct timescale for these.
  correct_time_params <- parameter_time_convert(new_random_params)
  # Run the model across each of the three sites, do the aggregation
  output_n <- run_model_nest_revised(
    input_model_data = input_model_data,
    model_parameters = correct_time_params,
    field_data = input_field_data,
    input_month = input_month)

  output_check <- output_n |>
  summarize(rss=sum((Rsoil-medianR)^2),
            accept = FALSE,
            check = runif(1) > cor(Rsoil,medianR)^2,
            finite_rss = is.finite(rss))


# Note: https://en.wikipedia.org/wiki/Coefficient_of_determination says that r^2 (given by cor) is the same as R^2 because we only have one predictor (Rsoil and median R)
  out_list <- list(parameters = short_list,output_resp = select(output_n,Year,Area,results),output_test = output_check)


  return(out_list)


}


# Test the code
i<-20
output_test <- run_model_revised_random(
  input_model_data = forcing_model_ready_input2$modeling_data[[i]],
  model_parameters = forcing_model_ready_input2$parameters[[i]],
  input_field_data = forcing_model_ready_input2$field_data[[i]],
  input_month = 8
)

# Define a metropolis estimate for a given number of simulations
metropolis_estimate <- function(model_data,field_data,parameters,n_sims) {

  # Initialize everything
  model_out <- run_model_revised_random(
    input_model_data = model_data,
    input_field_data = field_data,
    model_parameters = parameters)

  curr_stats <- model_out$output_test
  curr_params <- model_out$parameters

  ### Now apply the metropolis hastings
  out_stats <- vector(mode = "list",length = n_sims)
  out_params <- vector(mode = "list",length = n_sims)
  out_fluxes <- vector(mode = "list",length = n_sims)



  for (sim_count in 1:n_sims) {

    proposed_stats <- run_model_revised_random(
      input_model_data = model_data,
      input_field_data = field_data,
      model_parameters = curr_params)


    if(proposed_stats$output_test$rss < curr_stats$rss  | proposed_stats$output_test$check & proposed_stats$output_test$finite_rss) {
      proposed_stats$output_test$accept <- TRUE
      curr_stats <- proposed_stats$output_test
      curr_params <- proposed_stats$parameters
    }

    out_stats[[sim_count]] <- proposed_stats$output_test
    out_params[[sim_count]] <-proposed_stats$parameters |> filter(model_forcing_estimate) |> select(name,value)
    out_fluxes[[sim_count]] <- proposed_stats$output_resp

  }



  out_values <- tibble(sims = 1:n_sims,
                       stats = out_stats,
                       params = out_params,
                       fluxes = out_fluxes) |>
    unnest(cols=c(stats)) |>
    select(-check) |>
    slice_tail(prop=.6) |> ### Chose the last 60%
    filter(accept) # Just make sure we are only take parameter sets that were accepted


  return(out_values)


}

i<-20  # Null
i<-9    # Quality, NC
i<-1  # Microbe, 2012

### Can we do a smaller dataset?
 out_estimates <- metropolis_estimate(model_data = forcing_model_ready_input2$modeling_data[[i]],
                             field_data = forcing_model_ready_input2$field_data[[i]],
                             parameters = forcing_model_ready_input2$parameters[[i]],
                             n_sims = 100)

# Works!


 ### Now lets get ready to estimate everything!

curr_sims <- 2000
for (i in 1:nrow(forcing_model_ready_input2)) {

  print(i)

  out_tibble <- metropolis_estimate(model_data = forcing_model_ready_input2$modeling_data[[i]],
                                    field_data = forcing_model_ready_input2$field_data[[i]],
                                    parameters = forcing_model_ready_input2$parameters[[i]],
                                    n_sims = curr_sims)

  filename <- paste0('parameter-estimation-outputs/estimates-no-swc/',
                     forcing_model_ready_input2$Year[[i]],'--',
                     forcing_model_ready_input2$model[[i]],'--',
                     forcing_model_ready_input2$depth[[i]],'.Rda')

  save(out_tibble,file = filename)


}

