### Author: JMZ
### Last modified: 24/06/01
### Purpose: Analyze sensitivity / model forcing results that were consistent with observations.  Used to produce figures



### Helper function to make a boxplot:
calculate_double_boxplot <- function(in_tibble) {


  filtered_fluxes_cumulative <- in_tibble |>
    #filter(rss < 1e1) |> # Cutoff badd rss
    unnest(cols=c(fluxes)) |> unnest(cols=c(results)) |>
    filter(Date >= as.Date("2016-01-01")) |>
    mutate(soil_R_nan = if_else(root_R == 0,NA,soil_R)) |>
    group_by(year,sims) |>
    summarize(across(.cols=c("root_R":"soil_R","soil_R_nan"),.fns=~sum(.x,na.rm=TRUE)) ) |>
    mutate(RA = root_R/soil_R,
           RA_gs = root_R/soil_R_nan)


  filtered_params_cumulative <- in_tibble |>
   # filter(rss < 1e1) |> # Cutoff badd rss
    unnest(cols=c(params)) |>
    select(-fluxes) |>
    filter(str_detect(name,"_litter")) |>
    group_by(sims) |>
    summarize(annual_litter = sum(value)*365)

  in_plot <- filtered_params_cumulative |> ggplot() + geom_boxplot(aes(y=annual_litter))
  plot.x <- layer_data(in_plot)[,1:6]
  colnames(plot.x) <- paste0("x.", gsub("y", "", colnames(plot.x)))
  litter_plot.x <- select(plot.x,-x.outliers)

  litter_plot.x <- mutate(litter_plot.x,name="annual_litter")


  results <- filtered_fluxes_cumulative |>
    #inner_join(filtered_params_cumulative,by="sims") |>
    pivot_longer(cols=c("root_R":"RA_gs")) |>
    group_by(name) |>
    nest() |>
    mutate(vals = map(.x=data,.f=function(x) {in_plot <- x |> ggplot() + geom_boxplot(aes(y=value))
    plot.x <- layer_data(in_plot)[,1:6]
    colnames(plot.x) <- paste0("x.", gsub("y", "", colnames(plot.x)))
    plot.x <- select(plot.x,-x.outliers)
    return(plot.x)})) |>
    select(name,vals) |>
    unnest(cols=c(vals)) |>
    rbind(litter_plot.x)



  return(results)



}


# Run these at target values
### STEP 0: Read in simulation results. Keep track of files that didn't work (we need to investigate what is happening)

input_files <- list.files(path = 'parameter-estimation-outputs/estimates-no-swc',pattern="^N",full.names = TRUE)

# Define a list of vectors for different outputs
filtered_fluxes <- vector(mode="list",length=length(input_files))
filtered_fluxes_cumulative <- vector(mode="list",length=length(input_files))
out_params <- vector(mode="list",length=length(input_files))
boxplot_data <- vector(mode="list",length=length(input_files))

for(i in seq_along(input_files)) {

  print(i)
  load(input_files[[i]])


  # # Figure out if there is a median value - get the good values for each one - this tells us the frequency of "acceptable" values
  # filtered_results <- out_tibble |>
  #   hoist(flux,median_vals = "median_vals") |>
  #   mutate(n_sim = 1:n()) |>
  #   unnest(cols=c(median_vals)) |>
  #   mutate(n_obs = map_dbl(.x=median_vals,.f=~(.x |> filter(year>2015) |> pull(cdf_value) |> between(.25,.75) |> sum()))) |>
  #   filter(n_obs > 0)


  filtered_fluxes[[i]] <- out_tibble |>
   # filter(rss < 1e1) |> # Cutoff badd rss
    unnest(cols=c(fluxes)) |> unnest(cols=c(results)) |>
    filter(Date >= as.Date("2016-01-01")) |>
    mutate(doy = yday(Date),
           RA= if_else(soil_R == 0,0,root_R/soil_R)  # Just make sure we aren't getting any funky values
           ) |>  # Modify this accordingly
    pivot_longer(cols=-c("sims","rss","accept","params","Year","Area","Date","month","year","doy")) |>
    group_by(doy,name) |>
    reframe(value = quantile(value, c(0.25, 0.5, 0.75),na.rm=TRUE),
            quantile = c("q0.25", "q0.5", "q0.75") )


  filtered_fluxes_cumulative[[i]] <- out_tibble |>
    #filter(rss < 1e1) |> # Cutoff badd rss
    unnest(cols=c(fluxes)) |> unnest(cols=c(results)) |>
    filter(Date >= as.Date("2016-01-01")) |>
    group_by(Year,year,sims) |>
    summarize(across(.cols=c("root_R":"soil_R"),.fns=sum) ) |>
    mutate(RA = root_R/soil_R) |>
    pivot_longer(cols=root_R:soil_R) |>
    group_by(Year,name) |>
    reframe(value = quantile(value, c(0.25, 0.5, 0.75),na.rm=TRUE),
            quantile = c("q0.25", "q0.5", "q0.75") )


  boxplot_data[[i]] <- calculate_double_boxplot(out_tibble)

       out_params[[i]] <-  out_tibble |>
     select(sims,params)


}


model_sensitivity_data <- tibble(
  Year = map_chr(.x=input_files,.f=~str_extract(.x,pattern="(?<=estimates-no-swc/)[:alnum:]+(?=--)")),
  model = map_chr(.x=input_files,.f=~str_extract(.x,pattern="(?<=--)[:graph:]+(?=--)")),
  depth = map_chr(.x=input_files,.f=~str_extract(.x,pattern="T_soil_[:digit:]{1,2}")),
 # value = histogram_data,
  params = out_params,
 # fluxes = out_fluxes,
  filtered_fluxes = filtered_fluxes,
 filtered_fluxes_cumulative = filtered_fluxes_cumulative,
 boxplot_data = boxplot_data
#  filtered_sims = filtered_sims
)



save(model_sensitivity_data,file='parameter-estimation-outputs/parameter-estimate-no-swc-summary.Rda')

