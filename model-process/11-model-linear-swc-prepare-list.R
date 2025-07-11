### Author: JMZ
### Last modified: 24/06/01
### Purpose: Create a nested list for all the modeling data in a concise frame for analysis. Uses results from Zobitz et al 2022.


# This sets the three models and determines what parameters we want to pull. We are selecting the incubation-field model parameters from Zobitz et al 2022
which_model <- read_csv('param-values/soil-model-param-type.csv') |>
  select(-description,-units) |>
  pivot_longer(cols=c(-"name"),names_to = "model") |>
  filter(value) |>
  group_by(model) |>
  nest() |>
  filter(model %in% c("null-incubation-field","microbe-incubation-field","quality-incubation-field")) |>
  mutate(model = str_extract(model,pattern = "[:alpha:]*(?=-)"))  # Just extract out the first name of the model


# Load up parameter values from Zobitz et al 2022
load('data-raw/incubation-field-approach-results.Rda')

### Load up the minimum RSS from the estimated parameter values
estimated_params_by_model <- incubation_field_approach_results |>
  filter(Year !="NALL",depth %in% c(5,10),
         model %in% c("null","microbe","quality")) |> # Select only parameters which we want
  group_by(Year,depth,model) |>
  nest() |>
  mutate(good_params = map(.x=data,.f=~(.x |> slice(which.min(rss)) ) )) |>
  hoist(good_params,estimated_params = "params" ) |>
  unnest(cols=c(estimated_params)) |>
  select(Year,depth,model,estimated_params)


### Join the functions with the estimated parameters, initial conditions, together
model_ready_functions <- model_functions_list |>
  inner_join(initial_conditions_list,by="model") |>
  inner_join(which_model,by="model") |>
  rename(which_parameters = data) |>
  right_join(estimated_params_by_model,by="model") |> ### This will serve as a double check for the initial conditions too!
  group_by(Year,depth) |>
  nest() |>
  mutate(Year = if_else(Year =="N1968","N1969",Year)) |>
  rename(estimate_data = data) |>
  mutate(depth = paste0("T_soil_",depth))  # Add on this to the soil depth for joining

# Clean up the modeling data and nest together for joining
modeling_param_data_nest <- modeling_param_data |>
  rename(Year = site) |>
  group_by(Year,depth) |>
  nest() |>
  rename(site_data = data) |>
  mutate(depth = paste0("T_soil_",as.character(depth)))

# Join the model, data, parameters at each of the depths with the functions and initial conditions -
# estimate_data: functions, estimated parameters
# site_data: environmental forcing data, overall parameters
# The unnested list will then have all the different model options with the estimated parameters, ready for simulation!

forcing_model_ready_input <- model_ready_functions |>
  inner_join(modeling_param_data_nest,by=c("Year","depth")) |>
  unnest(cols=c("site_data")) |>
  unnest(cols=c("estimate_data"))

