### Author: JMZ
### Last modified: 24/06/01
### Purpose: Read in and prepare the different parameters used in each model



# Read in parameters
input_parameters <- readxl::read_xlsx('param-values/soil-model-param.xlsx') |>
  rename(N1969 = N1968) |>
  pivot_longer(cols=c("N2012","N1990","N1969","NC"),names_to="site",values_to="value") |>
  group_by(site) |>  # nest them by the site
  nest() |>
  rename(parameters = data)

# Join the forcing data with the parameters in a nice nested frame! - these parameters are specific to a site, but not to a model ....
modeling_param_data <- modeling_data |>
  inner_join(input_parameters,by="site")


usethis::use_data(modeling_param_data,overwrite = TRUE)


