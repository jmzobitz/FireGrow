# Estimate the diffusivity from our measurements to compute D.
library(Fulbright)
library(tidyverse)

# Functions to compute the first and second derivatives
first_deriv_est_revised <- function(input_data) {

  diff_T <- vector("list",length=(dim(input_data)[1]-2))

  for (i in 2: length(diff_T)) {
    T_soil_1 <- input_data$T_soil[[i+1]]
    T_soil_2 <- input_data$T_soil[[i-1]]

    diff_T[[i]] <- tibble(Date = input_data$Date[[i]],
                          rate = 0.5*(T_soil_2-T_soil_1)/86400  # convert to degC / s
    )
  }

  return(bind_rows(diff_T))
}

second_deriv_est <- function(input_data) {

  second_deriv <- input_data %>% pivot_wider(names_from=layer,values_from=T_soil,  names_glue = "L{.name}") %>%
    mutate(second_deriv = 2*((L30-L10)/(.20*.25)-(L10-L5)/(.05*.25)) ) %>%
    select(second_deriv) %>% pull()
  # Units C / m^2
  return(tibble(layer = "L10",sdv = second_deriv))

}

# Compute the first derivatives
first_deriv_data <- soil_temperature_iButton_data %>% group_by(ID,layer) %>% nest() %>%
  mutate(first_deriv = map(data,first_deriv_est_revised)) %>%
  select(-data) %>%
  unnest(cols=c(first_deriv)) %>%
  ungroup()

# Compute the second derivative

# Nest each of the temperature measurements by site and Date, computing the second derivative.
second_deriv_data <- soil_temperature_iButton_data %>%
  mutate(layer = as.numeric(str_sub(layer,start=2))) %>%
  group_by(ID,Date) %>%
  nest() %>%
  mutate(sdv = map(data,second_deriv_est)) %>%
  select(-data) %>%
  unnest(cols=c(sdv)) %>%
  ungroup()

# Now join these together - yay!
diffusivity <- first_deriv_data %>%
  inner_join(second_deriv_data,by=c("ID","Date","layer")) %>%
  mutate(diffusivity = abs(rate/sdv)) %>%
  filter(rate!=0,sdv!=0)


# Look at the diffusivity
diffusivity %>% ggplot(aes(x=ID,y=diffusivity)) + geom_boxplot() + ylim(c(0,1E-7))



# Save the diffusivity data
usethis::use_data(diffusivity, overwrite = TRUE)
