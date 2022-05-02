# Compute the active layer depth via newton's method, knowing the thermal conductivity and some other things

library(FireGrow)
library(tidyverse)
library(lubridate)

# First get a smoothed temperature function over the year
canada_weather_ensemble <- canada_weather %>%
  select(date,mean_temp) %>%
  rename(Date=date) %>%
  group_by(Date) %>%
  dplyr::summarise(T_air = quantile(mean_temp, c(0.025, 0.5, 0.975),na.rm=TRUE), quantile = c(0.025, 0.5, 0.975))

# Then bring these together:
p1 <- canada_weather_ensemble %>%
  pivot_wider(names_from = quantile,values_from=T_air,names_prefix = "q") %>%
  ggplot(aes(x=Date,y=q0.5)) + geom_line(color='red') +
  geom_ribbon(aes(ymin=q0.025, ymax=q0.975), alpha=0.2,fill='red') +
  theme_fulbright() +
  labs(y = 'Air Temperature',x='Year') +
  scale_x_date(date_breaks = "years" , date_labels = "'%y",
               limits = c(as.Date('2010-01-01'), NA))



# Define the fitted function
c <- 2*pi
input_temp <- canada_weather_ensemble %>% filter(quantile==0.5) %>%
  mutate(time = decimal_date(Date)-2010,
         sin_coef = sin(c*time),
         cos_coef = cos(c*time))

# Do a fit of the periodic data - david periodic 1959
# sin(x+y) = sinx cos y + cos x sin y
# sin(x-y) = sin x cos y - cos x sin y
# we have T_0 + A_0 sin(w*t + phi) =
# T_0 + A_0 * sin(wt) * cos(phi) + A_0 cos(wt) * sin(phi)

# sin_coef --> a1 = cos(phi)  cos_coef --> b1 = sin(phi)
# phi = atan(sin_coef / cos_coef)


periodic_fit <- lm(T_air~sin_coef + cos_coef,data = input_temp)
summary(periodic_fit)

# Defining coeffieicnts
T_0 <- coefficients(periodic_fit)[1]
a1 <- coefficients(periodic_fit)[2]
b1 <- coefficients(periodic_fit)[3]

theta <- atan2(b1,a1)
A <- sqrt(a1^2+b1^2)
# OK, now fit a linear model

A_max_min = (max(input_temp$T_air,na.rm=TRUE)-min(input_temp$T_air,na.rm=TRUE))/2

fitted_temp <- input_temp %>% mutate(
  my_fit = T_0 + A*sin(c*time+theta),
  my_fit2 = T_0+A_max_min*sin(c*time+theta))
# Make a plot to show the ensemble values
p1 +
  geom_line(data=fitted_temp,aes(x=Date, y=my_fit)) +
  geom_line(data=fitted_temp,aes(x=Date, y=my_fit2),color='blue')


# OK, so let's just use 1 years worth of data

short_time <- fitted_temp %>% filter(time < 1)

# Now define a function for the snow depth
soil_temp_depth <- function(z,time,T_0,A_0,D,c,theta,fS=0,DS=1) {
  sqrt_val <- sqrt(c/(2*D))

  temp <- T_0 + A_0*exp(-sqrt_val*z)*exp(-fS*DS)*sin(c*time+theta-sqrt_val*z)

  return(temp)
}

# Next we define a function that for a given timeseries computes the snow depth

compute_depth_timeseries <- function(times,T_0,A_0,D,c,theta,fS=0,DS=1) {

  out_depths <- vector(mode="list",length=length(times))
  for(i in seq_along(out_depths)) {


    out_depths[[i]] <- active_depth_bisection(0,times[[i]],T_0,A_0,D,c,theta,fS,DS)

  }

  out_values <- bind_rows(out_depths) %>% mutate(time=1:n())
  return(out_values)



}

# Success!  Now we just need to loop across each of the different sites and diffusivities - cool!

diffusivity_list <- diffusivity %>%
  mutate(diffusivity = diffusivity*86400*365) %>%  # Change to per year
  group_by(ID,Date) %>% nest()


# Now we are going to compute these out:
active_layer_results <- diffusivity_list %>%
  mutate(depth = map(.x=data,
                     .f=~compute_depth_timeseries(short_time$time,T_0,A_max_min,.x$diffusivity,c,theta)))

# Save the data
usethiss::use_data(active_layer_results,overwrite=TRUE)

