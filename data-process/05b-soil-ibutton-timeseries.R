### Author: JMZ
### Last modified: 24/06/01
### Purpose: Parameterize a model of soil temperature with collected IButton Data


# Main model: T = T_0 + A * exp(-kz) sin(wt + phi - kz)
# T = T_0 + A_bar sin(wt + phi_bar) where
# A_bar = A*exp(-kz), phi_bar = phi - kz
# k = sqrt(w/2D)
# w = 2pi
# t is measured in fraction of a year, z in meters


# Do a fit of the periodic data - david periodic 1959
# we have T = T_0 + A_bar sin(wt + phi_bar)  =
# sin(x+y) = sinx cos y + cos x sin y
# T_bar =T_0 +  A_bar * sin(wt) * cos(phi_bar) + A_bar cos(wt) * sin(phi_bar)
# A_bar determined by max and min.

# So our fitted model is T_bar = T_0 + a1 * sin_coef + b1 cos_coef

### 1) Determine constants:
w <- 2*pi

### 2) Extract out the data at each layer, create linear fit coefficients, and adjust by the mean temperature
soil_data_layer <- soil_temperature_iButton_data |>
  mutate(layer = str_extract(layer,pattern="[^[:alpha:]]+")) |>
  mutate(layer = as.numeric(layer),
         year = lubridate::year(Date),
         frac_year = lubridate::decimal_date(Date)-2018,
         sin_coef = sin(w*frac_year),
         cos_coef = cos(w*frac_year)) |>
  group_by(ID,layer) |>
  mutate(sin_coef = 0.5*(max(T_soil)-min(T_soil))*sin_coef,
         cos_coef = 0.5*(max(T_soil)-min(T_soil))*cos_coef ) |>
  nest() |>
  mutate(A_bar = map(.x=data,.f=~0.5*(max(.x$T_soil)-min(.x$T_soil))))

### 3) Do a fit for each year

fit_vals <- soil_data_layer |>
  mutate(periodic_fit = map(.x=data,.f=~lm(T_soil~1+sin_coef + cos_coef,data=.x)),
         summary_vals = map(.x=periodic_fit,.f=~broom::glance(.x)),
         coeff = map(.x=periodic_fit,.f=~broom::tidy(.x)))

# Compute the correspondence between the different terms
correspondence = tibble(term = fit_vals$coeff[[1]]$term,
                        name = c("T_0","a1","b1"))

# Set up the data frame of coefficients
# We do some joining here and the slick within command to successively add the values to our data frame - woot!
soil_temp_fit_vals <- fit_vals |>
  mutate(out_coeffs = map2(.x=coeff,.y=data,.f=~(
    .x |> inner_join(correspondence,by="term") |> select(name,estimate) |> pivot_wider(names_from="name",values_from="estimate") |>
      within( { # Add to this data frame
        phi_bar <- atan2(b1,a1)  # The second component goes first
      }) ))) |>
  select(-c("periodic_fit","coeff")) |>
  mutate(out_coeffs=map2(.x=out_coeffs,.y=A_bar,.f=~(cbind(.x,.y) |> rename(A_bar=".y")))) |>
  select(-A_bar)


################

### 5) Infer params for N1968 site. Now for N1968 we will just take the average across all the different values (we tried to a linear regression but it was not significant)
N1968_params <- soil_temp_fit_vals |>
  unnest(cols=c("out_coeffs")) |>
  pivot_longer(cols=c("T_0":"A_bar")) |>
  group_by(layer,name) |>
  summarize(value = mean(value),ID="N1968") |>
  pivot_wider() |>
  group_by(ID,layer) |>
  nest() |>
  rename(out_coeffs=data)


# Bind these up
fit_params_adj <- rbind(select(soil_temp_fit_vals,ID,layer,out_coeffs), N1968_params)


### 6) Create a timeseries of data Ok! We can do the timeseries now!
start <- gpp_timeseries$Date |> min()
year_start <- gpp_timeseries$Date |> year() |> min()
end <- gpp_timeseries$Date |> max()
test_date <- tibble(date = seq(start, end, by = "1 days"),
                    frac_year = decimal_date(date)-year_start,
                    sin_coef = sin(w*frac_year),
                    cos_coef = cos(w*frac_year))


## T = T_0 + A_bar sin(wt + phi_bar) where
soil_temp_timeseries <- fit_params_adj |>
  mutate(T_soil = map(.x=out_coeffs,.f=~(cbind(test_date,
                                               T_soil=.x$T_0+.x$A_bar*sin(w*test_date$frac_year+.x$phi_bar)))))


# Solve these all out together
usethis::use_data(soil_temp_timeseries,overwrite = TRUE)
