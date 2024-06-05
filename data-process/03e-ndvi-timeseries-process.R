### Author: JMZ
### Last modified: 24/06/01
### Purpose: Code that computes the LST timeseries for each site and year -



### So now we want to set where the land surface temperature is freezing (273.15) to make GPP 0 - this expands out the data that we can use.

ndvi_data_revised <- canada_modis_data |>
  mutate(year = year(Date),
    day_year = yday(Date)) |>
  na.omit(ndvi)  # Remove NA's


nested_ndvi <- ndvi_data_revised |>
  mutate(year=lubridate::year(Date)) |>
  rename(measurement=ndvi) |>  ### Need to make this consistent
  group_by(ID,year) |>
  nest()

uncertainty_ndvi <- data.frame(sigma=.035,type='rmse')  # See 03d-ndvi-uncertainty-process.R

# Now apply these to each of the entries using the map function
ndvi_timeseries <- nested_ndvi |>
  filter(year > 2008) |>   # Remove entries that have more than one year
  mutate(smoothed_ndvi = map(.x=data,~smoother_compute(.x,uncertainty_ndvi))) |>
  select(smoothed_ndvi) |> # Select only the smoothed data
  unnest(cols = c(smoothed_ndvi)) |> # Unnest them into a data frame
  #mutate(ID=if_else(str_detect(ID,".+(?=(-JZ))"),str_extract(ID,".+(?=(-JZ))"),ID)) |>  # Some string cleanup here.  * sigh *
  mutate(site=str_extract(ID,"^[:alnum:]*(?-)"),
         sample=str_extract(ID,"(?<=-)[:alnum:]*$")) |>
  ungroup() |>
  select(-year) |>
  rename(ndvi = measurement) |>
  relocate(ID,site,sample,Date,ndvi)

# Solve these all out together
usethis::use_data(ndvi_timeseries,overwrite = TRUE)



