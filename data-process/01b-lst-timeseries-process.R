### Author: JMZ
### Last modified: 24/06/01
### Purpose: Code that computes the LST timeseries for each site and year -


# Nest the data together

### So now we want to set where the land surface temperature is freezing (273.15) to make GPP 0 - this expands out the data that we can use.

lst_data_revised <- lst_data |>
  mutate(day_year = yday(Date)) |>
  na.omit(lst)  # Remove NA's

#gpp_data_revised |> ggplot(aes(x=Date,y=gpp)) + geom_point() + facet_wrap(.~site)

nested_lst <- lst_data_revised |> mutate(year=lubridate::year(Date)) |>
  rename(measurement=lst) |>  ### Need to make this consistent
  group_by(ID,year) |> nest()

uncertainty_lst <- data.frame(sigma=2,type='rmse')  # Setting this to Ghent 2019

# Now apply these to each of the entries using the map function
lst_timeseries <- nested_lst |>
  filter(year > 2008) |>   # Remove entries that have more than one year
  mutate(smoothed_lst = map(.x=data,~smoother_compute(.x,uncertainty_lst))) |>
  select(smoothed_lst) |> # Select only the smoothed data
  unnest(cols = c(smoothed_lst)) |> # Unnest them into a data frame
  #mutate(ID=if_else(str_detect(ID,".+(?=(-JZ))"),str_extract(ID,".+(?=(-JZ))"),ID)) |>  # Some string cleanup here.  * sigh *
  mutate(site=str_extract(ID,"^[:alnum:]*(?-)"),
         sample=str_extract(ID,"(?<=-)[:alnum:]*$")) |>
  ungroup() |>
  select(-year) |>
  rename(lst = measurement) |>
  relocate(ID,site,sample,Date,lst)

# Solve these all out together
usethis::use_data(lst_timeseries,overwrite = TRUE)



