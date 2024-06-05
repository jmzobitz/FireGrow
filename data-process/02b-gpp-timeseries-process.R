### Author: JMZ
### Last modified: 24/06/01
### Purpose: Code that computes the GPP timeseries for each site and year - smoothing for GPP values for each of the sites we examined here.


### So now we want to set where the land surface temperature is freezing (273.15) to make GPP 0 - this expands out the data that we can use.

gpp_data_revised <- lst_data |>
  left_join(gpp_data,by=c("ID","site","sample","Date")) |>
  mutate(day_year = yday(Date)) |>
 # mutate(gpp = if_else(day_year > 330 | day_year < 30,0,gpp)) |>  # Introduce a condition of zero GPP for the last month of the year
mutate(gpp=if_else(lst < 273.15,0,gpp)) |> # Mutate to change GPP values
#filter(lst >= 273.15) |> # Mutate to change GPP values
select(-lst,-day_year) |> # Remove LST
  na.omit(gpp)  # Remove NA's

#gpp_data_revised |> ggplot(aes(x=Date,y=gpp)) + geom_point() + facet_wrap(.~site)

nested_gpp <- gpp_data_revised |>
  mutate(year=lubridate::year(Date)) |>
  rename(measurement=gpp) |>  ### Need to make this consistent
  group_by(ID,year) |> nest()

uncertainty_gpp <- data.frame(sigma=.5,type='rmse_percent')  # Setting this to 50% of RMSE / Mean (see Wang et al 2017, so this is a percent

# Now apply these to each of the entries using the map function
gpp_timeseries <- nested_gpp |>
  filter(year > 2008) |>   # Remove entries that have more than one year
  mutate(smoothed_gpp = map(.x=data,.f=~smoother_compute(.x,uncertainty_gpp))) |>
  select(smoothed_gpp) |> # Select only the smoothed data
  unnest(cols = c(smoothed_gpp)) |> # Unnest them into a data frame
  #mutate(ID=if_else(str_detect(ID,".+(?=(-JZ))"),str_extract(ID,".+(?=(-JZ))"),ID)) |>  # Some string cleanup here.  * sigh *
  mutate(site=str_extract(ID,"^[:alnum:]*(?-)"),
       sample=str_extract(ID,"(?<=-)[:alnum:]*$")) |>
  ungroup() |>
  select(-year) |>
  rename(gpp = measurement) |>
  relocate(ID,site,sample,Date,gpp) |>
  mutate(gpp=as.vector(gpp))

# Solve these all out together
usethis::use_data(gpp_timeseries,overwrite = TRUE)



