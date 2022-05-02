# Use the canada data download to see how this works

library(weathercan)
library(tidyverse)

# Search for stations
stations <- read_csv('data-raw/appears_data.csv')

# How many dates do we have?
n_sites <- dim(stations)[1]

# Define the out vector
out <- vector("list", n_sites)


for (i in seq_along(out)) {

  out[[i]] <- stations_search(coords=c(stations$latitude[i],stations$longitude[i]),interval="day",dist = 30) # Pull all stations within 30 km of our site
}

out_vals <- bind_rows(out) %>%
  distinct(station_id,.keep_all=TRUE)


canada_weather <- weather_dl(station_ids = pull(out_vals,station_id), start = "2010-01-01", end = "2020-12-31",interval="day")

usethis::use_data(canada_weather,overwrite = TRUE)

