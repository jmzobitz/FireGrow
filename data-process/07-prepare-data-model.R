### Author: JMZ
### Last modified: 24/06/01
### Purpose: Join the different timeseries for modeling together, grouping by the depth and the site ID.


library(tidyverse)
library(FireGrow)


swc_timeseries_adj <- swc_timeseries |>
  mutate(site = str_extract(ID,"^[:alnum:]*(?-)") )

soil_temp_timeseries_adj <- soil_temp_timeseries |>
  mutate(ID = if_else(ID=="N1968","N1969",ID)) |>
  #select(-data) |>
  unnest(cols=c("T_soil")) |>
 # pivot_longer(cols=c("T_soil_5","T_soil_10"),names_to="layer",values_to = "T_soil") |>
  select(ID,layer,date,T_soil)

modeling_data <- gpp_timeseries |>
  inner_join(swc_timeseries_adj,by=c("ID","site","sample","Date")) |>
  inner_join(soil_temp_timeseries_adj,by=c("site"="ID","Date"="date")) |>
  rename(soilT = T_soil,depth=layer) |>
  mutate(gpp = gpp*1000) |>
  #filter(Date >= as.Date('2015-09-01')) |>
  group_by(ID,depth) |>
  nest() |>
  filter(ID %in% c("N1969-A","N1969-B","N1969-C" ,"N1990-A","N1990-B","N1990-C","N2012-FBA","N2012-FBB","N2012-FBC","NC-A","NC-B","NC-C")) |>  # Remove the non fire pixels from 2012
  separate(col="ID",into=c("site",NA),sep="-",remove=FALSE)  # Just do some cleanup here

usethis::use_data(modeling_data,overwrite = TRUE)
