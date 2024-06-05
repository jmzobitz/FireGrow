### Author: JMZ
### Last modified: 24/06/01
### Purpose: Load in MODIS Terra and Aqua data to a common dataset


library(tidyverse)
library(lubridate)

# See: http://www.ntsg.umt.edu/files/modis/MOD17UsersGuide2015_v3.pdf (user guide download)
# https://ladsweb.modaps.eosdis.nasa.gov/filespec/MODIS/6/MYD17A2H  (for bit string info)

# see 'data-raw/bitString-work.xlsx'
qa_mask <- c(0,	1,	2,	3,
             8,	9,	10,	11,
             128,129,130,131,
             136,137,138,139)


gpp_data_myd <- read_csv('data-raw/firegrow-gpp/40ae50be-0689-4d53-a921-70c310c51573/2022-FireGrow-v2-MYD17A2H-006-results.csv') |> mutate(year=year(Date),day=yday(Date)) |>
  filter(MYD17A2H_006_Psn_QC_500m %in% qa_mask) |>
  mutate(site=str_sub(ID,end=-3),
         sample=str_sub(ID,-1)) |>
  mutate(MYD17A2H_006_Gpp_500m = if_else(day > 360, MYD17A2H_006_Gpp_500m/8, MYD17A2H_006_Gpp_500m/5 )) |>  # See the user guide for this step
  select(ID,site,sample,Date,MYD17A2H_006_Gpp_500m) |>
  rename(gpp=5)

gpp_data_mod <- read_csv('data-raw/firegrow-gpp/40ae50be-0689-4d53-a921-70c310c51573/2022-FireGrow-v2-MOD17A2H-006-results.csv') |> mutate(year=year(Date),day=yday(Date)) |>
  filter(MOD17A2H_006_Psn_QC_500m %in% qa_mask) |>
  mutate(site=str_sub(ID,end=-3),
         sample=str_sub(ID,-1)) |>
  mutate(MOD17A2H_006_Gpp_500m = if_else(day > 360, MOD17A2H_006_Gpp_500m/8, MOD17A2H_006_Gpp_500m/5 )) |> # See the user guide for this step
  select(ID,site,sample,Date,MOD17A2H_006_Gpp_500m) |>
  rename(gpp=5)




gpp_data <- rbind(gpp_data_mod,gpp_data_myd) |> arrange(Date) |> na.omit(gpp)



usethis::use_data(gpp_data,overwrite = TRUE)


