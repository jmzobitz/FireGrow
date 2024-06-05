### Author: JMZ
### Last modified: 24/06/01
### Purpose: Load up the field data for use in the optimization


field_data <- readxl::read_xlsx('data-raw/Modeling data Canada North.xlsx') |>
  select(1,8,9,12,15:20) |>
  separate(1,into=c("Year","Plot")) |>
  separate(Plot,into=c("Area","Plot"),sep=1) |>
  #mutate(Year = if_else(Year=="N1969","N1968",Year)) |>  # Change 1969 to 1968
  mutate(
    Plot = as.numeric(Plot),
    respiration = `Soil Respiration, g/CO2/m2/h`* (12 / 44 ) * (24),  # Convert to g C / m2 day
  )  |>
  select(Year,Area,Plot,respiration) |>
  group_by(Year,Area) |>
  summarize(Rsoil = median(respiration)) |>
  mutate(Area = if_else(Year == "N2012",paste0("FB",Area),Area)) |>
  group_by(Year) |>
  nest() |>
  rename(field_data = data)


usethis::use_data(field_data,overwrite = TRUE)
