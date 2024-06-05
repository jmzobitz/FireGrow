### Author: JMZ
### Last modified: 24/06/01
### Purpose: Read in the reflectance data from MODIS (used to process NDVI)



# Read in MOD and MYD data
inData_MOD <- read_csv('data-raw/firegrow-vi/86beaaee-f164-4b4d-8ccf-b781a58f97af/FireGrow-reflectance-MOD09GA-006-results.csv')
inData_MYD <- read_csv('data-raw/firegrow-vi/86beaaee-f164-4b4d-8ccf-b781a58f97af/FireGrow-reflectance-MYD09GA-006-results.csv')

# Just combine the names since we are combining the two together
names(inData_MYD) <- names(inData_MOD)

inData <- rbind(inData_MOD,inData_MYD)

# Define the bands and the flags
bands <- data.frame(
  band1 = inData$MOD09GA_006_sur_refl_b01_1,
  band2 = inData$MOD09GA_006_sur_refl_b02_1)

bandsFlag <- bands

# Determine the state of the measurement
stateFlagData <- data.frame(
  cloud_state = inData$MOD09GA_006_state_1km_1_cloud_state, #0b00
  cloud_shadow = inData$MOD09GA_006_state_1km_1_cloud_shadow,#0b0
  #  land_water=inData$`MOD09GA_006_state_1km_1_land/water_flag`,#0b001 (always because on land?)
  aerosol=inData$MOD09GA_006_state_1km_1_aerosol_quantity, #0b00
  cirrus=inData$MOD09GA_006_state_1km_1_cirrus_detected, #0b00
  internal_cloud=inData$MOD09GA_006_state_1km_1_internal_cloud_algorithm_flag, #0b0
  internal_fire=inData$MOD09GA_006_state_1km_1_internal_fire_algorithm_flag,#0b0
  #snow_ice=inData$`MOD09GA_006_state_1km_1_MOD35_snow/ice_flag`,#0b0
  next_to_cloud=inData$MOD09GA_006_state_1km_1_Pixel_is_adjacent_to_cloud, #0b0
  Salt_pan=inData$MOD09GA_006_state_1km_1_Salt_pan#0b0
  #internal_snow=inData$MOD09GA_006_state_1km_1_internal_snow_mask #0b0
)

state_flag_matrix <- stateFlagData

# Determine the QA Band
bandsFlagData <- data.frame(
  flag_band_1 = inData$MOD09GA_006_QC_500m_1_band_1_data_quality_four_bit_range,
  flag_band_2 = inData$MOD09GA_006_QC_500m_1_band_2_data_quality_four_bit_range)

bands_flag_matrix <- bandsFlagData

for (i in 1:dim(bandsFlagData)[2]) {
  bands_flag_matrix[,i] <- bandsFlagData[,i] |> str_detect("0b0000")
  bandsFlag[,i] <- bands[,i]!=-28672
}

for (i in 1:dim(stateFlagData)[2]) {

  state_flag_matrix[,i] <- stateFlagData[,i] |> str_detect("0b0|0b00")

}


# Set up the MODIS data
canada_modis_data <- inData |>
  select(ID,Date) |>
  mutate(bands_flags=rowSums(bands_flag_matrix),
         states_flag=rowSums(state_flag_matrix)) |>
  bind_cols(bands) |>
  filter(bands_flags ==2 & states_flag == 8) |>
  select(-bands_flags,-states_flag) |>
  mutate(ndvi = (band2 - band1)/(band2+band1) ) |>
  mutate(site=str_sub(ID,end=-3),
         sample=str_sub(ID,-1)) |>
  relocate(c("site","sample"),.after=ID)


# Save the data for future use
usethis::use_data(canada_modis_data,overwrite=TRUE)

