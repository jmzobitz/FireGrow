### Author: JMZ
### Last modified: 24/06/01
### Purpose: Load in and plot the LST for each site

#



# We found the QA bits in the documentation where the error was less than 1 kelvin.  Whew!
# Helpful site: https://www.r-bloggers.com/2012/12/modis-qc-bits/

# Determined from some clever interpretation of the QA bits.  See lab notebook pg 8
# see 'data-raw/bitString-work.xlsx'
qa_bits <- c(0, 4, 8, 12, 1, 5, 9, 13,
             16, 20, 24, 28, 17, 21, 25, 29,
             32, 36, 40, 44, 33, 37, 41, 45,
             48, 52, 56, 60, 49, 53, 57, 61,
             64, 68, 72, 76, 65, 69, 73, 77,
             80, 84, 88, 92, 81, 85, 89, 93,
             96, 100, 104, 108, 97, 101, 105, 109,
             112, 116, 120, 124, 113, 117, 121, 125)






lst_data_mod <- read_csv('data-raw/firegrow-gpp/7192b05f-487c-4167-948e-ae21d5bdd00f/Fire-Grow-LST-MOD11A2-006-results.csv') |> mutate(year=year(Date),day=yday(Date)) |> filter(MOD11A2_006_QC_Day %in% qa_bits) |>
  mutate(site=str_sub(ID,end=-3),
         sample=str_sub(ID,-1)) |>
  select(ID,site,sample,Date,MOD11A2_006_LST_Day_1km) |>
  rename(lst=5)


lst_data_myd <- read_csv('data-raw/firegrow-gpp/7192b05f-487c-4167-948e-ae21d5bdd00f/Fire-Grow-LST-MYD11A2-006-results.csv') |> mutate(year=year(Date),day=yday(Date)) |> filter(MYD11A2_006_QC_Day %in% qa_bits) |>
  mutate(site=str_sub(ID,end=-3),
         sample=str_sub(ID,-1)) |>
  select(ID,site,sample,Date,MYD11A2_006_LST_Day_1km) |>
  rename(lst=5)




# Arrange the data together
lst_data <- rbind(lst_data_mod,lst_data_myd)  |> arrange(Date)


# Save for later use
usethis::use_data(lst_data,overwrite = TRUE)



