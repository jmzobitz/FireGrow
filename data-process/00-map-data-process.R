### Author: JMZ
### Last modified: 24/06/01
### Purpose: Code that allows you to process and save up the maps  - this runs and saves all the files

library(MODISTools)
library(leaflet)
library(sf)

# Load up data on all the sites
sites <- read_csv('data-raw/appears_data.csv') |> select(-Category) |>
  rename(site_name = ID,
         lat = latitude,
         long = longitude)



save(file = "data-raw/sites.Rda",sites)


# Use MODIS tools to get a subset of the sites so we know which tiles to work from.

out_list <- vector("list",length=dim(sites)[1])
bb <- vector("list",length=dim(sites)[1])

for(i in seq_along(out_list)) {
  print(i)
  # Grab the data
  subset <- MODISTools::mt_subset(product = "MOD11A2",
                                  lat = sites$lat[[i]],
                                  lon = sites$long[[i]],
                                  band = "LST_Day_1km",
                                  start = "2020-06-01",
                                  end = "2020-06-15",
                                  progress = FALSE)

  # convert sinusoidal to lat / lon
  lat_lon <- MODISTools::sin_to_ll(subset$xllcorner, subset$yllcorner)

  # bind with the original dataframe
  out_list[[i]] <- cbind(subset, lat_lon)

  # convert to bounding box
  bb[[i]] <- apply(out_list[[i]], 1, function(x){
    MODISTools::mt_bbox(xllcorner = x['xllcorner'],
                        yllcorner = x['yllcorner'],
                        cellsize = x['cellsize'],
                        nrows = x['nrows'],
                        ncols = x['ncols'])
  })



}


# Band these together
modis_pixels <- tibble(sites,select(bind_rows(bb),1)) |>
  rename(pixel_boundary = 4)



# Save the data
save(file = "data-raw/firemap-data/modis_pixels.Rda",modis_pixels)



# read in fire shapefile data
# Downloaded from: https://cwfis.cfs.nrcan.gc.ca/datamart - select National Fire Database fire polygon data  (too large to store shapefiles on github)




fire_data <-  st_read("data-raw/Fire-Shapefiles/NFDB_poly/NFDB_poly_20210707.shp")

fire_2012 <- fire_data |>
  filter(YEAR == "2012",
         SRC_AGENCY %in% c("YT","NT")) |>
  st_zm() |>
  st_transform(4326) |>
  as_Spatial()

# Let's just get modis pixels in the area of the 2012 fire (by investigation)
fire_2012_only <- fire_data |>
  filter(YEAR == "2012",
         FIRE_ID=="2012DA013") |>
  st_zm() |>
  st_transform(4326) |>
  as_Spatial()

fire_1990 <- fire_data |>
  filter(YEAR == "1990",
         SRC_AGENCY %in% c("YT","NT")) |>
  st_zm() |>
  st_transform(4326) |>
  as_Spatial()

fire_1968 <- fire_data |>
  filter(YEAR == "1968",
         SRC_AGENCY %in% c("YT","NT")) |>
  st_zm() |>
  st_transform(4326) |>
  as_Spatial()


fire_1969 <- fire_data |>
  filter(YEAR == "1969",
         SRC_AGENCY %in% c("YT","NT")) |>
  st_zm() |>
  st_transform(4326) |>
  as_Spatial()


save(file = "data-raw/firemap-data/fire_2012.Rda",fire_2012)
save(file = "data-raw/firemap-data/fire_1990.Rda",fire_1990)
save(file = "data-raw/firemap-data/fire_1969.Rda",fire_1969)
save(file = "data-raw/firemap-data/fire_1968.Rda",fire_1968)
