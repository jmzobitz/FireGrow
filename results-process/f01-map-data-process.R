### Author: JMZ
### Last modified: 24/06/01
### Purpose: Code that allows you to process and save up the maps  - this runs and saves all the files

library(MODISTools)
library(leaflet)
library(sf)
library(leafem)
library(leaflegend)
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


# Make the map - yay!
leaflet() |>
  addTiles() |>
  setView(lng = -136, lat=67,zoom=6) |>
  addPolygons(data=modis_pixels$pixel_boundary,color = "yellow", weight = 5, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,group="MODIS pixels") |>
  addMarkers(data=modis_pixels,lng = ~long, lat=~lat, popup=~site_name) |>
  addPolygons(data=fire_2012,color = "#444444", weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.2,group="2012 Fires",popup=~paste("Cause: ",CAUSE, "</br>", "Date: ", REP_DATE, "</br>", "Size (hectacres): ", SIZE_HA, "</br>", "Fire ID: ",FIRE_ID)) |>
  addPolygons(data=fire_1990,color = "red", weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.2,group="1990 Fires",popup=~paste("Cause: ",CAUSE, "</br>", "Date: ", REP_DATE, "</br>", "Size (hectacres): ", SIZE_HA, "</br>", "Fire ID: ",FIRE_ID)) |>
  addPolygons(data=fire_1969,color = "blue", weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.2,group="1969 Fires",popup=~paste("Cause: ",CAUSE, "</br>", "Date: ", REP_DATE, "</br>", "Size (hectacres): ", SIZE_HA, "</br>", "Fire ID: ",FIRE_ID)) |>
  addPolygons(data=fire_1968,color = "orange", weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.2,group="1968 Fires",popup=~paste("Cause: ",CAUSE, "</br>", "Date: ", REP_DATE, "</br>", "Size (hectacres): ", SIZE_HA, "</br>", "Fire ID: ",FIRE_ID)) |>
  addLayersControl(
    overlayGroups = c("1968 Fires", "1969 Fires", "1990 Fires", "2012 Fires","MODIS pixels"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addMouseCoordinates()  # Just so we can see where we are.


# Now let's do this via tmap
# Get the sites loaded up in a way so they are easy to label.
my_sites <- modis_pixels |>
  mutate(plot_pixel = if_else(site_name %in% c("N2012-A","N2012-B","N2012-C"),FALSE,TRUE)) |>  # Remove MODIS pixels we didn't actually measure
  mutate(plot_point = if_else(site_name %in% c("N2012-FBA","N2012-FBB","N2012-FBC"),FALSE,TRUE)) |>  # Remove sites we didn't actually measure
  separate(site_name,into=c("site_name",NA),sep="-") |>
  mutate(site_name = factor(site_name,levels=c("N2012","N1990","N1969","NC"),  # Note the change N1969 should be 1968
                            labels=c("2012","1990","1968","Control")) )

#my_colors <- c('#e41a1c','#377eb8','#4daf4a','#984ea3')
my_colors <- c('#785EF0','#DC267F','#FE6100','#FFB000')


# Define the pallette
pal <- colorFactor(my_colors, my_sites$site_name,
                   na.color = "transparent")


# Define the symbols:
shapes <- c('rect', 'circle', 'triangle', 'diamond')
symbols <- setNames(Map(f = makeSymbol
                        ,shape = shapes
                        ,fillColor = my_colors
                        ,color = 'black'
                        ,opacity = 1
                        ,fillOpacity = .5
                        ,height = 16
                        ,width = 16
                        ,'stroke-width' = 2), shapes)

join_data <- tibble(shapes,site_name=c("2012","1990","1968","Control"))
my_sites_map <- inner_join(my_sites,join_data,by=c('site_name')) |>
  rename(symbol=shapes)

# Define some mapping functions to help us:
#setView(lng = -136, lat=67,zoom=4) |>
make_base_map <- function(map_title,long_view=-136,lat_view=67,save_file=NULL) {

  tag.map.title <- htmltools::tags$style(htmltools::HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 5%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-size: 28px;
  }
"))

  title <- htmltools::tags$div(
    tag.map.title, htmltools::HTML(map_title)
  )


  out_map <- leaflet(options = leafletOptions(
    attributionControl=TRUE)) |>
    addTiles(attribution = '© OpenStreetMap contributors 2024. Distributed under the Open Data Commons Open Database License (ODbL) v1.0.') |>
    setView(lng =long_view, lat=lat_view,zoom=4) |>
    addRectangles(
      lng1=-134.3, lat1=68.2,
      lng2=-133.1, lat2=67.8,
      color = 'blue',
      fillColor = "transparent"
    ) |>
    addRectangles(
      lng1=-137.7, lat1=66.4,
      lng2=-136.5, lat2=65.85,
      color='red',
      fillColor = "transparent"
    ) |>
    addScaleBar() |>
    addControl(title, position = "topleft", className="map-title")

  if(!is.null(save_file)) {

    mapview::mapshot(out_map, file = save_file,
                     remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))


  } else {
    return(out_map)
  }



}

make_site_map <- function(map_title,long_view,lat_view,save_file=NULL) {

  tag.map.title <- htmltools::tags$style(htmltools::HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 5%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-size: 28px;
  }
"))

  title <- htmltools::tags$div(
    tag.map.title, htmltools::HTML(map_title)
  )


  out_map <- leaflet(options = leafletOptions(
    attributionControl=TRUE)) |>
    addTiles(attribution = '© OpenStreetMap contributors 2024. Distributed under the Open Data Commons Open Database License (ODbL) v1.0.') |>
    setView(lng =long_view, lat=lat_view,zoom=9) |>
    addMarkers(
      data = filter(my_sites_map,plot_point),
      lng = ~ long,
      lat = ~ lat,
      icon = ~ icons(
        iconUrl = symbols[symbol],
        iconWidth = 20,
        iconHeight = 20
      ) ) |>
    addPolygons(data=fire_1968,color = my_colors[3], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="1968 Fires.") |>
    addPolygons(data=fire_1990,color = my_colors[2], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="1990 Fires") |>
    addPolygons(data=fire_2012,color = my_colors[1], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="2012 Fires") |>
    addRectangles(
      lng1=-134.3, lat1=68.2,
      lng2=-133.1, lat2=67.8,
      color = 'blue',
      fillColor = "transparent"
    ) |>
    addRectangles(
      lng1=-137.7, lat1=66.4,
      lng2=-136.5, lat2=65.85,
      color='red',
      fillColor = "transparent"
    ) |>
    addScaleBar(options = scaleBarOptions(maxWidth=200) ) |>
    addLegendImage(
      images = symbols,
      labels = c("2012","1990","1968","Control"),
      # width = 50,
      #  height = 50,
      orientation = 'vertical',
      title = htmltools::tags$div('Chronosequence Site',
                                  style = 'font-size: 24px; text-align: center;'),
      position = 'topright'
    ) |>
    addControl(title, position = "topleft", className="map-title")

  if(!is.null(save_file)) {

    mapview::mapshot(out_map, file = save_file,
                     remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))


  } else {
    return(out_map)
  }



}

make_inset_map <- function(map_title,long_view,lat_view,save_file=NULL) {

  tag.map.title <- htmltools::tags$style(htmltools::HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 5%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-size: 28px;
  }
"))

  title <- htmltools::tags$div(
    tag.map.title, htmltools::HTML(map_title)
  )


  out_map <- leaflet(options = leafletOptions(
    attributionControl=TRUE)) |>
    addTiles(attribution = '© OpenStreetMap contributors 2024. Distributed under the Open Data Commons Open Database License (ODbL) v1.0.') |>
    setView(lng = long_view, lat=lat_view,zoom=12) |>
    addMarkers(
      data = filter(my_sites_map,plot_point),
      lng = ~ long,
      lat = ~ lat,
      icon = ~ icons(
        iconUrl = symbols[symbol],
        iconWidth = 15,
        iconHeight = 15
      ) ) |>
    addPolygons(data=pull(filter(my_sites_map,plot_pixel),pixel_boundary),color = "black", weight = 2, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,group="MODIS pixels") |>
    addPolygons(data=fire_1968,color = my_colors[3], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="1968 Fires.") |>
    addPolygons(data=fire_1990,color = my_colors[2], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="1990 Fires") |>
    addPolygons(data=fire_2012,color = my_colors[1], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="2012 Fires") |>
    addRectangles(
      lng1=-134.3, lat1=68.2,
      lng2=-133.1, lat2=67.8,
      color = 'blue',
      fillColor = "transparent"
    ) |>
    addScaleBar(options = scaleBarOptions(maxWidth=200) ) |>
    addLegendImage(
      images = symbols,
      labels = c("2012","1990","1968","Control"),
      # width = 50,
      #  height = 50,
      orientation = 'vertical',
      title = htmltools::tags$div('Chronosequence Site',
                                  style = 'font-size: 24px; text-align: center;'),
      position = 'topright'
    ) |>
    addControl(title, position = "topleft", className="map-title")

  if(!is.null(save_file)) {

    mapview::mapshot(out_map, file = save_file,
                     remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))


  } else {
    return(out_map)
  }



}


make_base_map("a)",save_file = 'manuscript-figures/01-a-base-map.png')


make_site_map('b)',-136.94,66.15,save_file = 'manuscript-figures/01-b-north-sites.png')
make_site_map('c)',-133.59,68.025,save_file = 'manuscript-figures/01-c-south-sites.png')

make_inset_map("d)",-137.42,65.89,save_file = 'manuscript-figures/01-d-inset.png')
make_inset_map("e)",-133.47,68,save_file = 'manuscript-figures/01-e-inset.png')


make_inset_map("f)",-136.71,66.3,save_file = 'manuscript-figures/01-f-inset.png')
make_inset_map("g)",-137.27,66.0,save_file = 'manuscript-figures/01-g-inset.png')


