### Author: JMZ
### Last modified: 24/06/01
### Purpose: Code that allows you to process and save up the maps  - this runs and saves all the files

library(leaflet)
library(sf)
library(leafem)
library(leaflegend)
library(htmltools)

# load all the data
load(file = "data-raw/firemap-data/modis_pixels.Rda")



load(file = "data-raw/firemap-data/fire_2012.Rda")
load(file = "data-raw/firemap-data/fire_1990.Rda")
load(file = "data-raw/firemap-data/fire_1969.Rda")
load(file = "data-raw/firemap-data/fire_1968.Rda")


# Now let's do this via tmap
# Get the sites loaded up in a way so they are easy to label.
my_sites <- modis_pixels |>
  mutate(plot_pixel = if_else(site_name %in% c("N2012-A","N2012-B","N2012-C"),FALSE,TRUE)) |>  # Remove MODIS pixels we didn't actually measure
  mutate(plot_point = if_else(site_name %in% c("N2012-FBA","N2012-FBB","N2012-FBC"),FALSE,TRUE)) |>  # Remove sites we didn't actually measure
  separate(site_name,into=c("site_name",NA),sep="-") |>
  mutate(site_name = factor(site_name,levels=c("N2012","N1990","N1969","NC"),  # Note the change N1969 should be 1968
                            labels=c("2012","1990","1968","Control")) )

my_colors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c')


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
    addTiles(
      urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"#,
      #attribution = "© OpenStreetMap contributors © CartoDB"
    ) |>
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
    addTiles(
      urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
      attribution = "© OpenStreetMap contributors © CartoDB"
    ) |>
    #addTiles(attribution = '© OpenStreetMap contributors 2024. Distributed under the Open Data Commons Open Database License (ODbL) v1.0.') |>
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
    addPolygons(data=fire_1968,color = '#fe9929', weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="1968 Fires.") |>
    addPolygons(data=fire_1990,color = '#fe9929', weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="1990 Fires") |>
    addPolygons(data=fire_2012,color = '#fe9929', weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="2012 Fires") |>
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
      title = htmltools::tags$div('Chronosequence Site:',
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

make_site_map('a)',-136.94,66.15)
make_site_map('b)',-133.59,68.025)

make_base_map("",save_file = 'manuscript-figures/01-a-base-map.png')
make_site_map('b)',-136.94,66.15,save_file = 'manuscript-figures/01-b-south-sites.png')
make_site_map('a)',-133.59,68.025,save_file = 'manuscript-figures/01-a-north-sites.png')
# Combine the images into one map with an inset using cowplot
main_img_a <- cowplot::ggdraw() + cowplot::draw_image("manuscript-figures/01-a-north-sites.png")
main_img_b <- cowplot::ggdraw() + cowplot::draw_image("manuscript-figures/01-b-south-sites.png")
inset_img <- cowplot::ggdraw() + cowplot::draw_image('manuscript-figures/01-a-base-map.png')



# Add inset to the main image
combined_map_a <- cowplot::ggdraw() +
  cowplot::draw_plot(main_img_a) +
  cowplot::draw_plot(inset_img, x = 0.7, y = 0.05, width = 0.3, height = 0.3)

# Save the final combined map
ggsave('manuscript-figures/01-a-inset-north-sites.png', combined_map_a, width = 10, height = 8)

# Do this for the second map
# Add inset to the main image
combined_map_b <- cowplot::ggdraw() +
  cowplot::draw_plot(main_img_b) +
  cowplot::draw_plot(inset_img, x = 0.7, y = 0.05, width = 0.3, height = 0.3)

# Save the final combined map
ggsave('manuscript-figures/01-b-inset-south-sites.png', combined_map_b, width = 10, height = 8)






#make_inset_map("d)",-137.42,65.89,save_file = 'manuscript-figures/01-d-inset.png')
#make_inset_map("e)",-133.47,68,save_file = 'manuscript-figures/01-e-inset.png')


#make_inset_map("f)",-136.71,66.3,save_file = 'manuscript-figures/01-f-inset.png')
#make_inset_map("g)",-137.27,66.0,save_file = 'manuscript-figures/01-g-inset.png')


