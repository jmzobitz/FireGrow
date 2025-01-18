### Author: JMZ
### Last modified: 25/01/04
### Purpose: bring together the swc, tvdi, soil temperature, permafrost depth, litter into a common dataset and also plot them for the paper




####  Make the map first
library(leaflet)
library(sf)
library(leafem)
library(leaflegend)
library(htmltools)

# load all the data
load(file = "data-raw/firemap-data/modis_pixels.Rda")



load(file = "data-raw/firemap-data/fire_2012.Rda")

# Now let's do this via tmap
# Get the sites loaded up in a way so they are easy to label.
my_sites <- modis_pixels |>
  filter(str_detect(site_name,"2012")) |>
  mutate(within_fire = str_detect(site_name,pattern="FB")) |>
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
shapes <- c('rect')
symbols <- setNames(Map(f = makeSymbol
                        ,shape = shapes
                        ,fillColor = my_colors[[1]]
                        ,color = 'black'
                        ,opacity = 1
                        ,fillOpacity = .5
                        ,height = 16
                        ,width = 16
                        ,'stroke-width' = 2), shapes)

join_data <- tibble(shapes,site_name=c("2012","1990","1968","Control"))
my_sites_map <- inner_join(my_sites,join_data,by=c('site_name')) |>
  rename(symbol=shapes)



  long_view <- -137.42
  lat_view <- 65.89

  out_map <- leaflet(options = leafletOptions(
    attributionControl=TRUE)) |>
    addTiles(
      urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"#,
      #attribution = "© OpenStreetMap contributors © CartoDB"
    ) |>
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
    addPolygons(data=pull(filter(my_sites_map,plot_pixel),pixel_boundary),color = "black", weight = 2, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,group="MODIS pixels used") |>
    addPolygons(data=pull(filter(my_sites_map,!plot_pixel),pixel_boundary),color = "red", weight = 2, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,group="MODIS pixels",options = pathOptions(dashArray = "5, 10")) |>
    addPolygons(data=fire_2012,color = '#fe9929', weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="2012 Fires") |>
    addScaleBar(options = scaleBarOptions(maxWidth=200) ) |>
    addLegendImage(
      images = symbols,
      labels = c("2012"),
      # width = 50,
      #  height = 50,
      orientation = 'vertical',
      title = htmltools::HTML('<div style="font-size: 24px; text-align: center;">Chronosequence Site:</div>'),
      position = 'topright'
    ) |>
    addLegend(
      position = "bottomright",
      colors = c("red","black"),
      opacity = 1.0,
      labels = c("Red Dashed: MODIS pixel encompassing field site","Black Solid: MODIS pixels used in study"),
      title = htmltools::HTML('<div style="font-size: 24px; text-align: left;">MODIS Pixels:</div>')
    )
    #addControl(title, position = "topleft", className="map-title")

save_file <- 'manuscript-figures/map-2012-gpp.png'

    mapview::mapshot(out_map, file = save_file,
                     remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))



### Now make the GPP plot

library(grid)
library(gridExtra)

# Join the different timeseries together, grouping by the depth and the site ID. # We will run the models at each of the different depths.


my_labeller <- as_labeller(c(f_TRUE="Black~Solid:~MODIS~pixels~used~'in'~study",
                             f_FALSE="Red~Dashed:~MODIS~pixel~encompassing~field~site"),
                           default = label_parsed)



date_br1 <- seq(from = as.Date("2011-01-01"), to =  as.Date("2023-12-31"), by = "1 year")
date_br5 <- seq(from = as.Date("2011-01-01"), to =  as.Date("2023-12-31"), by = "3 months")





out_plot<-gpp_timeseries |>
  mutate(gpp = gpp*1000) |>
  filter(site == "N2012") |>
  mutate(within_fire = str_detect(ID,pattern="FB"),
         within_fire = paste0("f_",within_fire)) |>
  filter(between(Date,as.Date('2011-01-01'),as.Date('2013-12-31'))) |>
  group_by(within_fire,Date) |>
  reframe(out_value = quantile(gpp,na.rm=TRUE,probs = c(0.025,0.5,0.975)),q=c(1,2,3)) |>
  pivot_wider(names_from="q",values_from=out_value,names_prefix="q") |>
  ggplot(aes(x=Date)) +
  geom_line(aes(y=q2)) +
  geom_ribbon(aes(ymin=q1,ymax=q3),alpha=0.5) +
  facet_grid(.~within_fire,labeller = my_labeller) +
  labs(x="Day of year",y=bquote(~F[GPP]~'('~g~C~m^-2~d^-1*~')')) +
  theme_fulbright() +
  scale_x_date(breaks=date_br1,date_labels = "%Y",minor_breaks = date_br5) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1),
        axis.title=element_text(size=24),
        panel.grid.major.x=element_line(colour="grey",linetype = 'dashed'),
        panel.grid.minor.x=element_line(colour="grey",linetype = 'dashed')
  )



# Load Leaflet map image
leaflet_img <- cowplot::ggdraw() +
  cowplot::draw_image(save_file)

# Combine Leaflet map and ggplot side by side
combined_plot <- cowplot::plot_grid(leaflet_img, out_plot, ncol = 2, rel_widths = c(1, 1))

# Save combined plot
ggsave('manuscript-figures/modis-2012-fire-compare.png', combined_plot, width = 22, height = 6)



