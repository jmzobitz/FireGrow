### Author: JMZ
### Last modified: 24/06/01
### Purpose: Code that plots out the modeled soil temperatures at the different sites.


# Define the labels

my_labeller <- as_labeller(c(N1968="1968",
                             N1990="1990",
                             N2012="2012",
                             NC = "Control",
                             L5 = "5 cm",
                             L10 = "10"),
                           default = label_parsed)



soilT_time_small <- soil_temp_timeseries |>
  unnest(cols=c("T_soil")) |>
  mutate(layer = paste0("L",layer)) |>
 # pivot_longer(cols=c("T_soil_5","T_soil_10")) |>
  filter(lubridate::year(date) %in% c(2018,2019)) |>
  #rename(layer=name) |>
  mutate(ID = factor(ID,levels=c("N2012","N1990","N1968","NC"),
                     labels=c("2012","1990","1968","Control"))) |>
  mutate(layer = factor(layer,levels=c("L5","L10","L30"),
                        labels = c("5 cm","10 cm","30 cm")))





p1 <- soil_temperature_iButton_data |>
  filter(layer != "L30") |>
  mutate(layer = factor(layer,levels=c("L5","L10","L30"),
                        labels = c("5 cm","10 cm","30 cm"))) |>
    mutate(ID = factor(ID,levels=c("N2012","N1990","N1968","NC"),
                         labels=c("2012","1990","1968","Control"))) |>
  ggplot(aes(x=Date,y=T_soil)) + geom_point(size=0.5,alpha=0.4) +
  geom_line(data = soilT_time_small,aes(x=date,y=T_soil),color='red',inherit.aes=FALSE) +
  facet_grid(layer~ID) + theme_fulbright() +
    labs(y="Soil Temperature (°C)") +
   scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%m") +
   geom_hline(yintercept = 0, linetype = 'dashed') +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
         axis.title=element_text(size=24))



ggsave('manuscript-figures/soil-temperature-canada.png',plot=p1,width=15)
