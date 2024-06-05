### Author: JMZ
### Last modified: 24/06/01
### Purpose: bring together the swc, tvdi, soil temperature, permafrost depth, litter into a common dataset and also plot them for the paper


# Join the different timeseries together, grouping by the depth and the site ID. # We will run the models at each of the different depths.

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
  filter(Date >= as.Date('2015-09-01')) |>
  group_by(ID,depth) |>
  nest() |>
  filter(ID %in% c("N1969-A","N1969-B","N1969-C" ,"N1990-A","N1990-B","N1990-C","N2012-FBA","N2012-FBB","N2012-FBC","NC-A","NC-B","NC-C")) |>  # Remove the non fire pixels from 2012
  separate(col="ID",into=c("site",NA),sep="-",remove=FALSE)  # Just do some cleanup here


### Make a list column of all the plots
modeling_data_plot <- gpp_timeseries |>
inner_join(swc_timeseries_adj,by=c("ID","site","sample","Date")) |>
inner_join(soil_temp_timeseries_adj,by=c("site"="ID","Date"="date")) |>
rename(soilT = T_soil,depth=layer) |>
  pivot_wider(names_from=depth,values_from=soilT) |>
mutate(gpp = gpp*1000) |>
filter(Date >= as.Date('2015-09-01')) |>
filter(ID %in% c("N1969-A","N1969-B","N1969-C" ,"N1990-A","N1990-B","N1990-C","N2012-FBA","N2012-FBB","N2012-FBC","NC-A","NC-B","NC-C")) |>  # Remove the non fire pixels from 2012
separate(col="ID",into=c("site",NA),sep="-",remove=FALSE) |> # Just do some cleanup here
  rename(T_soil_5 = `5`,T_soil_10=`10`) |>
#group_by(site,Date) |>
#mutate(across(c(gpp:T_soil_L30),~quantile(.x,na.rm=TRUE,probs = c(0.025,0.5,0.975)),q=c(1,2,3)))

  pivot_longer(cols=c("gpp":"T_soil_10")) |>
group_by(site,Date,name) |>
reframe(out_value = quantile(value,na.rm=TRUE,probs = c(0.025,0.5,0.975)),q=c(1,2,3))


#modeling_data$data[[2]] |> pivot_longer(cols=c("swc")) |> ggplot(aes(x=Date,y=value)) + geom_line() + facet_grid(name~.,scales="free_y")

# Now we are ready to plot according to our simulation
my_labeller <- as_labeller(c(T_soil_5="5~cm~T[S]~('°C')",
                             T_soil_10="10~cm~T[S]~('°C')",
                             gpp="GPP~(gC~m^-2~d^-1)",
                             swc="SWC~(no~units)",
                             N2012="2012",
                             N1990="1990",
                             N1969="1968",
                             NC="Control"),
                           default = label_parsed)

line_data <- tibble(name=c("swc","gpp","T_soil_5","T_soil_10"),yint=c(NA,NA,0,0)) |>
  mutate(name = factor(name,levels=c("swc","gpp","T_soil_5","T_soil_10")))

start_date <- modeling_data_plot$Date[[1]]
end_date <- tail(modeling_data_plot$Date,1)
date_br1 <- seq(from = as.Date("2015-01-01"), to =  as.Date("2022-01-01"), by = "1 year")
date_br5 <-seq(from = as.Date("2015-07-01"), to =  as.Date("2022-07-01"), by = "1 year")


# Make a representative plot of the data
p1 <- modeling_data_plot |> ungroup() |>
  pivot_wider(names_from="q",values_from=out_value,names_prefix="q") |>
  mutate(site = factor(site,levels=c("N2012","N1990","N1969","NC"))) |>
  mutate(name = factor(name,levels=c("swc","gpp","T_soil_5","T_soil_10"))) |>
  filter(Date >= as.Date("2015-12-31")) |>
  ggplot(aes(x=Date)) +
  geom_line(aes(y=q2,color=name)) +
  geom_ribbon(aes(ymin=q1,ymax=q3,fill=name),alpha=0.5) +
  facet_grid(name~site,scales="free_y",labeller = my_labeller) +
  geom_hline(data=line_data,aes(yintercept=yint),linetype='solid',color='grey80') + theme_fulbright() +
  labs(y="Value",x="Year") +
  scale_x_date(breaks=date_br1,date_labels = "%Y",minor_breaks = date_br5) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1),
        axis.title=element_text(size=24),
        panel.grid.major.x=element_line(colour="grey",linetype = 'dashed')) + guides(color="none",fill="none")

ggsave('manuscript-figures/model-forcings.png',plot=p1,width=15,height=10)
