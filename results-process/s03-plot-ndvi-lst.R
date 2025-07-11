# Show the rectangle of MODIS ndvi data

ndvi_lst <- canada_modis_data |>
  select(ID,site,sample,Date,ndvi) |>
  inner_join(lst_data,by=c("ID","site","sample","Date"))
### Yay!  We just need to solve this out and form the K matrix ....

# Filtering on when things are freezing
ndvi_lst |>
  mutate(freezing = lst <= 273.15) |>  # Separate when things are freezing
  ggplot(aes(x=ndvi,y=lst)) +
  geom_point(alpha=0.2) +
  facet_wrap(.~site) +
  theme_fulbright()


# Filtering on when things are freezing
ndvi_lst |>
  mutate(freezing = lst <= 273.15,
         doy = lubridate::yday(Date)) |>  # Separate when things are freezing
  ggplot(aes(x=doy,y=lst-273.15,color=freezing)) +
  geom_point() +
  facet_wrap(.~site) +
  theme_fulbright()


# Plot with NDVI excluded
ndvi_lst |>
  mutate(freezing = lst <= 273.15) |>
  filter(!freezing) |>
  ggplot(aes(x=ndvi,y=lst)) + geom_point() +
  facet_wrap(.~site) +
  theme_fulbright()


### OK: define a function for each site that computes the min and max lst for each VI band

cut_vals <- seq(0,1,by=0.02)
cut_centers <- seq(0.01,1,by=0.02)

# Bin the NDVI by min and max temperature - see Feng et al Pedosphere
ndvi_binned <- ndvi_lst |>
  group_by(site,cut(ndvi, breaks=cut_vals,labels=cut_centers)) |>
  summarise(min_lst = min(lst),
            max_lst = max(lst)) |>
  na.omit() |>
  pivot_longer(cols=c("min_lst","max_lst")) |>
  rename(ndvi=2) |>
  mutate(ndvi=as.numeric(levels(ndvi))[ndvi])




# Plot the regression

ndvi_lst_small <- ndvi_lst |>
  mutate(site = factor(site,levels=c("N2012-F","N1990","N1969","NC"),
                       labels=c("2012","1990","1968","Control")))

p1 <- ndvi_binned |>
  filter(site !="N2012") |>
  mutate(site = factor(site,levels=c("N2012-F","N1990","N1969","NC"),
                      labels=c("2012","1990","1968","Control"))) |>
  ggplot(aes(x=ndvi,y=value-273.15,color=name)) +
  #geom_point() +
  geom_point(data=filter(ndvi_lst_small,site!="N2012"),mapping=aes(x=ndvi,y=lst-273.15),inherit.aes=FALSE,alpha=0.4,size=0.5) +
  facet_grid(.~site) +
  geom_smooth(method="lm",se=FALSE) +
  xlim(c(0,1)) +
  theme_fulbright() +
  labs(x="NDVI",
       y="LST (°C)") +
  guides(color="none")





ggsave('manuscript-figures/ndvi-lst-regressions.png',plot=p1,width=15)

# WOW!  This is working.  OK, let's compute the values

ndvi_results_stats <- ndvi_binned |>
  group_by(site,name) |>
  nest() |>
  mutate(model_fit = map(data,~lm(value~ndvi,data=.x)),
         coeff = map(model_fit,broom::tidy),
         stats = map(model_fit,broom::glance))

# Next step: get the kernels organized for GSVD - yikes!
ndvi_results |> unnest(cols=c(stats)) |> View()
