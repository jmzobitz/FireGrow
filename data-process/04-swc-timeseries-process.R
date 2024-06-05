### Author: JMZ
### Last modified: 24/06/01
### Purpose: Use the data of ndvi and lst to do regressions of min an max values by bins


ndvi <-canada_modis_data |>
  select(-(band1:band2))

ndvi2 <- ndvi |> group_by(ID,Date,site,sample) |> nest() |> rename(ndvi_data = data)
lst2 <- lst_data |>  group_by(ID,Date,site,sample) |> nest() |> rename(lst_data = data)

joined_data <- lst2 |> inner_join(ndvi2,by=c("ID","site","sample","Date")) |>
  unnest(cols=c("ndvi_data")) |>
  unnest(cols=c("lst_data")) |>
  filter(lubridate::year(Date) > 2008) |>
  mutate(freezing = lst <= 273.15)

### NOTE: in results shown on 15-June to group we didn't filter for freezing!


# Determine how we are going to bin the data
cut_vals <- seq(0,1,by=0.02)
cut_centers <- seq(0.01,1,by=0.02)

# Bin the NDVI by min and max temperature - see Zhang et al Pedosphere
ndvi_binned <- joined_data |>
  #filter(!freezing) |>
  group_by(site,cut(ndvi, breaks=cut_vals,labels=cut_centers)) |>
  summarise(min_lst = min(lst),
            max_lst = max(lst)) |>
  na.omit() |>
  pivot_longer(cols=c("min_lst","max_lst")) |>
  rename(ndvi=2) |>
  mutate(ndvi=as.numeric(levels(ndvi))[ndvi])

# Plot the regression
ndvi_binned |>
  ggplot(aes(x = ndvi, y = value, color = name)) +
  geom_point() +
  facet_grid(. ~ site) +
  geom_smooth(method = "lm") +
  theme_fulbright()

ndvi_results <- ndvi_binned |>
  group_by(site,name) |>
  nest() |>
  mutate(
    model_fit = map(data, ~ lm(value ~ ndvi, data = .x)),
    coeff = map(model_fit, broom::tidy),
    stats = map(model_fit, broom::glance)
  )

ndvi_results |>
  unnest(cols=c(stats)) |>
  select(site,r.squared,p.value)

## Bring the results together now

# Fit the timeseries together
joined_timeseries <- lst_timeseries |>
  inner_join(ndvi_timeseries,by=c("ID","site","sample","Date"))

regression_results <- ndvi_results |>
  unnest(cols=c(coeff)) |>
  select(site,name,term,estimate) |>
  mutate(term=if_else(term=='(Intercept)','intercept','slope')) |>
  unite(name_term,name,term,sep='_') |>  # Join terms
  pivot_wider(names_from="name_term",values_from="estimate")

### Join together
swc_timeseries <- joined_timeseries |>
  inner_join(regression_results,by="site") |>
  mutate(fvi_min = min_lst_intercept+min_lst_slope*ndvi,
         fvi_max = max_lst_intercept+max_lst_slope*ndvi,
         tvdi = (lst - fvi_min)/(fvi_max - fvi_min),
         tvdi = if_else(tvdi<0,0,tvdi),
         tvdi = if_else(tvdi>1,1,tvdi),
         swc = 1-tvdi,
         site = if_else(str_detect(ID,"-F"),"N2012-F",site)) |>
  select(ID,site,sample,Date,swc)



usethis::use_data(swc_timeseries,overwrite=TRUE)
