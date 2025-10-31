library(FireGrow)
library(tidyverse)

compute_diff <- soil_temperature_iButton_data |>
  mutate(doy = yday(Date)) |>
  filter(doy %in% 121:244,
         layer %in% c("L5","L10")) |>
  group_by(layer) |>
  nest() |>
  mutate(diff = map(.x=data, .f=~(.x |> pivot_wider(names_from = ID,values_from = T_soil) |>
                                    pivot_longer(cols=N1990:N2012) |>
                                    mutate(diff = NC-value))),
    out_stats = map(.x=data,.f=~(.x |> pivot_wider(names_from = ID,values_from = T_soil) |>
                                        pivot_longer(cols=N1990:N2012) |>
                                        group_by(name) |>
                                        summarize(obs_mean_resid = mean(NC-value,na.rm=TRUE),
                                                  obs_sd_resid = sd(NC-value,na.rm=TRUE)))))

compute_diff |>
  select(-data,-out_stats) |>
  unnest(cols=c("diff")) |>
  mutate(layer = factor(layer,levels=c("L5","L10","L30"),
                        labels = c("5 cm","10 cm","30 cm"))) |>
  mutate(name = factor(name,levels=c("N2012","N1990","N1968","NC"),
                     labels=c("2012","1990","1968","Control"))) |>
  ggplot(aes(x=doy,y=diff,color=name,shape=name)) + geom_point(size=2) + facet_grid(layer~.,scales = "free_y") +
  labs(x = "Day of Year", y = "Difference from Control (\u00B0C)",
       shape = "Chronosequence site:",color = "Chronosequence site:") +
  theme_fulbright() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme(
    #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title=element_text(size=14)
    )

observed_diff <- compute_diff |>
  select(-data,-diff) |>
  unnest(cols=c("out_stats"))

## Now do the same for the difference of the modeled soil data

soilT_time_small <- soil_temp_timeseries |>
  unnest(cols=c("T_soil")) |>
  mutate(layer = paste0("L",layer)) |>
  mutate(doy = yday(date)) |>
  filter(doy %in% 121:244,
         layer %in% c("L5","L10")) |>
  # pivot_longer(cols=c("T_soil_5","T_soil_10")) |>
  #filter(lubridate::year(date) %in% c(2018,2019)) |>
  #rename(layer=name) |>
  mutate(ID = factor(ID,levels=c("N2012","N1990","N1968","NC"),
                     labels=c("2012","1990","1968","Control"))) |>
  filter(layer != "L30")


modeled_diff <-soilT_time_small |>
  group_by(layer) |>
  nest() |>
  mutate(diff = map(.x=data, .f=~(.x |> select(ID,date,T_soil) |>
                                    pivot_wider(names_from = ID,values_from = T_soil,names_prefix = "N") |>
                                    pivot_longer(cols=N1990:N1968) |>
                                    group_by(name) |>
                                    summarize(mod_mean_resid = mean(NControl-value,na.rm=TRUE),
                                              mod_sd_resid = sd(NControl-value,na.rm=TRUE)) ) ) ) |>
  select(-data) |>
  unnest(cols=c("diff"))

modeled_diff |>
  left_join(observed_diff,by=c("layer","name")) |>
  mutate(layer = factor(layer,levels=c("L5","L10"),
                        labels = c("5 cm","10 cm"))) |>
  ggplot(aes(x=obs_mean_resid,y=mod_mean_resid,shape=layer)) + geom_point(size=4,color='blue') +
  geom_abline() +
  labs(
    x = expression(mu[meas]),
    y = expression(mu[mod]),
    shape = "Depth:"
  ) +
  theme_fulbright()

modeled_diff |>
  left_join(observed_diff,by=c("layer","name")) |>
  mutate(layer = factor(layer,levels=c("L5","L10"),
                        labels = c("5 cm","10 cm"))) |>
  ggplot(aes(x=obs_sd_resid,y=mod_sd_resid,shape=layer)) + geom_point(size=4,color='red') +
  geom_abline() +
  ylim(c(0,2)) + xlim(c(0,2)) +
  labs(
    x = expression(sigma[meas]),
    y = expression(sigma[mod]),
    shape = "Depth:") +
    theme_fulbright()

