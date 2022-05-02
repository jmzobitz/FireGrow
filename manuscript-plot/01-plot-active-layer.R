# Make some plots using the diffusivity results calculated

library(tidyverse)
library(FireGrow)
library(lubridate)
library(gganimate)
library(gifski)

# Sample plot:
my_labeller_depth <- as_labeller(c(depth="Active Layer", min_depth="Min thaw depth",max_depth="Max thaw depth"),
                                 default = label_value)



# Compute the ensemble average of each one, by site:

test_data <- active_layer_results %>%
  select(-data) %>% unnest(cols=c(depth)) %>%
  filter(converged==TRUE) %>%
  select(-Date) %>% group_by(ID,time) %>%
  summarise(depth = quantile(depth, c(0.25, 0.5, 0.75),na.rm=TRUE), quantile = c(0.25, 0.5, 0.75),
            min_depth = quantile(min_depth, c(0.25, 0.5, 0.75),na.rm=TRUE), quantile = c(0.25, 0.5, 0.75),
            max_depth = quantile(max_depth, c(0.25, 0.5, 0.75),na.rm=TRUE), quantile = c(0.25, 0.5, 0.75)) %>% ungroup() %>%
  pivot_wider(names_from = quantile,values_from=c("depth","min_depth","max_depth"),names_prefix = "q")


# Ok let's try plotting this out
test_data %>%
  ggplot(aes(x=(time-1)/365)) + geom_line(aes(y=max_depth_q0.5)) + facet_grid(.~ID) + geom_ribbon(aes(ymin=max_depth_q0.25,ymax=max_depth_q0.75),alpha=0.3) +
  ggtitle('Max Thaw Depth Ensemble Plot') +
  ylab('Depth (m)') +
  xlab('Time (fraction of year)') +
  scale_y_reverse(limits=c(2,0)) +
  theme_fulbright()


# Ok let's try plotting this out
test_data %>%
  ggplot(aes(x=(time-1)/365)) + geom_line(aes(y=depth_q0.5)) + facet_grid(.~ID) + geom_ribbon(aes(ymin=depth_q0.25,ymax=depth_q0.75),alpha=0.3) +
  ggtitle('Active layer Depth Ensemble Plot') +
  ylab('Depth (m)') +
  xlab('Time (fraction of year)') +
  scale_y_reverse(limits=c(2,0)) +
  theme_fulbright()




test_data <- active_layer_results %>%
  select(-data) %>% unnest(cols=c(depth)) %>%
  filter(converged==TRUE) %>%
  select(-Date) %>% group_by(ID,time) %>%
  summarise(depth = quantile(depth, c(0.025, 0.5, 0.975),na.rm=TRUE), quantile = c(0.025, 0.5, 0.975),
            min_depth = quantile(min_depth, c(0.025, 0.5, 0.975),na.rm=TRUE), quantile = c(0.025, 0.5, 0.975),
            max_depth = quantile(max_depth, c(0.025, 0.5, 0.975),na.rm=TRUE), quantile = c(0.025, 0.5, 0.975)) %>% ungroup() %>%
  pivot_wider(names_from = quantile,values_from=c("depth","min_depth","max_depth"),names_prefix = "q")






my_labeller <- as_labeller(c(N1969="N1969", N1990="N1990",N2012="N2012",NC="NC",L10="10 cm",L30="30 cm",L5="5 cm"),
                           default = label_value)


# Data frame for the active layer
measured_active_layer <- tibble(ID = c("N2012","N1990","NC"),
                                layer = c(1.01,0.88,0.29),
                                error = c(0.09,0.10,0.01)
)
# Ok let's try plotting this out
p1 <- test_data %>%
  inner_join(measured_active_layer,by=c("ID")) %>%
  mutate(ID = factor(ID,levels=c('N2012','N1990','NC'))) %>%
  mutate( year_frac =(time-1)/365 ) %>%
  ggplot(aes(x=year_frac)) +
  #geom_rect(ymin=-Inf,ymax=Inf,xmin = 213/365,xmax = 243/365,fill='blue',alpha=0.2) +
  geom_line(aes(y=depth_q0.5),size=1.5) +
  facet_grid(.~ID,labeller = my_labeller) +
  geom_ribbon(aes(ymin=depth_q0.025,ymax=depth_q0.975),alpha=0.3) +
  geom_rect(aes(ymin=layer-error,ymax=error+layer),xmin = 213/365,xmax = 243/365,alpha=0.2,fill='red') +
  ggtitle('Active Layer Depth') +
  ylab('Depth (m)') +
  xlab('Time (fraction of year)') +
  #scale_y_reverse(limits=c(2,0)) +
  theme_fulbright()



# August is days 213 - 243, so do the the fraction

# Basic plot outlining this:
p_orig <- test_data %>%
  inner_join(measured_active_layer,by=c("ID")) %>%
  mutate(ID = factor(ID,levels=c('N2012','N1990','NC'))) %>%
  mutate( year_frac =(time-1)/365 ) %>%
  ggplot(aes(x=year_frac)) +
  #geom_rect(ymin=-Inf,ymax=Inf,xmin = 213/365,xmax = 243/365,fill='blue',alpha=0.2) +
  #geom_line(aes(y=depth_q0.5)) +
  facet_grid(.~ID,labeller = my_labeller) +
  #geom_ribbon(aes(ymin=depth_q0.025,ymax=depth_q0.975),alpha=0.3) +
  geom_rect(aes(ymin=layer-error,ymax=error+layer),xmin = 213/365,xmax = 243/365,alpha=0.2,fill='red') +
  ggtitle('Active Layer Depth') +
  ylab('Depth (m)') +
  xlab('Time (fraction of year)') +
  scale_y_reverse(limits=c(2,0)) +
  theme_fulbright()

p_orig
ggsave(plot = p_orig,filename = 'manuscript-figures/active-layer.png',width=10,height=5)


anim <-p1 +
  transition_reveal(time)
anim

animate(anim, nframes = 24,
        width = 1200, height = 600,
        renderer = gifski_renderer("manuscript-figures/active-layer-animation.gif"))


# Look at the diffusivity
diffusivity %>%
  mutate(ID = factor(ID,levels=c('N2012','N1990','NC'))) %>%
  ggplot(aes(x=ID,y=diffusivity)) +
  geom_boxplot() +
  geom_jitter(alpha=0.4,aes(color=ID)) +
  theme_fulbright() +
  ylim(c(0,1e-7)) +
  labs(y = bquote('D ('*m^2~s^-1*')'),x='Site') +
  guides(color=FALSE)

# Plot the air temperature
p1 <- canada_weather_ensemble %>%
  pivot_wider(names_from = quantile,values_from=T_air,names_prefix = "q") %>%
  ggplot(aes(x=Date,y=q0.5)) + geom_line(color='red') +
  geom_ribbon(aes(ymin=q0.025, ymax=q0.975), alpha=0.2,fill='red') +
  theme_fulbright() +
  labs(y = 'Air Temperature',x='Year') +
  scale_x_date(date_breaks = "years" , date_labels = "'%y",
               limits = c(as.Date('2010-01-01'), NA))

p1


soil_temperature_iButton_data %>%
  mutate(layer = factor(layer,levels=c('L5','L10','L30'))) %>%
  mutate(ID = factor(ID,levels=c('N2012','N1990','NC'))) %>%
  ggplot(aes(x=Date,y=T_soil,color=ID)) + facet_grid(layer~ID,labeller = my_labeller) + geom_line() + theme_fulbright() +
  ylab("Soil Temp") +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  guides(color=FALSE)



### Can't seem to get this to work.  Bummer!

p_temp <- soil_temperature_iButton_data %>%
  filter(Date <= '2018-10-31') %>%
  #mutate(Date = as.factor(Date)) %>%
  mutate(depth = as.numeric(str_extract(layer,pattern="\\d+"))) %>%
  mutate(ID = factor(ID,levels=c('N2012','N1990','NC'))) %>%
  ggplot(aes(y=depth,x=T_soil,group=Date)) + facet_grid(.~ID,labeller = my_labeller) + geom_line() +geom_point(size=4,aes(color=layer),inherit.aes = TRUE) + theme_fulbright() +
  scale_y_reverse(limits=c(32,0)) +
  guides(color=FALSE) +
  labs(x = "Soil Temperature", y = "Depth (m)")

anim <-p_temp +
  transition_reveal(Date,keep_last = FALSE)

anim

transition_states(Date,wrap=FALSE,transitioning=FALSE)
#ease_aes('linear')
