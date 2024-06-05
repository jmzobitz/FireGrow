### Author: JMZ
### Last modified: 24/06/01
### Purpose: Scripts that make plots of the soil temperature and diffusivity data


# Make a quick plot:
p1 <- soil_temperature_iButton_data |>
  mutate(ID = factor(ID,levels=c('N2012','N1990','N1968','NC'),
                       labels=c("2012","1990","1968","Control") ),
         layer = factor(layer,levels=c('L5','L10','L30'),
                     labels=c("5 cm","10 cm","30 cm") ) ) |>
  ggplot(aes(x=Date,y=T_soil,color=layer)) +
  facet_grid(.~ID) +
  geom_line() +
  theme_fulbright() +
  labs(y=bquote(~T[Soil]~'('^o*C~')'),color="Depth:") +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1)) +
  geom_hline(yintercept=0,linetype='dashed')

ggsave('manuscript-figures/soil-temp-depth.png',plot=p1,width=10,height=6)

# Look at the diffusivity
p2 <- diffusivity |>
  mutate(ID = factor(ID,levels=c('N2012','N1990','N1968','NC'),
                     labels=c("2012","1990","1968","Control") ),
         real = factor(real,levels=c(TRUE,FALSE),labels=c('Measured','Imputed'))) |>
  ggplot(aes(x=ID,y=diffusivity,color=real)) +
  geom_boxplot() + ylim(c(0,1E-7)) +
  theme_fulbright() +
  labs(y=bquote(~Diffusivity~'('~m^-2~s^-1~')'),x="Year",color='Type:') +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1),
        axis.title=element_text(size=18))

ggsave('manuscript-figures/diffusivity.png',plot=p2,width=6,height=4)


