# Read in the Ibutton data from Kajar

library(tidyverse)
library(lubridate)
library(readxl)

# Load in each dataset:
N1990 <- read_xlsx('data-raw/kajar/Fire N1990 daily averages T.xlsx',sheet="Concluding") %>% select(1:4) %>%
  rename(Date=1,L5=2,L10=3,L30=4) %>%
  mutate(source="iButton",ID="N1990",Date=as.Date(Date))

N2012 <- read_xlsx('data-raw/kajar/Fire N2012 daily average T.xlsx',sheet="Concluding") %>% select(1:4) %>%
  rename(Date=1,L5=2,L10=3,L30=4) %>%
  mutate(source="iButton",ID="N2012",Date=as.Date(Date))

NC <- read_xlsx('data-raw/kajar/NControl daily average T.xlsx',sheet="Concluding") %>% select(1:4) %>%
  rename(Date=1,L5=2,L10=3,L30=4) %>%
  mutate(source="iButton",ID="NC",Date=as.Date(Date))

# Combine all the data together:

soil_temperature_iButton_data <- rbind(NC,N1990,N2012) %>% pivot_longer(cols=c(2:4),names_to="layer",values_to="T_soil") %>%
  relocate(ID,Date,layer,T_soil,source)

# Make a quick plot:
soil_temperature_iButton_data %>% ggplot(aes(x=Date,y=T_soil,color=layer)) + facet_grid(.~ID) + geom_line()

# Save the data
usethis::use_data(soil_temperature_iButton_data)
