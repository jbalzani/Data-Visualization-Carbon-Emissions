#some question wording may be from HarvardX staff
##climate change
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

#latest year where carbon emissions reported
temp_carbon %>% 
  .$year %>%
  max() #no good, reports latest year but no emissions are reported
library(tidyverse)
library(dslabs)
data(temp_carbon)
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max() #should work, returns 2014
library(tidyverse)
library(dslabs)
data(temp_carbon)
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(year) #returns an error, can't find year
library(tidyverse)
library(dslabs)
data(temp_carbon)
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max() #returns 2014, seems to work
library(tidyverse)
library(dslabs)
data(temp_carbon)
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max() #should work, returns 2014
library(tidyverse)
library(dslabs)
data(temp_carbon)
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(.$year) #should work, nope its NA, I guess you can't do dot operator
#in max

#difference in CO2 from first to last year
temp_carbon_filtered <- temp_carbon %>%
  filter(!is.na(carbon_emissions))
temp_carbon_filtered %>%
  select(year) %>%
  min()

which.min(temp_carbon_filtered$year) #136
which.max(temp_carbon_filtered$year) #135
#check
earliest_year <- temp_carbon_filtered$year [136] #1751
latest_year <- temp_carbon_filtered$year[135] #2014

temp_carbon_filtered$carbon_emissions[136]
temp_carbon_filtered$carbon_emissions[135]

9855/3

#difference in temperature from first to last year
temp_filtered <- temp_carbon %>%
  filter(!is.na(temp_anomaly))
which.min(temp_filtered$year) #1
which.max(temp_filtered$year) #139
#check
earliest_year2 <- temp_filtered$year [1] 
latest_year2 <- temp_filtered$year[139]
earliest_year2
latest_year2
temp_filtered$temp_anomaly[1]
temp_filtered$temp_anomaly[139]
temp_increase <- temp_filtered$temp_anomaly[139] - 
  temp_filtered$temp_anomaly[1]
temp_increase

#time series plot of temp anomaly
p <- temp_filtered %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_point()
p <- p + geom_hline(aes(yintercept= 0), col = "blue")
p2 <- p +ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = .05, label = "20th century mean"), col = "blue")
p2 + geom_line(aes(year, ocean_anomaly), col = "turquoise") +
  geom_line(aes(year, land_anomaly), col = "brown")

#greenhouse gases portion
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, c02 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

#carbon emissions time series
temp_carbon_filtered %>% ggplot(aes(year, carbon_emissions)) +
  geom_point() +
  ylab("metric tons carbon")

co2_time <- historic_co2 %>%
  filter(!is.na(co2)) %>%
  ggplot(aes(year, co2), color = source) +
  geom_line() +
  ylab("co2 concentration") +
  scale_x_continuous(limits = c(-3000, 2018))
co2_time