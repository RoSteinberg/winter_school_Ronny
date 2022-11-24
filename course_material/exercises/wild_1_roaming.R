# Script name
# Author
# Date


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rgbif) # Access anything on GBIF
# See: https://data-blog.gbif.org/post/gbif-api-beginners-guide/

library(pangaear) # Access anything on PANGAEA
# See: https://docs.ropensci.org/pangaear/
library(lubridate)
# Data --------------------------------------------------------------------

# 


# Example -----------------------------------------------------------------

#


# Exercise 1 --------------------------------------------------------------

#


# Exercise 2 --------------------------------------------------------------

#


# Exercise 3 --------------------------------------------------------------

#


# BONUS -------------------------------------------------------------------

#

######---------------------------------------------------------------------

# The library containing these functions
library(oceanexplorer)

# Get data
winter_temp <- get_NOAA(var = "temperature", spat_res = 1, av_period = "winter")

# Plot
plot_NOAA(winter_temp)

# The library containing these functions
library(sdmpredictors)

# Explore datasets in the package
list_datasets()

# Explore layers in a dataset
BO_layers <- list_layers(datasets = "Bio-ORACLE")

# Average surface temperature
surface_temp <- load_layers("BO22_tempmean_ss")


# The library containing these functions
library(pangaear)

# Search for PAR data within Kongsfjorden
# NB: Bounding box is: minlon, minlat, maxlon, maxlat
search_res <- pg_search(query = "PAR", bbox = c(11, 78, 13, 80), count = 10)
head(search_res)

# Download
kong_PAR_dl <- pg_data(doi = search_res$doi[1])

# Prep
kong_PAR <- kong_PAR_dl[[1]]$data %>% 
  mutate(t = as.POSIXct(str_replace(`Date/Time`, "T", " ")))

# Plot
kong_PAR %>% 
  filter(t < "2012-07-31 00:00:00") %>% 
  ggplot(aes(x = t, y = `PAR [µmol/m**2/s]`)) +
  geom_line(aes(colour = as.factor(`Depth water [m]`))) +
  facet_wrap(~Comment, ncol = 1) + 
  labs(x = NULL, colour = "Depth [m]", 
       title = "PAR data from Kongsfjorden for July, 2012",
       subtitle = "Shallower values taken both above and below kelp canopy",
       caption = str_wrap(kong_PAR_dl[[1]]$citation, 100))

# Download Sylt hydrochemistry data from 1974
sylt_hydro <- pg_data(doi = '10.1594/PANGAEA.842951')

sylt_temp <- sylt_hydro[[1]]$data %>% 
  mutate(t = as.POSIXct(str_replace(`Date/Time`, "T", " ")))

# Download Sylt hydrochemistry data from 2016
sylt_hydro_2 <- pg_data(doi = '10.1594/PANGAEA.918023')

sylt_temp_2016<-
  sylt_hydro_2[[1]]$data %>% 
  mutate(t = as.POSIXct(str_replace(`Date/Time`, "T", " "))) %>%
  # Then create a month abbreviation column
  mutate(month = month(t, label = T)) %>%
  # Then create a year abbreviation column
  mutate(year = year(t)) %>%
  # Then group by years and months
  group_by(year,month) %>% 
  # Then calculate the mean for each year and month
  summarise(mean_temp = mean(`Temp [°C]`, na.rm = TRUE), 
            # and the SD
            sd_temp = sd(`Temp [°C]`, na.rm = TRUE),
            .groups = 'drop')
  # Begin ggplot
  ggplot(aes(x = month, y = mean_temp)) + 
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), 
              fill = "black", alpha = 0.2) + 
  # Create dots
  geom_point() + 
  # Create lines
  geom_line() + 
  # Change labels
  labs(x = " ", y = "Δ Temperature (K)") +
  scale_x_discrete(breaks = c('Jan', 'Feb', 'Mrz', 'Apr', 'Mai', 'Jun', 
                              'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez'),
                   labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                              'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  scale_color_brewer(palette = 'Set1')+
  theme_bw()


%>% 
  # Begin ggplot
  ggplot(aes(x = month, y = mean_temp, group = site)) + 
  # Create a ribbon
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), 
              fill = "black", alpha = 0.2) + 
  # Create dots
  geom_point(aes(colour = site)) + 
  # Create lines
  geom_line(aes(colour = site, group = site)) + 


ggplot() +
  geom_line(data = sylt_temp, aes(x = t, y = `Temp [°C]`))

ggplot() +
  geom_line(data = sylt_temp_2, aes(x = t, y = `Temp [°C]`))

Temp_diff <- 
  read.delim("course_material/data/Helroad.csv", header=TRUE, sep=",", dec=".") %>% 
  unite(Year, Month, Day, col = "t", sep = "-")%>%
  # Then create a month abbreviation column
  mutate(month = month(t, label = T)) %>%
  # Then create a year abbreviation column
  mutate(year = year(t)) %>%
  # Then group by years and months
  group_by(year,month) %>% 
  # Then calculate the mean for each year and month
  summarise(mean_temp = mean(Temperature, na.rm = TRUE), 
            # and the SD
            sd_temp = sd(Temperature, na.rm = TRUE),
            .groups = 'drop') %>%
  # Then group by month
  group_by(month) %>%
  # Then calculate the mean for every month over all years
  mutate(anual_mean_temp = mean(mean_temp, na.rm = TRUE)) %>%
  # Then calculate the monthly differnce for every year
  mutate(anom_temp = mean_temp - mean(mean_temp, na.rm = T)) %>%
  ungroup() %>%
  # Then filter the Years you want
  filter(year %in% c(2003,2004,2014,2015)) %>%
  # Begin ggplot
  ggplot(aes(x = month, y = anom_temp, group = as.factor(year))) + 
  # Create dots
  geom_point(aes(colour = as.factor(year))) + 
  # Create lines
  geom_line(aes(colour = as.factor(year), group = as.factor(year)), linewidth = 0.5) + 
  # Change labels
  labs(x = " ", y = "Δ Temperature (K)", colour = "Year") +
  scale_x_discrete(breaks = c('Jan', 'Feb', 'Mrz', 'Apr', 'Mai', 'Jun', 
                              'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez'),
                   labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                              'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  scale_color_brewer(palette = 'Set1')+
  theme_bw()