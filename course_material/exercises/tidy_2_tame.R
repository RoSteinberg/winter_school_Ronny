# Script name
# Author
# Date


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Data --------------------------------------------------------------------

# SST data
sst_NOAA <- read_csv("course_material/data/sst_NOAA.csv")


# Example -----------------------------------------------------------------

# All five functions at once
sst_NOAA %>% 
  arrange(site, temp) %>% 
  select(t, temp) %>% 
  filter(temp >= 23) %>% 
  mutate(year = year(t)) %>% 
  summarise(mean_year = mean(year))


# Exercise 1 --------------------------------------------------------------

# Filter a dataset to have only data for WA from 2005-2010
# Plot as a line plot
# sst_NOAA_WA <- 
#   sst_NOAA %>%
#   filter(site == 'WA', year(t) >= 2005) %>%
#   filter(year(t) <= 2010)

sst_NOAA_WA <- 
  sst_NOAA %>%
  filter(site == 'WA', year(t) %in% seq(2005,2010))

head(sst_NOAA_WA)
tail(sst_NOAA_WA)

ggplot() +
  geom_line(data = sst_NOAA_WA, aes(x = t, y = temp)) +
  labs(x = NULL, y = "Temperature (°C)") +
  theme_bw()
# Exercise 2 --------------------------------------------------------------

# Create an informative table of the 10 highest monthly temperature in the Med
sst_NOAA %>%
  filter(site == 'Med') %>%
  arrange(site, desc(temp)) %>%
  head(10)

# Exercise 3 --------------------------------------------------------------

# Plot the the annual mean temperatures of WA as a bar plot

sst_NOAA_annual_WA <- 
  sst_NOAA %>%
  filter(site == 'WA') %>%
  group_by(year(t)) %>%
  summarise(mean_temp = mean(temp, na.rm = T))

# sst_NOAA_annual_WA$`year(t)` <- as.factor(sst_NOAA_annual_WA$`year(t)`)

ggplot() +
  geom_bar(data = sst_NOAA_annual_WA, aes(x = `year(t)`, y = mean_temp), stat = 'identity') +
  labs(x = NULL, y = "Temperature (°C)") +
  theme_bw()


# BONUS -------------------------------------------------------------------

# Find the mean temperature for 2002 in Med and 2005 in WA in the same code chunk


sst_NOAA %>%
  mutate(year = year(t)) %>%
  group_by(site,year) %>%
  filter(site == "Med" | site == "WA") %>%
  summarise(mean_temp = mean(temp, na.rm = T),
            count = n(),
            .groups = 'drop') %>%
  filter(year == "2002" | year == "2005")

#####--------------------------------------------------------------------
# Load the SACTN Day 1 data
read_csv("course_material/data/sst_NOAA.csv") %>%
  # Then create a month abbreviation column
  mutate(month = month(t, label = T)) %>% 
  # Then group by sites and months
  group_by(site, month) %>% 
  # Lastly calculate the mean
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            # and the SD
            sd_temp = sd(temp, na.rm = TRUE)) %>% 
  # Begin ggplot
  ggplot(aes(x = month, y = mean_temp, group = site)) + 
  # Create a ribbon
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), 
              fill = "black", alpha = 0.2) + 
  # Create dots
  geom_point(aes(colour = site)) + 
  # Create lines
  geom_line(aes(colour = site, group = site)) + 
  # Change labels
  labs(x = "Month", y = "Temperature (°C)", colour = "Site") +
  scale_color_viridis_d(option='A')+
  theme_bw()


########## ___________-

data(package = .packages(all.available = TRUE))
