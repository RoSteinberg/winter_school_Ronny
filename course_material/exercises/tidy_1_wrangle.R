# Tidy Excersice 1
# Ronny Steinberg
# 23.11.22


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(palmerpenguins)
library(lubridate)


# Data --------------------------------------------------------------------

# The mangled data
load("course_material/data/OISST_mangled.RData")

# The tidy data
sst_NOAA <- read_csv("course_material/data/sst_NOAA.csv")

head(OISST1)
# Example -----------------------------------------------------------------

head(OISST3)

OISST3_tidy <- OISST3 %>% 
  pivot_wider(id_cols = idx, names_from = type, values_from = name)
head(OISST3_tidy)

# Exercise 1 --------------------------------------------------------------

# Combine OISST4a and OISST4b into a new object
head(OISST4a)

OISST4a_tidy <- OISST4a %>% 
  separate(col = index, into = c("site", "t"), sep = " ")
head(OISST4a_tidy)
OISST4a_tidy2 <- OISST4a_tidy %>%
  separate(col = t, into = c('year', 'month', 'day'), sep = '-')

head(OISST4a_tidy2)
head(OISST4b)

OISST4b_tidy <- OISST4b %>% 
  unite(year, month, day, col = "t", sep = "-")
head(OISST4b_tidy)

#OISST4_tidy <- left_join(OISST4a_tidy, OISST4b_tidy)
#head(OISST4_tidy)

OISST4_tidy <- left_join(OISST4a_tidy, OISST4b_tidy, by = c("site", "t"))
head(OISST4_tidy)
# Exercise 2 --------------------------------------------------------------

# Ensure that the date formatting is correct on your new object

OISST4_tidy$t <- as.Date(OISST4_tidy$t)

# Exercise 3 --------------------------------------------------------------

# Split the date column on `sst_NOAA` and re-unite them
head(sst_NOAA)

sst_NOAA_1 <- sst_NOAA %>%
  separate(col = t, into = c('year', 'month', 'day'), sep = '-')
head(sst_NOAA_1)

sst_NOAA_2 <- sst_NOAA_1 %>%
  unite(year, month, day, col = 't', sep = '-')
head(sst_NOAA_2)

# BONUS -------------------------------------------------------------------

# Plot the temperatures of two time series against each other as a scatterplot

head(sst_NOAA)
head(OISST1)

# Meaning temperature from time series 1 are the X axis, and time series 2 on the Y axis
# Hint: This requires filtering out one time series
# Then pivoting the temperatures wide into columns


ggplot() +
  geom_line(data = sst_NOAA, aes(x = t, y = temp, colour = site)) +
  geom_line(data = OISST1, aes(x = t, y = temp))+
  labs(x = NULL, y = "Temperature (°C)", colour = "Site") +
  theme_bw()


Temp_plot<- left_join(OISST1, sst_NOAA, by = c("site", "t"))
head(Temp_plot)
Temp_plot$site <- as.factor(Temp_plot$site)

ggplot() +
  geom_line(data = Temp_plot, aes(x = temp.x, y = temp.y))+
  labs(x = "Temperature NOAA (°C)", y = "Temperature OISST (°C)") +
  facet_wrap(~site) +
  theme_bw()


