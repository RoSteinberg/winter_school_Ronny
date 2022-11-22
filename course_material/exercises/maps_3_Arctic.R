# Script name
# Author
# Date


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggsn)
library(ggOceanMaps)


# Data --------------------------------------------------------------------

# ???


# Example -----------------------------------------------------------------

# The basic map
ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_polar()


# Exercise 1 --------------------------------------------------------------

# Directly access the shape of a region near a pole and plot with polar projection


# Exercise 2 --------------------------------------------------------------

# Add a data layer to a polar projection plot


# Exercise 3 --------------------------------------------------------------

# Use ggoceanmaps to create a similar plot


# BONUS -------------------------------------------------------------------

# Create a workflow for creating a polar plot for any region


#####-----------------------------------------

# Load libraries
library(tidyverse)
library(ggOceanMaps)
library(ggOceanMapsData)

# NB: ggOceanMapsData is not on CRAN
# Uncomment and run this line of code:
# remotes::install_github("MikkoVihtakari/ggOceanMapsData")
# If this causes an error, think about why

# Fixed base map
map_global_fix <- map_data('world') %>% 
  rename(lon = long) %>% 
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon))

# Load SST from 2022-12-25 to 2022-12-31
load("C:/Users/rosteinb/winter_school_Ronny/course_material/data/OISST_2000.RData")

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Numeric sizing for lon/lat 
  coord_cartesian()

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Equal sizing for lon/lat 
  coord_equal()

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Ratio (Y divided by X) sizing for lon/lat 
  coord_fixed(ratio = 2)

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Behind the scenes this adapts the "mercator" projection
  coord_quickmap()

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  coord_sf() # sf = simple feature

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  scale_y_reverse() +
  # A very different projection
  coord_polar()

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  geom_tile(data = OISST_2000, aes(fill = temp)) +
  coord_map(projection = "ortho", orientation = c(90, 0, 0))

# Filter map data and plot it in one code chunk
map_global_fix %>% 
  filter(lon > 9, lon < 28, lat > 76, lat < 81) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Filtering the OISST_2022 data directly in geom_tile()
  geom_tile(data = filter(OISST_2000,
                          lon > 9, lon < 28, lat > 76, lat < 81), 
            aes(fill = temp)) +
  coord_map(projection = "ortho", orientation = c(90, 0, 0))

# Filter map data and plot it in one code chunk
map_global_fix %>% 
  filter(lon > 9, lon < 28, lat > 76, lat < 81) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Filtering the OISST_2022 data directly in geom_tile()
  geom_tile(data = filter(OISST_2000,
                          lon > 9, lon < 28, lat > 76, lat < 81), 
            aes(fill = temp)) +
  coord_cartesian(expand = F)

# Filter map data and plot it in one code chunk
map_global_fix %>% 
  filter(lon > 9, lon < 28, lat > 76, lat < 81) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Filtering the OISST_2022 data directly in geom_tile()
  geom_tile(data = filter(OISST_2000,
                          lon > 9, lon < 28, lat > 76, lat < 81), 
            aes(fill = temp)) +
  coord_cartesian(expand = F)

