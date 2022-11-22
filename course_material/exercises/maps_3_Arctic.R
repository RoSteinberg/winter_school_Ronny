# Script name
# Author
# Date


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggsn)
library(ggOceanMaps)

# devtools::install_github("MikkoVihtakari/ggOceanMapsData")
library(ggOceanMapsData)


# Data --------------------------------------------------------------------

# ???


# Example -----------------------------------------------------------------

# The basic map
ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_polar()


# Exercise 1 --------------------------------------------------------------

# Directly access the shape of a region near a pole and plot with polar projection

basemap(limits = c( -45, 90, 50, 60), bathymetry = F)


# Exercise 2 --------------------------------------------------------------

# Add a data layer to a polar projection plot
basemap(limits = c( -45, 90, 50, 60), bathymetry = T)

# Exercise 3 --------------------------------------------------------------

# Use ggoceanmaps to create a similar plot


# BONUS -------------------------------------------------------------------

# Create a workflow for creating a polar plot for any region


#####-----------------------------------------

# Load libraries
library(tidyverse)
library(ggOceanMaps)
library(ggOceanMapsData)
# devtools::install_github("MikkoVihtakari/ggOceanMapsData")

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
load("C:/Users/rosteinb/winter_school_Ronny/course_material/data/OISST_2022.RData")

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
  geom_tile(data = OISST_2022, aes(fill = temp)) +
  coord_map(projection = "ortho", orientation = c(90, 0, 0))

# Filter map data and plot it in one code chunk
map_global_fix %>% 
  filter(lon > 9, lon < 28, lat > 76, lat < 81) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Filtering the OISST_2022 data directly in geom_tile()
  geom_tile(data = filter(OISST_2022,
                          lon > 9, lon < 28, lat > 76, lat < 81), 
            aes(fill = temp)) +
  coord_map(projection = "ortho", orientation = c(90, 0, 0))

# Filter map data and plot it in one code chunk
map_global_fix %>% 
  filter(lon > 9, lon < 28, lat > 76, lat < 81) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Filtering the OISST_2022 data directly in geom_tile()
  geom_tile(data = filter(OISST_2022,
                          lon > 9, lon < 28, lat > 76, lat < 81), 
            aes(fill = temp)) +
  coord_cartesian(expand = F)

# Filter map data and plot it in one code chunk
map_global_fix %>% 
  filter(lon > 9, lon < 28, lat > 76, lat < 81) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Filtering the OISST_2022 data directly in geom_tile()
  geom_tile(data = filter(OISST_2022,
                          lon > 9, lon < 28, lat > 76, lat < 81), 
            aes(fill = temp)) +
  coord_cartesian(expand = F)

# South east Asia
basemap(limits = c(100, 160, -20, 30), bathymetry = TRUE)

# North Sea
basemap(limits = c(0, 10, 50, 60), bathymetry = TRUE)

#glaciers
basemap(limits = 60, glaciers = TRUE, bathymetry = TRUE)

