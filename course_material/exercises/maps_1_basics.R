# Script name
# Author
# Date


# Libraries ---------------------------------------------------------------

# Which libraries should be loaded?


# Data --------------------------------------------------------------------

# Call the global data to the environment
map_data_world <- map_data("world")


# Example -----------------------------------------------------------------

# The basic map
map_data_world %>% 
  filter(region == "Germany") %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group))


# Exercise 1 --------------------------------------------------------------

# Create maps of four regions and combine
# Use a mix of cropping and direct access 


# Exercise 2 --------------------------------------------------------------

# Create a map that benefits from a scale bar and add a North arrow
# Hint: use annotate("segment", ...) to accomplish this


# Exercise 3 --------------------------------------------------------------

# Create a meaningful inset map


# BONUS -------------------------------------------------------------------

# Plot maps using Google Maps

