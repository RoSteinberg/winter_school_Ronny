# Script name
# Author
# Date


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(palmerpenguins)
library(ggridges)


# Data --------------------------------------------------------------------

# Load the dataset into the local environment
penguins <- penguins


# Example -----------------------------------------------------------------

# The basic plot
ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = species))


# Exercise 1 --------------------------------------------------------------

# Create a basic plot with different x and y axes

ggplot(penguins, 
       aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(colour = species))
# Exercise 2 --------------------------------------------------------------

# Change the aes() arguments

ggplot(penguins, 
       aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(colour = species)) 

# Exercise 3 --------------------------------------------------------------

# Change the labels

ggplot(penguins, 
       aes(x = bill_length_mm, y = bill_depth_mm, shape = sex)) +
  geom_point(aes(colour = species)) +
  labs(x = 'Bill depth (mm)', y = 'Bill lengtn (mm)')
# Create a ridgeplot

ggplot(data = penguins,
       # NB: Arguments passed to first aes() apply to all geoms
       aes(x = bill_length_mm, y = bill_depth_mm, colour = species)) +
  geom_point(aes(shape = sex)) +
  geom_smooth(method = "lm") +
  labs(x = 'Bill depth (mm)', y = 'Bill lengtn (mm)')+
  theme_ridges(font_size = 14,
               font_family = "",
               line_size = 0.5,
               grid = TRUE,
               center_axis_labels = TRUE)


