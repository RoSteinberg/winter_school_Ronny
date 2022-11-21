# Script name
# Author
# Date


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(palmerpenguins)


# Data --------------------------------------------------------------------

# Load the dataset into the local environment
penguins <- penguins


# Example -----------------------------------------------------------------

# Basic faceted plot
lm_1 <- ggplot(data = penguins,
               aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~species)
lm_1

lm_2 <- ggplot(data = penguins,
               aes(x = bill_depth_mm, y = bill_length_mm, colour = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~species)
lm_2

# Basic combined plot
ggarrange(lm_1, lm_2)


# Exercise 1 --------------------------------------------------------------

# Create a new plot type and facet by gender
lm_3 <- ggplot(data = penguins,
               aes(x = bill_depth_mm, y = bill_length_mm, colour = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~species)
lm_3

# Exercise 2 --------------------------------------------------------------

# Create a new plot type and facet by two categories
lm_4 <- ggplot(data = penguins,
               aes(x = bill_depth_mm, y = bill_length_mm, colour = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(species~island)
lm_4

# Exercise 3 --------------------------------------------------------------

# Combine all of the plots you've created so far
# Save them as a high-res file larger than 2 MB

grid_1 <- ggarrange(lm_1, lm_2,lm_3, lm_4,
                    # Set number of rows and columns
                    ncol = 2, nrow = 2,
                    # Label each figure
                    labels = c("a)", "b)", "c)", "d)"),
                    # Create common legend
                    common.legend = TRUE,
                    # Set legend position
                    legend = "bottom")
grid_1

# Different file types
ggsave(plot = grid_1, filename = "figures/grid_1.pdf")
ggsave(plot = grid_1, filename = "figures/grid_1.png")
ggsave(plot = grid_1, filename = "figures/grid_1.eps")

# Change dimensions
ggsave(plot = grid_1, filename = "figures/grid_1.png", 
       width = 10, height = 8)

# Change DPI
ggsave(plot = grid_1, filename = "figures/grid_1.png", dpi = 600)

# BONUS -------------------------------------------------------------------

# Use a different package to combine plots




