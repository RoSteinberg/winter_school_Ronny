# Script name
# Author
# Date
rm()

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(palmerpenguins)


# Data --------------------------------------------------------------------

# Load the dataset into the local environment
penguins <- penguins


# Example -----------------------------------------------------------------

# continuos viridis colour palette
ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm))+
  #scale_colour_viridis_c(option = "A")
  #scale_color_distiller(palette = 'Set1')
  scale_colour_gradientn(colours = c('#e87278','#5e8436','#8771de','#c0ae48','#c73762','#b25626','#c33a39'))

# Discrete viridis colour palette
ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(sex))) +
  #scale_colour_viridis_d(option = "D")
  #scale_colour_brewer(palette = 'Set1')
  scale_color_manual(values = c('#e87278', '#5e8436'),
                     labels = c('female', 'male', 'other'))+
  labels(colour = 'Sex')

ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(sex))) +
  # How to use custom palette
  scale_colour_manual(values = c('#e87278', '#5e8436'),
                      # How to change the legend text
                      labels = c("female", "male", "other")) + 
  # How to change the legend title
  labs(colour = "Sex") 

# Compare species
ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
  geom_boxplot(aes(fill = species), show.legend = F) +
  stat_compare_means(method = "anova")


# First create a list of comparisons to feed into our figure
penguins_levels <- levels(penguins$species)
my_comparisons <- list(c(penguins_levels[1], penguins_levels[2]), 
                       c(penguins_levels[2], penguins_levels[3]),
                       c(penguins_levels[1], penguins_levels[3]))

# Then we stack it all together
ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
  geom_boxplot(aes(fill  = species), colour = "grey40", show.legend = F) +
  stat_compare_means(method = "anova", colour = "grey50",
                     label.x = 1.8, label.y = 32) +
  # Add pairwise comparisons p-value
  stat_compare_means(comparisons = my_comparisons,
                     label.y = c(62, 64, 66)) +
  # Perform t-tests between each group and the overall mean
  stat_compare_means(label = "p.signif", 
                     method = "t.test",
                     ref.group = ".all.") + 
  # Add horizontal line at base mean
  geom_hline(yintercept = mean(penguins$bill_length_mm, na.rm = T), 
             linetype = 2) + 
  labs(y = "Bill length (mm)", x = NULL) +
  theme_bw()
# Exercise 1 --------------------------------------------------------------

# Create your own continuous and discrete colour palettes
# Create and combine two figures, each using a different palette

lm_1 <-
  ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(species)))+
  scale_colour_manual(values = c('#e87278', '#5e8436', '#8771de'),
                      # How to change the legend text
                      labels = c("Adelie", "Chinstrap", "Gentoo"))+
  labs(x  = 'Body mass (g)', y  = 'Bill length (mm)', colour = 'Species')+
  theme_bw()

lm_2 <-
  ggplot(data = penguins,
       aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(sex)))+
  scale_colour_manual(values = c('#c0ae48','#b25626'),
                      # How to change the legend text
                      labels = c("Female", "Male", "other"))+
  labs(x  = 'Bill length (mm)', y  = 'Bill length (mm)', colour = 'Sex')+
  theme_bw()

ggarrange(lm_1, lm_2)

# Exercise 2 --------------------------------------------------------------

# Create two versions of the same figure and combine
# Use a viridis colour palette against a default palette in a way that 
# allows features in the data to be more pronounced

lm_3 <- 
  ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(species)))+
  scale_colour_manual(values = c('#e87278', '#5e8436', '#8771de'),
                      # How to change the legend text
                      labels = c("Adelie", "Chinstrap", "Gentoo"))+
  labs(x  = 'Body mass (g)', y  = 'Bill length (mm)', colour = 'Species')+
  theme_bw()

lm_4 <- 
  ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(species)))+
  scale_colour_viridis_d(option = "D")+
  labs(x  = 'Body mass (g)', y  = 'Bill length (mm)', colour = 'Species')+
  theme_bw()

ggarrange(lm_3, lm_4)

# Exercise 3 --------------------------------------------------------------

# Plot and combine t-test and ANOVA stats using sst_NOAA
# See this site for more info on plotting stats:
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/


library(readr)
sst_NOAA <- read_csv("course_material/data/sst_NOAA.csv")
sst_NOAA$site <- as.factor(sst_NOAA$site)

NOAA_levels <- levels(sst_NOAA$site)
my_comparisons <- list(c(NOAA_levels[1], NOAA_levels[2]), 
                       c(NOAA_levels[2], NOAA_levels[3]),
                       c(NOAA_levels[1], NOAA_levels[3]))
ggplot(sst_NOAA,
       aes(x = site, y = temp)) +
  geom_boxplot(aes(fill = as.factor(site)), show.legend = F)+
  stat_compare_means(method = "anova", colour = "grey50",
                     label.x = 1.8, label.y = 32) +
  # Add pairwise comparisons p-value
  stat_compare_means(comparisons = my_comparisons,
                     label.y = c(62, 64, 66)) +
  # Perform t-tests between each group and the overall mean
  stat_compare_means(label = "p.signif", 
                     method = "t.test",
                     ref.group = ".all.") + 
  # Add horizontal line at base mean
  geom_hline(yintercept = mean(sst_NOAA$temp, na.rm = T), 
             linetype = 2) + 
  scale_colour_manual(values = c('#e87278', '#5e8436', '#8771de'),
                      labels = c("Med", "NW_Atl", "WA"))+
  labs(x  = 'Site', y  = 'Temperature (°C)', colour = 'Site')+
  theme_bw()


# BONUS -------------------------------------------------------------------

