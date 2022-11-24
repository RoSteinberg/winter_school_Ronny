# Helgoland Road Data Plots and Tabs
# Ronny Steinberg
# 23.11.2022

# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)

# Temperature Difference ------------------------------------------------------

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


# Secchi depth diffirence ------------------------------------------------------

Secc_diff <- 
  read.delim("course_material/data/Helroad.csv", header=TRUE, sep=",", dec=".") %>% 
  unite(Year, Month, Day, col = "t", sep = "-")%>%
  # Then create a month abbreviation column
  mutate(month = month(t, label = T)) %>%
  # Then create a year abbreviation column
  mutate(year = year(t)) %>%
  # Then group by years and months
  group_by(year,month) %>% 
  # Then calculate the mean for each year and month
  summarise(mean_secc = mean(Secchi, na.rm = TRUE), 
            # and the SD
            sd_temp = sd(Secchi, na.rm = TRUE),
            .groups = 'drop') %>%
  # Then group by month
  group_by(month) %>%
  # Then calculate the mean for every month over all years
  mutate(mean_a = mean(mean_secc, na.rm = TRUE)) %>%
  # Then calculate the monthly differnce for every year
  mutate(anom = mean_secc - mean(mean_secc, na.rm = T)) %>%
  ungroup() %>%
  # Then filter the Years you want
  filter(year %in% c(2003,2004,2014,2015)) %>%
  # Begin ggplot
  ggplot(aes(x = month, y = anom, group = as.factor(year))) + 
  # Create dots
  geom_point(aes(colour = as.factor(year))) + 
  # Create lines
  geom_line(aes(colour = as.factor(year), group = as.factor(year)), linewidth = 0.5) + 
  # Change labels
  labs(x = " ", y = "Δ Secchi depth (m)", colour = "Year") +
  scale_x_discrete(breaks = c('Jan', 'Feb', 'Mrz', 'Apr', 'Mai', 'Jun', 
                              'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez'),
                   labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                              'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  scale_color_brewer(palette = 'Set1')+
  theme_bw()

# Group both figures

road <- 
ggarrange(Temp_diff, Secc_diff,
          # Set number of rows and columns
          ncol = 2,
          # Label each figure
          labels = c("a)", "b)"),
          # Create common legend
          common.legend = TRUE,
          # Set legend position
          legend = "bottom")

# Save the figure ------------------------------------------------------------

ggsave(road, filename = "figures/Road_diff.png", width = 10, height = 7, dpi = 600)

# Create a Table with all Data -----------------------------------------------
#### Temperature Table -------------------------------------------------------

Temp_tab <-
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
  # Then calculate the monthly difference for every year
  mutate(anom_temp = mean_temp - mean(mean_temp, na.rm = T)) %>%
  ungroup() %>%
  # Then filter the Years you want
  filter(year %in% c(2003,2004,2014,2015))

#### Secchi depth Table -------------------------------------------------------

Secc_tab <- 
read.delim("course_material/data/Helroad.csv", header=TRUE, sep=",", dec=".") %>% 
  unite(Year, Month, Day, col = "t", sep = "-")%>%
  # Then create a month abbreviation column
  mutate(month = month(t, label = T)) %>%
  # Then create a year abbreviation column
  mutate(year = year(t)) %>%
  # Then group by years and months
  group_by(year,month) %>% 
  # Then calculate the mean for each year and month
  summarise(mean_secc = mean(Secchi, na.rm = TRUE), 
            # and the SD
            sd_secc = sd(Secchi, na.rm = TRUE),
            .groups = 'drop') %>%
  # Then group by month
  group_by(month) %>%
  # Then calculate the mean for every month over all years
  mutate(anual_mean_secc = mean(mean_secc, na.rm = TRUE)) %>%
  # Then calculate the monthly differnce for every year
  mutate(anom_secc = mean_secc - mean(mean_secc, na.rm = T)) %>%
  ungroup() %>%
  # Then filter the Years you want
  filter(year %in% c(2003,2004,2014,2015))

#### combine both tabs --------------------------------------------------------
Road_tab<- left_join(Temp_tab, Secc_tab, by = c("year", "month"))

