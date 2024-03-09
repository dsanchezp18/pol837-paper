# R Script: Data Visualization
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script performs data visualization for the POL837 term research paper.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(lubridate)

# Load the full dataset

load("data/full_df.RData")

# Load weather data on its own

temperature_min <- read_csv("data/weather/min_temperature.csv")

temperature_max <- read_csv("data/weather/max_temperature.csv")

precipitation <- read_csv("data/weather/precipitation.csv")

cantons_df <- read_csv("data/other/ecuador_cantons.csv")

# Quick data preparation for the weather data --------------------------------

temperature_df <- 
  temperature_min %>% 
  rename(min_temperature = value) %>%
  left_join(temperature_max %>% select(date, canton_id, max_temperature = value), by = c("date", "canton_id")) %>% 
  mutate(avg_temp = (min_temperature + max_temperature) / 2) %>% 
  left_join(cantons_df, by = "canton_id")

precipitation_df <- 
  precipitation %>% 
  rename(precipitation = value) %>% 
  left_join(cantons_df, by = "canton_id")

# Survey responses --------------------------------------------------------

# Plotting survey responses through time

df %>% 
  group_by(interview_date, wave) %>% 
  summarise(count_of_responses = n()) %>%
  ggplot(aes(x = interview_date, y = count_of_responses, fill = as.factor(wave))) +
  geom_col() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") + 
  scale_color_discrete(palette = "Set4") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Same but facetting for the waves (free scales)

interview_dates_barchart <-
  df %>% 
  filter(!is.na(interview_date), 
         !(interview_date %>% between(as.Date("2008-07-10"), as.Date("2008-12-12")))) %>% 
  group_by(interview_date, wave) %>% 
  summarise(count_of_responses = n()) %>%
  ggplot(aes(x = interview_date, y = count_of_responses)) +
  geom_col(fill = "forestgreen") +
  geom_vline(xintercept = as.Date("2008-01-01"), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.Date("2009-01-01"), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.Date("2012-01-01"), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.Date("2017-01-02"), linetype = "dashed", color = "blue", linewidth = 0.1) +
  facet_wrap(~wave, nrow = 2, scales = "free") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
  scale_color_discrete(palette = "Set4") +
 labs(x = "Survey Interview Date",
      y = "Count of Responses") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6, color = "black"),
        text = element_text(family = 'serif', color = "black"),
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
        plot.caption = element_text(hjust = 0),
        panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.3),
        panel.grid.minor.y = element_line(linetype = "dashed", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey80", colour = "black", linewidth = 1)
  )

interview_dates_barchart

# Exporting relevant figures ------------------------------------------------

# Interview dates barchart (facets)

ggsave("figures/interview_dates_barchart.png", 
        interview_dates_barchart, 
        width = 17, 
        height = 10,
        units = "cm", 
        dpi = 800)

# Temperature visualization --------------------------------------------------

# Get the overall sd for the temperatures

temperature_sd <- 
  temperature_df %>% 
  summarise(sd_avg_temp = sd(avg_temp, na.rm = T),
            sd_min_temp = sd(min_temperature, na.rm = T),
            sd_max_temp = sd(max_temperature, na.rm = T)) %>% 
  data.frame()
  
# Monthly mean temperatures, all country (Polar coordinates)

ecuador_monthly_mean_temps <-
  temperature_df %>% 
  group_by(month = month(date, label = T)) %>% 
  summarise(avg_temp = mean(avg_temp, na.rm = T),
            min_temp = mean(min_temperature, na.rm = T),
            max_temp = mean(max_temperature, na.rm = T),
            sd_avg_temp = sd(avg_temp, na.rm = T),
            sd_min_temp = sd(min_temperature, na.rm = T),
            sd_max_temp = sd(max_temperature, na.rm = T)) %>%
  pivot_longer(cols = c(avg_temp, min_temp, max_temp, sd_avg_temp, sd_min_temp, sd_max_temp), 
               names_to = "temperature_type", 
               values_to = "value") %>% 
  mutate(base = case_when(
    temperature_type == "avg_temp" ~ 19.5,
    temperature_type == "min_temp" ~ 12.5,
    temperature_type == "max_temp" ~ 23
  )) %>%
  arrange(temperature_type, month)

ecuador_monthly_mean_temps %>% 
  filter(temperature_type %in% c("avg_temp", "min_temp", "max_temp")) %>%
  ggplot(aes(x = as.factor(month), y = value - base, group = as.factor(month))) +
  geom_col() +
  facet_wrap(~temperature_type) +
  labs(title = "Monthly Average Temperature for Ecuador",
       x = "Month",
       y = "Average Temperature (C)")  + 
  coord_polar()

# Standard deviation of the monthly mean temperatures, all country (Polar coordinates)

ecuador_monthly_mean_temps %>% 
  filter(temperature_type %in% c("sd_min_temp", "sd_max_temp")) %>%
  ggplot(aes(x = as.factor(month), y = value, group = as.factor(month))) +
  geom_col() +
  facet_wrap(~temperature_type) +
  labs(title = "Standard Deviation of Monthly Average Temperature for Ecuador",
       x = "Month",
       y = "Standard Deviation (C)")  + 
  coord_polar()