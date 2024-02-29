# R Script: Weather Data Exploratory Analysis
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script performs an exploratory analysis of the processed daily weather data from the National Oceanic and Atmospheric Administration (NOAA) for Ecuador.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr)
library(readr)
library(ggplot2)

# Load the data

temperature_df <- read_csv("data/weather/temperature_processed.csv",
                           show_col_types = FALSE)

precipitation_df <- read_csv("data/weather/precipitation_processed.csv",
                             show_col_types = FALSE)

# Exploratory Analysis -----------------------------------------------------

## Data for Quito ----------------------------------------------

# Get a yearly average of the avg temperature for Quito 

quito_avg_temp <- 
  temperature_df %>% 
  filter(canton_name == "QUITO") %>% 
  group_by(year) %>% 
  summarise(avg_temp = mean(avg_temp))

# Get a yearly average of the precipitation for Quito

quito_avg_precip <- 
  precipitation_df %>% 
  filter(canton_name == "QUITO") %>% 
  group_by(year) %>% 
  summarise(avg_precip = mean(value))

# Plot the yearly average of temperature for Quito

ggplot(quito_avg_temp, aes(x = year, y = avg_temp)) +
  geom_line() +
  labs(title = "Yearly Average Temperature for Quito",
       x = "Year",
       y = "Average Temperature (C)")

# Plot the yearly average of precipitation for Quito

ggplot(quito_avg_precip, aes(x = year, y = avg_precip)) +
  geom_line() +
  labs(title = "Yearly Average Precipitation for Quito",
       x = "Year",
       y = "Average Precipitation (mm)")

## Data for Guayaquil ----------------------------------------------

# Get a yearly average of the avg temperature for Guayaquil

guayaquil_avg_temp <- 
  temperature_df %>% 
  filter(canton_name == "GUAYAQUIL") %>% 
  group_by(year) %>% 
  summarise(avg_temp = mean(avg_temp))

# Get a yearly average of the precipitation for Guayaquil

guayaquil_avg_precip <- 
  precipitation_df %>% 
  filter(canton_name == "GUAYAQUIL") %>% 
  group_by(year) %>% 
  summarise(avg_precip = mean(value))

# Plot the yearly average of temperature for Guayaquil

ggplot(guayaquil_avg_temp, aes(x = year, y = avg_temp)) +
  geom_line() +
  labs(title = "Yearly Average Temperature for Guayaquil",
       x = "Year",
       y = "Average Temperature (C)")

# Plot the yearly average of precipitation for Guayaquil

ggplot(guayaquil_avg_precip, aes(x = year, y = avg_precip)) +
  geom_line() +
  labs(title = "Yearly Average Precipitation for Guayaquil",
       x = "Year",
       y = "Average Precipitation (mm)")

## National Data ------------------------------------------------

# Get a yearly average of the avg temperature for Ecuador

ecuador_avg_temp <- 
  temperature_df %>% 
  group_by(year) %>% 
  summarise(avg_temp = mean(avg_temp, na.rm = T))

#E Get a yearly average of the precipitation for Ecuador

ecuador_avg_precip <- 
  precipitation_df %>% 
  group_by(year) %>% 
  summarise(avg_precip = mean(value, na.rm = T))
