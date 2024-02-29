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

# Get a yearly average of the avg temperature for Quito 

quito_avg_temp <- 
  temperature_df %>% 
  filter(canton_name == "QUITO") %>% 
  group_by(year) %>% 
  summarise(avg_temp = mean(avg_temp))