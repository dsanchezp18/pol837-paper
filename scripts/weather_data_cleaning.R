# R Script: Weather Data Cleaning
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script performs data cleaning on the daily weather data from the National Oceanic and Atmospheric Administration (NOAA) for Ecuador.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr)
library(readr)
library(lubridate)

# Load the data

min_temperature_df <- read_csv("data/weather/min_temperature.csv",
                               show_col_types = FALSE)

max_temperature_df <- read_csv("data/weather/max_temperature.csv",
                                 show_col_types = FALSE)

precipitation_df <- read_csv("data/weather/precipitation.csv",
                              show_col_types = FALSE)

# Data Cleaning -----------------------------------------------------------

# Creating the average temperature variable

temperature_df <- 
  min_temperature_df %>% 
  rename(min_temp = value) %>% 
  left_join(max_temperature_df  %>% select(canton_id, date, max_temp = value), by = c("date", "canton_id"))  %>% 
  mutate(spread_temp = max_temp - min_temp,
         avg_temp = spread_temp/2)