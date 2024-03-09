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

precipitation_raw <- read_csv("data/weather/precipitation.csv",
                              show_col_types = FALSE)

ecuador_cantons_df <- read_csv("data/other/ecuador_cantons.csv",
                               show_col_types = FALSE)

# Data Cleaning -----------------------------------------------------------

# Create a temperature dataframe
# Creating the average temperature variable and add canton identifiers to the temperature data

temperature_df <- 
  min_temperature_df %>% 
  relocate(year, .after = canton_id) %>%
  rename(min_temp = value) %>% 
  left_join(max_temperature_df  %>% select(canton_id, date, max_temp = value), by = c("date", "canton_id"))  %>% 
  left_join(ecuador_cantons_df, by = "canton_id")  %>%
  mutate(spread_temp = max_temp - min_temp,
         avg_temp = rowMeans(select(., min_temp, max_temp))) %>% 
  relocate(canton_name, prov, .after = canton_id) 

# Create a precipitation dataframe

precipitation_df <- 
  precipitation_raw %>% 
  relocate(year, .after = canton_id) %>%
  rename(precipitation = value) %>%
  left_join(ecuador_cantons_df, by = "canton_id")  %>%
  relocate(canton_name, prov, .after = canton_id)

# Export the data ---------------------------------------------------------

write_csv(temperature_df, "data/weather/temperature_processed.csv") # ignored in github due to size

write_csv(precipitation_df, "data/weather/precipitation_processed.csv")
