# R Script: NOAA Weather Data Download
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script downloads the processed daily weather data from the National Oceanic and Atmospheric Administration (NOAA) for Ecuador.
# I download from Laboratorio LIDE's GitHub repository, which contains the processed data and relevant code for replication. 

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr)
library(readr)

# Downloading the data -----------------------------------------------------

# Download the processed daily weather data from Laboratorio LIDE's GitHub repository

# 1. Minimum temperature

min_temperature_df <- read_csv("https://raw.githubusercontent.com/laboratoriolide/ecuador-temperature-noaa/main/data/weather/processed/min_temperature.csv",
                               show_col_types = FALSE)  

# 2. Maximum temperature

max_temperature_df <- read_csv("https://raw.githubusercontent.com/laboratoriolide/ecuador-temperature-noaa/main/data/weather/processed/max_temperature.csv",
                                show_col_types = FALSE)

# 3. Precipitation

precipitation_df <- read_csv("https://raw.githubusercontent.com/laboratoriolide/ecuador-temperature-noaa/main/data/weather/processed/precipitation.csv",
                             show_col_types = FALSE)

# Exporting the data -------------------------------------------------------

# Export the data to the data folder

write_csv(min_temperature_df, "data/weather/min_temperature.csv")

write_csv(max_temperature_df, "data/weather/max_temperature.csv")

write_csv(precipitation_df, "data/weather/precipitation.csv")



