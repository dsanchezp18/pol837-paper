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

min_temperature_df <- read_csv("data/weather/min_temperature.csv",
                               show_col_types = FALSE)

max_temperature_df <- read_csv("data/weather/max_temperature.csv",
                                 show_col_types = FALSE)

precipitation_df <- read_csv("data/weather/precipitation.csv",
                                show_col_types = FALSE)

# 

# Exploratory Analysis -----------------------------------------------------

