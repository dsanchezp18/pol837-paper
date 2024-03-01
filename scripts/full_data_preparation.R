# R Script: AmericasBarometer Data Cleaning
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script joins the AmericasBarometer data with the daily weather data from the National Oceanic and Atmospheric Administration (NOAA) for Ecuador.
# Other preparation done accordingly. 

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr)
library(ggplot2)
library(haven)
library(lubridate)
library(stringr)

# Load AmericasBarometer (AB) data

# Merged Ecuador file (2004-2023)

ecu_ab_raw <- read_sav("data/americas_barometer/ECU_merge_2004-2023_LAPOP_AmericasBarometer_v1.0_w.sav")

# Temperature data 

temperature_df <- read_csv("data/weather/temperature_processed.csv",
                           show_col_types = FALSE)

# Precipitation data

precipitation_df <- read_csv("data/weather/precipitation_processed.csv",
                             show_col_types = FALSE)


# AB Data Cleaning (full file 2004-2023) ------------------------------------------------------------

# Apply or remove the labels for relevant variables

ecu_ab <-
    ecu_ab_raw %>% 
    mutate(year = zap_labels(year),
           wave = zap_labels(wave),
           pais = as_factor(pais),
           fecha = if_else(fecha == "NR", NA_character_, fecha),
           canton_id_ab = case_when(
               year %in% 2004:2008 ~ as.character(canton),
               year == 2010 ~ as.character(municipio10),
               year %in% c(2012,2014, 2016, 2019, 2023) ~ as.character(municipio),
               year == 2021 ~ as.character(municipio1t)),
           canton_name_ab = case_when(
               year %in% 2004:2008 ~ as_factor(canton),
               year == 2010 ~ as_factor(municipio10),
               year %in% c(2012,2014, 2016, 2019, 2023) ~ as_factor(municipio),
               year == 2021 ~ as_factor(municipio1t))
        )

# Data cleaning (2008-2023) ------------------------------------------------------------

# Select only 2008-2023 and clean the data

ecu_ab_2008_2023 <- 
    ecu_ab %>% 
    filter(year >= 2008)  %>% 
    mutate(interview_date = parse_date_time(fecha, order = "dby"))

# Joining the AB with the weather data ------------------------------------------------------------

joined_ab_weather <- 
    ecu_ab_2008_2023 %>% 
    left_join(temperature_df, by = c("year", "canton")) %>% 
    left_join(precipitation_df, by = c("year", "canton"))