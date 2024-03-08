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
library(readr)
library(lubridate)
library(stringr)
library(fuzzyjoin)

# Load AmericasBarometer (AB) data

# Merged Ecuador file (2004-2023)

ecu_ab_raw <- read_sav("data/americas_barometer/ECU_merge_2004-2023_LAPOP_AmericasBarometer_v1.0_w.sav")

# 2010 file for Ecuador

ecu_ab_2010_raw <- read_sav("data/americas_barometer/1707311029Ecuador_LAPOP_AmericasBarometer 2010 data set  approved v3.sav")

# Weather data

min_temperature_df <- read_csv("data/weather/min_temperature.csv",
                               show_col_types = FALSE)

max_temperature_df <- read_csv("data/weather/max_temperature.csv",
                                 show_col_types = FALSE)

precipitation_raw <- read_csv("data/weather/precipitation.csv",
                              show_col_types = FALSE)

# Cantons data, clean to match with AB canton names

ecuador_cantons_df <- 
    read_csv("data/other/ecuador_cantons.csv",
              show_col_types = FALSE) %>% 
    mutate(canton_name_clean = str_to_lower(canton_name) %>% 
                               str_replace_all("cantón","") %>% 
                               str_remove("^[346]\\s*") %>%  
                               str_trim(),
           province_name_clean = str_to_lower(prov) %>% 
                                 str_remove("^[346]\\s*") %>%  
                                 str_trim()) %>% 
    arrange(canton_name_clean, province_name_clean)

# AB Data Cleaning (full file 2004-2023) ------------------------------------------------------------

# Apply or remove the labels for relevant variables
# Join the 2010 file with the main file to get 2010 canton names
# Create clean cantons names and ids to join to weather data later
# Also do the provinces, will need them for doing a canton-province join due to cantons having the same name in different provinces

ecu_ab <-
    ecu_ab_raw %>% 
    left_join(ecu_ab_2010_raw %>% select(idnum, municipio2010 = municipio), by = "idnum") %>%
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
               year == 2010 ~ as_factor(municipio2010),
               year %in% c(2012,2014, 2016, 2019, 2023) ~ as_factor(municipio),
               year == 2021 ~ as_factor(municipio1t)),
            canton_name_clean = str_to_lower(canton_name_ab) %>% 
                                str_replace_all("cantón","") %>%
                                str_replace_all("distrito metropolitano de","") %>%
                                str_remove("^[346]\\s*") %>%  
                                str_trim(),
            province_id_ab = if_else(year == 2021, as.character(prov1t), as.character(prov)),
            province_name_ab = if_else(year == 2021, as_factor(prov1t), as_factor(prov)),
            province_name_clean = str_to_lower(province_name_ab) %>% 
                                  str_trim())

# Canton name matching ------------------------------------------------------------

# Extract unique canton-province combinations from the AB data

unique_cantons_ab <- 
    ecu_ab %>%
    select(canton_id_ab, canton_name_clean, province_name_clean) %>% 
    distinct(canton_name_clean, province_name_clean, .keep_all = T) %>% 
    arrange(canton_name_clean)

# Match unique canton names from the AB data with the canton names from the cantons data
# Use the JW method to match the names (string distance matching)

matched_ab_cantons <- 
    unique_cantons_ab %>% 
    stringdist_left_join(ecuador_cantons_df %>% select(canton_id, canton_name_clean_dpa = canton_name_clean, province_name_dpa = province_name_clean, canton_name), 
                         by = c("canton_name_clean" = "canton_name_clean_dpa"), 
                         method = "jw",
                         max_dist = 0.1,
                         distance_col = "string_distance_cantons") %>% 
    arrange(desc(string_distance_cantons))

# Most are good matches, but some are not. 

# Join the matched canton names with the AB data ------------------------------------------------------------

# Join based on province and canton names

ecu_ab_with_cantons <- 
    ecu_ab %>% 
    left_join(matched_ab_cantons %>% select(canton_name_clean_dpa, province_name_dpa, canton_id, canton_name), 
              by = c("canton_name_clean" = "canton_name_clean_dpa", "province_name_clean" = "province_name_dpa")) 

# Count missing values for canton names (matched) per year

ecu_ab_with_cantons %>% 
    group_by(year) %>% 
    summarise(missing_canton = sum(is.na(canton_name)))

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