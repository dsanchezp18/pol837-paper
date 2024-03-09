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
                               show_col_types = FALSE) %>% 
                      select(canton_id,
                             date, 
                             min_temperature = value)

max_temperature_df <- read_csv("data/weather/max_temperature.csv",
                                show_col_types = FALSE) %>%
                        select(canton_id,
                               date, 
                               max_temperature = value)                   

precipitation_df <- read_csv("data/weather/precipitation.csv",
                              show_col_types = FALSE) %>% 
                        select(canton_id,
                               date, 
                               precipitation = value)

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
    distinct(canton_id_ab, .keep_all = T) %>% 
    arrange(canton_name_clean)

## Fuzzy matching of canton names ------------------------------------------------------------

# Match unique canton names from the AB data with the canton names from the cantons data
# Use the JW method to match the names (string distance matching)

fuzzy_matches <- 
    unique_cantons_ab %>% 
    stringdist_left_join(ecuador_cantons_df %>% select(canton_id, canton_name_clean_dpa = canton_name_clean, province_name_dpa = prov), 
                         by = c("canton_name_clean" = "canton_name_clean_dpa"), 
                         method = "jw",
                         max_dist = 0.101,
                         distance_col = "string_distance_cantons") %>%
    arrange(desc(string_distance_cantons))

# Most are good matches, but some are not, filter out those that I manually checked that are not good matches
# 913902 and 901302: bolivar, manabi
# 
correct_fuzzy_matches <-
    fuzzy_matches %>% 
    filter(!(canton_id_ab %in% c(913902, 901302)), !is.na(canton_id)) %>% 
    select(canton_id_ab, canton_id)

# Manual matching ------------------------------------------------------------

# Get all NA matches and manually match them

non_matches <- 
    fuzzy_matches %>% 
    filter(is.na(canton_id)) %>% 
    select(canton_id_ab, canton_name_clean, province_name_clean)

# Construct a manual matching table for the canton names that were not matched correctly

manual_matches <- 
    tibble(canton_id_ab = c(913902, 901302, 900036, 918902, 901802, 900205,900030, 900137,919907,900081,909907, 900907,900083,90907, 909908,900908,
                            900084, 900177,900144, 900115, 91401, 917903, 900185, 900008, 900116,900147, 900148, 900209, 900106, 900011, 90106, 900049,900210,91001,923901,
                            902301,92301, 90100, 914906, 900168, 900015, 900035, 909919, 900919, 900158, 909920),
           canton_id = c(1302, 1302, 602,1802,1802,1802,402, 1302,1907,923,907,907,907,907, 908, 908, 908, 2201, 1307, 1108, 1401, 1703, 1703, 104, 1116, 1318, 1310, 1807,106, 106, 106, 504, 1808, 
                         1001, 2301, 2301, 2301, 922, 1406, 1406,109, 401, 919, 919, 1316, 920)
    ) %>% 
    mutate_all(as.character)

# Combine the correct fuzzy matches with the manual matches

ab_cantons_to_dpa_cantons <- 
    correct_fuzzy_matches %>% 
    bind_rows(manual_matches)

# Join the matched canton names with the AB data ------------------------------------------------------------
# Join based on province and canton names

ecu_ab_with_cantons <- 
    ecu_ab %>% 
    left_join(ab_cantons_to_dpa_cantons, by = "canton_id_ab")

# Count missing values for canton names (matched) per year

ecu_ab_with_cantons %>% 
    group_by(year) %>% 
    summarise(missing_canton = sum(is.na(canton_id)))

# Data cleaning (2008-2023) ------------------------------------------------------------

# Select only 2008-2023 and clean the data
# Create a date variable from the fecha variable

ecu_ab_2008_2023 <- 
    ecu_ab_with_cantons %>% 
    filter(year >= 2008)  %>% 
    mutate(interview_date = dmy(fecha))

# Canton and weather data ------------------------------------------------------------

# Join the AB data with the canton data to get the canton names and ids
# Also join with the weather data, by day and canton, to get daily temperatures and precipitation

df <- 
    ecu_ab_2008_2023 %>%
    left_join(ecuador_cantons_df %>% select(canton_id, canton_dpa = canton_name, province_dpa = prov, prov_id), by = "canton_id") %>% 
    left_join(min_temperature_df, by = c("interview_date" = "date", "canton_id")) %>% 
    left_join(max_temperature_df, by = c("interview_date" = "date", "canton_id")) %>% 
    left_join(precipitation_df, by = c("interview_date" = "date", "canton_id"))

# Export the final data ------------------------------------------------------------

# As a CSV file

write_csv(df, "data/full_df.csv")

# As an RData file

save(df, file = "data/full_df.RData")