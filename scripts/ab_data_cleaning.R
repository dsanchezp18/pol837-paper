# R Script: AmericasBarometer Data Cleaning
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script performs data cleaning on the AmericasBarometer data.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr)
library(ggplot2)
library(haven)
library(lubridate)

# Load AmericasBarometer (AB) data

# Merged Ecuador file (2004-2023)

ecu_ab_raw <- read_sav("data/americas_barometer/ECU_merge_2004-2023_LAPOP_AmericasBarometer_v1.0_w.sav")

# Data Cleaning (full file 2004-2023) ------------------------------------------------------------

# Apply or remove the labels for relevant variables

ecu_ab <-
    ecu_ab_raw %>% 
    mutate(year = zap_labels(year),
           wave = zap_labels(wave),
           pais = as_factor(pais),
           fecha = if_else(fecha == "NR", NA_character_, fecha),
           municipality = if_else(year %in% 2004:2006, as_factor(canton), as_factor(municipio)))

# Data cleaning (2008-2023) ------------------------------------------------------------

# Select only 2008-2023 and clean the data

ecu_ab_2008_2023 <- 
    ecu_ab %>% 
    filter(year >= 2008)  %>% 
    mutate(interview_date = parse_date_time(fecha, order = "dby"))

