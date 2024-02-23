# R Script: AmericasBarometer Exploratory Analysis
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script performs exploratory analysis on the AmericasBarometer data.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr)
library(haven)

# Load AmericasBarometer (AB) data 

# Merged Ecuador file (2004-2023)

ecu_ab_raw <- read_sav("data/ECU_merge_2004-2023_LAPOP_AmericasBarometer_v1.0_w.sav")

# Analysis ----------------------------------------------------------------

# Look at survey date

ecu_ab_raw %>%
  select(fecha) %>%
  glimpse()

# Group by year and type of answer

ecu_ab_raw %>%
  group_by(year, fecha) %>%
  summarise(n = n()) %>%
  ungroup() 

# Neither 2004 nor 2006 seem to have date information

# Type of year variable

ecu_ab_raw %>%
  select(year) %>%
  glimpse()

# Select only 2008-2023

ecu_ab_raw %>%
    filter(year %in% 2008:2023) %>%
    select(fecha) %>%
    glimpse()