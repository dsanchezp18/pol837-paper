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

# Raw Merged Ecuador file (2004-2023)

ecu_ab_raw <- read_sav("data/americas_barometer/ECU_merge_2004-2023_LAPOP_AmericasBarometer_v1.0_w.sav")

# Cleaned Ecuador file (2008-2023)

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

# Look at parroquia (parish)

ecu_ab_raw %>%
  select(paroq) %>%
  glimpse()

# Filter out missing values

ecu_ab_raw %>%
  mutate(parish = zap_missing(paroq)) %>%
  filter(!is.na(parish))  %>% 
  group_by(year)  %>% 
  summarise(n = n())  %>%
  ungroup()

# Look at canton (municipality)

ecu_ab_raw %>%
  select(canton) %>%
  glimpse()

# Filter out missing values

ecu_ab_raw %>%
  mutate(municipality = zap_missing(canton)) %>%
  filter(!is.na(municipality))  %>% 
  group_by(year)  %>% 
  summarise(n = n())  %>%
  ungroup()

# Do the same for municipio 

ecu_ab_raw %>%
  mutate(municipality = zap_missing(municipio)) %>%
  filter(!is.na(municipality))  %>%
  group_by(year)  %>%
  summarise(n = n())  %>% 
  ungroup()

# And for municipio04

ecu_ab_raw %>%
  mutate(municipality = zap_missing(municipio04)) %>%
  filter(!is.na(municipality))  %>%
  group_by(year)  %>%
  summarise(n = n())  %>% 
  ungroup()

# And for municipio05

ecu_ab_raw %>%
  mutate(municipality = zap_missing(municipio06)) %>%
  filter(!is.na(municipality))  %>%
  group_by(year)  %>%
  summarise(n = n())  %>% 
  ungroup()

# And for municipio08

ecu_ab_raw %>%
  mutate(municipality = zap_missing(municipio08)) %>%
  filter(!is.na(municipality))  %>%
  group_by(year)  %>%
  summarise(n = n())  %>% 
  ungroup()

# And for municipio10

ecu_ab_raw %>%
  mutate(municipality = zap_missing(municipio10)) %>%
  filter(!is.na(municipality))  %>%
  group_by(year)  %>%
  summarise(n = n())  %>% 
  ungroup()

# Look at cluster variable

ecu_ab_raw %>%
  select(cluster) %>%
  glimpse()

# Filter out missing values

ecu_ab_raw %>%
  mutate(cluster = zap_missing(cluster)) %>%
  filter(!is.na(cluster))  %>% 
  group_by(year)  %>% 
  summarise(n = n())  %>%
  ungroup()