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

# 2010 file for Ecuador

ecu_ab_2010 <- read_sav("data/americas_barometer/1707311029Ecuador_LAPOP_AmericasBarometer 2010 data set  approved v3.sav")

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

# Look at the m1 variable, presidential job approval

ecu_ab_raw %>%
  select(m1) %>%
  glimpse()

# Count missing values per year

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_m1 = sum(is.na(m1))) %>%
  mutate(perc_m1 = missing_m1 / total_obs) %>%
  ungroup()

# Look at year 2004 ----------------------------------------------------

# Get 2004 only from the total

ecu_ab_2004_raw <- 
  ecu_ab_raw %>% 
  filter(year == 2004)

# Get unique values, both labels and canton names, for 2004

ecu_ab_2004_raw %>% 
  transmute(canton, canton_name = as_factor(canton)) %>% 
  distinct() %>% 
  arrange(canton)

# Look at the canton variables for 2004

ecu_ab_raw %>%
  filter(year == 2004) %>%
  select(canton, municipio, municipio04, municipio06, municipio08, municipio10, municipio1t) %>%
  glimpse()

# Count missing values for each variable

ecu_ab_raw %>%
  filter(year == 2004) %>%
  summarise(missing_canton = sum(is.na(canton)),
            missing_municipio = sum(is.na(municipio)),
            missing_municipio04 = sum(is.na(municipio04)),
            missing_municipio06 = sum(is.na(municipio06)),
            missing_municipio08 = sum(is.na(municipio08)),
            missing_municipio10 = sum(is.na(municipio10)),
            missing_municipio1t = sum(is.na(municipio1t)))

# Look at the unique values for each variable

ecu_2004_cases <-
  ecu_ab_raw %>%
  filter(year == 2004) %>%
  transmute(canton =  as_factor(canton)) %>%
  distinct(.keep_all = T) %>% 
  mutate(canton_name = as_factor(canton))

# Look at 2006 ----------------------------------------------------

# Look at the canton variables for 2006

ecu_ab_raw %>%
  filter(year == 2006) %>%
  select(canton, municipio, municipio04, municipio06, municipio08, municipio10, municipio1t) %>%
  glimpse()

# Count missing values for each variable

ecu_ab_raw %>%
  filter(year == 2006) %>%
  summarise(missing_canton = sum(is.na(canton)),
            missing_municipio = sum(is.na(municipio)),
            missing_municipio04 = sum(is.na(municipio04)),
            missing_municipio06 = sum(is.na(municipio06)),
            missing_municipio08 = sum(is.na(municipio08)),
            missing_municipio10 = sum(is.na(municipio10)),
            missing_municipio1t = sum(is.na(municipio1t)))

# Look at the unique values for each variable

ecu_2006_cases <-
  ecu_ab_raw %>%
  filter(year == 2006) %>%
  select(canton) %>%
  distinct(.keep_all = T) %>% 
  mutate(canton_name = as_factor(canton))

# Look at province missing values by year

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_canton = sum(is.na(prov))) %>%
  ungroup()

# Look at 2008 ----------------------------------------------------

# Look at the canton variables for 2008

ecu_ab_raw %>%
  filter(year == 2008) %>%
  select(canton, municipio, municipio04, municipio06, municipio08, municipio10, municipio1t) %>%
  glimpse()

# Count missing values for each variable

ecu_ab_raw %>%
  filter(year == 2008) %>%
  summarise(missing_canton = sum(is.na(canton)),
            missing_municipio = sum(is.na(municipio)),
            missing_municipio04 = sum(is.na(municipio04)),
            missing_municipio06 = sum(is.na(municipio06)),
            missing_municipio08 = sum(is.na(municipio08)),
            missing_municipio10 = sum(is.na(municipio10)),
            missing_municipio1t = sum(is.na(municipio1t)))

# Look at the unique values for each variable

ecu_2008_cases <-
  ecu_ab_raw %>%
  filter(year == 2008) %>%
  select(canton) %>%
  distinct(.keep_all = T) %>% 
  mutate(canton_name = as_factor(canton))

# Look at 2010 ----------------------------------------------------

# Look at the canton variables for 2010

ecu_ab_raw %>%
  filter(year == 2010) %>%
  select(canton, municipio, municipio04, municipio06, municipio08, municipio10, municipio1t) %>%
  glimpse()

# Count missing values for each variable

ecu_ab_raw %>%
  filter(year == 2010) %>%
  summarise(missing_canton = sum(is.na(canton)),
            missing_municipio = sum(is.na(municipio)),
            missing_municipio04 = sum(is.na(municipio04)),
            missing_municipio06 = sum(is.na(municipio06)),
            missing_municipio08 = sum(is.na(municipio08)),
            missing_municipio10 = sum(is.na(municipio10)),
            missing_municipio1t = sum(is.na(municipio1t)))

# Count cases for the 2010 year

ecu_ab_raw %>%
  filter(year == 2010) %>%
  summarise(n = n())

# Look at the unique values for each variable

ecu_2010_cases <-
  ecu_ab_raw %>%
  filter(year == 2010) %>%
  select(municipio10) %>%
  distinct(.keep_all = T) %>% 
  mutate(canton_name = as_factor(municipio10))

# 2010 has missing value labels for municipio10.

# Left join with the 2010 file to get the correct labels

joined_ab_2010 <-
  ecu_ab_raw %>%
  filter(year == 2010) %>% 
  select(idnum, municipio10) %>%
  left_join(ecu_ab_2010 %>% select(idnum, municipio), by = "idnum")

# Filter only for 2021 ----------------------------------------------------

ecu_ab_2021_raw <- 
  ecu_ab_raw %>% 
  filter(year == 2021)

# Look for missing values in municipality variables 

ecu_ab_2021_raw %>%
  summarise(missing_canton = sum(is.na(canton)),
            missing_municipio = sum(is.na(municipio)),
            missing_municipio04 = sum(is.na(municipio04)),
            missing_municipio06 = sum(is.na(municipio06)),
            missing_municipio08 = sum(is.na(municipio08)),
            missing_municipio10 = sum(is.na(municipio10)),
            missing_municipio1t = sum(is.na(municipio1t)))

# Look at estratopri, estratosec, upm, paroq, ur, ur1new, tamano, clusters missing 

ecu_ab_2021_raw %>%
  select(estratopri, estratosec, upm, paroq, ur, ur1new, tamano, cluster) %>%
  summarise(missing_estratopri = sum(is.na(estratopri)),
            missing_estratosec = sum(is.na(estratosec)),
            missing_upm = sum(is.na(upm)),
            missing_paroq = sum(is.na(paroq)),
            missing_ur = sum(is.na(ur)),
            missing_ur1new = sum(is.na(ur1new)),
            missing_tamano = sum(is.na(tamano)),
            missing_cluster = sum(is.na(cluster)))

# Group by UPM, apply labels and see counts 

ecu_ab_2021_raw %>%
  group_by(upm) %>%
  summarise(n = n()) %>%
  ungroup()