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

# 2021 file for Ecuador

ecu_ab_2021 <- read_dta("data/americas_barometer/ECU_2021_LAPOP_AmericasBarometer_v1.2_w.dta")

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

# Missing values of basic variables, by year

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_ur = sum(is.na(ur)),
            missing_q1 = sum(is.na(q1)),
            missing_ur1new = sum(is.na(ur1new)),
            missing_q2 = sum(is.na(q2)),
            missing_ed = sum(is.na(ed)),
            missing_edre = sum(is.na(edre)))
        
# Joining data with the 2021 file to get the gender variable

ecu_joined_2021<-
  ecu_ab_raw %>%
  left_join(ecu_ab_2021 %>% select(idnum, uniq_id, q1tb), by = c("idnum" = "uniq_id"))

# Look at the missing values of the gender variable

ecu_joined_2021 %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_q1tb = sum(is.na(q1tb))) %>% 
  mutate(perc_q1tb = missing_q1tb / total_obs)

# Look at etid

ecu_ab_raw %>%
  select(etid) %>%
  glimpse()

# Count missing values per year

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_etid = sum(is.na(etid))) %>% 
  mutate(perc_etid = missing_etid / total_obs)

# Regional etid (etid2), count 

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_etid2 = sum(is.na(etid2))) %>% 
  mutate(perc_etid2 = missing_etid2 / total_obs)

# Count missing values of religion q3ca

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_q3ca = sum(is.na(q3ca))) %>% 
  mutate(perc_q3ca = missing_q3ca / total_obs)

# Civil status (q11), count, 2004 thru 2010

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_q11 = sum(is.na(q11))) %>% 
  mutate(perc_q11 = missing_q11 / total_obs)

# Print labels 

print_labels(ecu_ab_raw$q11)

# Civil status q11n 2014 thru 2023

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_q11n = sum(is.na(q11n))) %>% 
  mutate(perc_q11n = missing_q11n / total_obs)

# Print labels

print_labels(ecu_ab_raw$q11n)

# Children (q12)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_q12 = sum(is.na(q12))) %>% 
  mutate(perc_q12 = missing_q12 / total_obs)

# Count missing values of religion variables (q3)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_q3 = sum(is.na(q3))) %>% 
  mutate(perc_q3 = missing_q3 / total_obs)

# Same, for q3c

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_q3c = sum(is.na(q3c))) %>% 
  mutate(perc_q3c = missing_q3c / total_obs)

# q3cn

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_q3cn = sum(is.na(q3cn))) %>% 
  mutate(perc_q3cn = missing_q3cn / total_obs)

# Print labels of all three religion labels

print_labels(ecu_ab_raw$q3)
print_labels(ecu_ab_raw$q3c)
print_labels(ecu_ab_raw$q3cn)

# Look at ed2 variable mothers education

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_ed2 = sum(is.na(ed2))) %>% 
  mutate(perc_ed2 = missing_ed2 / total_obs)

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

# Count missing values of the l1 variable

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_l1 = sum(is.na(l1))) %>%
  mutate(perc_l1 = missing_l1 / total_obs) %>%
  ungroup()

# M2, congress job approval

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_m2 = sum(is.na(m2))) %>%
  mutate(perc_m2 = missing_m2 / total_obs) %>%
  ungroup()

# exc7, corruption perception

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_exc7 = sum(is.na(exc7))) %>%
  mutate(perc_exc7 = missing_exc7 / total_obs) %>%
  ungroup()

# exc18, corruption tolerance

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_exc18 = sum(is.na(exc18))) %>%
  mutate(perc_exc18 = missing_exc18 / total_obs) %>%
  ungroup()

# ing4, support for democracy

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_ing4 = sum(is.na(ing4))) %>%
  mutate(perc_ing4 = missing_ing4 / total_obs) %>%
  ungroup()

# Count missing value for dem30

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_dem30 = sum(is.na(dem30))) %>%
  mutate(perc_dem30 = missing_dem30 / total_obs) %>%
  ungroup()

# count missing values for political interest pol1

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_pol1 = sum(is.na(pol1))) %>%
  mutate(perc_pol1 = missing_pol1 / total_obs) %>%
  ungroup()

# See labels for pol1

print_labels(ecu_ab_raw$pol1)

# Look at many confidence in institutions variables (b4 thru b37)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_b4 = sum(is.na(b4)),
            missing_b6 = sum(is.na(b6)),
            missing_b10 = sum(is.na(b10a)),
            missing_b11 = sum(is.na(b11)),
            missing_b12 = sum(is.na(b12)),
            missing_b13 = sum(is.na(b13)),
            missing_b14 = sum(is.na(b14)),
            missing_b15 = sum(is.na(b15)),
            missing_b16 = sum(is.na(b16)),
            missing_b17 = sum(is.na(b17)),
            missing_b18 = sum(is.na(b18)),
            missing_b19 = sum(is.na(b19)),
            missing_b20 = sum(is.na(b20)),
            missing_b21 = sum(is.na(b20a)),
            missing_b22 = sum(is.na(b21)),
            missing_b23 = sum(is.na(b21a)),
            missing_b24 = sum(is.na(b23)),
            missing_b25 = sum(is.na(b31)),
            missing_b26 = sum(is.na(b32)),
            missing_b27 = sum(is.na(b33)),
            missing_b28 = sum(is.na(b37)),
            missing_b29 = sum(is.na(b39)),
            missing_b30 = sum(is.na(b40)),
            missing_b31 = sum(is.na(b47)),
            missing_b32 = sum(is.na(b42)),
            missing_b33 = sum(is.na(b44)),
            missing_b34 = sum(is.na(b46)),
            missing_b35 = sum(is.na(b46a)),
            missing_b36 = sum(is.na(b47a)))

# Calculate the percent missing with transmute of b32 and cosmun12

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_b32 = sum(is.na(b32)),
            missing_cosmun12 = sum(is.na(cosmun12))) %>% 
  transmute(year, perc_b32 = missing_b32 / total_obs,
            perc_cosmun12 = missing_cosmun12 / total_obs)

# Calculate percent missing of muni6

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_muni6 = sum(is.na(muni6))) %>%
  transmute(year, perc_muni6 = missing_muni6 / total_obs)

# Look at missing values of the soct1, soct2, and soct3 variables

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_soct1 = sum(is.na(soct1)),
            missing_soct2 = sum(is.na(soct2)),
            missing_soct3 = sum(is.na(soct3)),
            missing_idio2 = sum(is.na(idio2)),
            missing_ocup = sum(is.na(ocup4a))) %>%
  mutate(perc_soct1 = missing_soct1 / total_obs,
         perc_soct2 = missing_soct2 / total_obs,
         perc_soct3 = missing_soct3 / total_obs)

# Look at ecumuni3 and ecumuni7 missing values

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(total_obs = n(),
            missing_ecumuni3 = sum(is.na(ecumuni3)),
            missing_ecumuni7 = sum(is.na(ecumuni7))) %>%
  mutate(perc_ecumuni3 = missing_ecumuni3 / total_obs,
         perc_ecumuni7 = missing_ecumuni7 / total_obs)
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

# Retrospective vote variables

# Vote in 2002 Presidential election: vb3_04
# Asked in 2004 only
# Count nas per year

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb3_04 = sum(is.na(vb3_04)),
            count = n())

# First round 2002: ecuvb3. Asked in 2004 and 2006
# Count nas per year

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_ecuvb3 = sum(is.na(ecuvb3)),
            count = n())

# First round 2006: vb3_08. Asked in 2008

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb3_08 = sum(is.na(vb3_08)),
            count = n())

# Should look for whether or not voted for Correa

print_labels(ecu_ab_raw$vb3_08)

# First round 2009: vb3_10. Asked in 2010

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb3_10 = sum(is.na(vb3_10)),
            count = n())

# Should look for whether or not voted for Correa

print_labels(ecu_ab_raw$vb3_10)

# First round 2009: vb3_12. Asked in 2012

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb3_12 = sum(is.na(vb3_12)),
            count = n())

# Should look for whether or not voted for Correa

print_labels(ecu_ab_raw$vb3_12)

# First round 2013: vb3n_14. Asked in 2014

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb3n_14 = sum(is.na(vb3n_14)),
            count = n())

# Should look for whether or not voted for Correa

print_labels(ecu_ab_raw$vb3n_14)

# First round 2013: vb3n_16. Asked in 2016

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb3n_16 = sum(is.na(vb3n_16)),
            count = n())

# Should look for whether or not voted for Correa

print_labels(ecu_ab_raw$vb3n_16)

# First round 2017: vb3n_18. Asked in 2019

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb3n_18 = sum(is.na(vb3n_18)),
            count = n())

# Should look if voted for Moreno 

print_labels(ecu_ab_raw$vb3n_18)

# First round 2021: vb3n asked in 2023

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb3n = sum(is.na(vb3n)),
            count = n())

# Should look if voted for Lasso

print_labels(ecu_ab_raw$vb3n)

# Other vote-related variables ------------------------------------------

# Registered to vote: vb1

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb1 = sum(is.na(vb1)),
            count = n())

# Voted: vb2

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb2 = sum(is.na(vb2)),
            count = n())

# Identify with a political party vb10

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb10 = sum(is.na(vb10)),
            count = n())

# What political party do you identify with? ecuvb11 (only 2006)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_ecuvb11 = sum(is.na(ecuvb11)),
            count = n())

# What political party do you identify with? vb11_08 (only 2008)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb11_08 = sum(is.na(vb11_08)),
            count = n())

# What political party do you identify with? vb11_10 (only 2010)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb11_10 = sum(is.na(vb11_10)),
            count = n())

# What political party do you identify with? vb11_12 (only 2012)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb11_12 = sum(is.na(vb11_12)),
            count = n())

# What political party do you identify with? vb11_14 (only 2014)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb11_14 = sum(is.na(vb11_14)),
            count = n())

# What political party do you identify with? vb11_16 (only 2016)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb11_16 = sum(is.na(vb11_16)),
            count = n())

# What political party do you identify with? vb11_18 (only 2018)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb11_18 = sum(is.na(vb11_18)),
            count = n())

# What political party do you identify with? vb11 (only 2023)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb11 = sum(is.na(vb11)),
            count = n())

# Negative, dislike political party vb10neg

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb10neg = sum(is.na(vb10neg)),
            count = n())

# Prospective vote ----------------------------------------------------

# VB20

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_vb20 = sum(is.na(vb20)),
            count = n())

# Look for the incumbent president on every year

# Print labels

print_labels(ecu_ab_raw$vb20)

# Social transfers ----------------------------------------------------

# cct1 (2010, 2012 only)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_cct1 = sum(is.na(cct1)),
            count = n())

# cct1b (2012 thru 2023, except 2021)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_cct1b = sum(is.na(cct1b)),
            count = n())

# Clientelism --------------------------------------------------------

# clien1, count missing values (2010 only)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_clien1 = sum(is.na(clien1)),
            count = n())
  
# clien1n count missing 

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_clien1n = sum(is.na(clien1n)),
            count = n())

# only 2014

# clien1na

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_clien1na = sum(is.na(clien1na)),
            count = n())

# clien2

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_clien2 = sum(is.na(clien2)),
            count = n())
  
# Political efficiency ------------------------------------------------

# Eff1

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_eff1 = sum(is.na(eff1)),
            count = n())

ecu_ab_raw$eff1

# Eff2

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_eff2 = sum(is.na(eff2)),
            count = n())

# Epp1

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_epp1 = sum(is.na(epp1)),
            count = n())

# Epp2

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_epp2 = sum(is.na(epp2)),
            count = n())

# Epp 3

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_epp3 = sum(is.na(epp3)),
            count = n())

# Protests prot3

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_prot3 = sum(is.na(prot3)),
            count = n())

# Life satisfaction ---------------------------------------------------

# ls3 (up until 2016)

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_ls3 = sum(is.na(ls3)),
            count = n())

# Show labels

print_labels(ecu_ab_raw$ls3)

# ls4 

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_ls4 = sum(is.na(ls4)),
            count = n())

# ls6

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_ls6 = sum(is.na(ls6)),
            count = n())

# ls6a 

ecu_ab_raw %>%
  group_by(year) %>%
  summarise(missing_ls6a = sum(is.na(ls6a)),
            count = n())