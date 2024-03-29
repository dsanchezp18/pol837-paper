# R Script: Empirical Models
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script estimates performs the empirical analysis for the POL837 term research paper.
# Performs descriptive statistics, and estimates for the main empirical models through regressions.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(readr)
library(dplyr)
library(fixest)
library(modelsummary)
library(forcats)

# Load the full dataset

load("data/full_df.RData")

# Redefine the full dataset

full_df <- df

# The "main" dataset for the analysis is the full dataset without the 2021 observations
 
df <- full_df  %>% filter(year != 2021)

# Table presentation parameters (modelsummary) -----------------------------

# Set the modelsummary parameters for the tables

stars <- c("*" = 0.1,
           "**" = 0.05,
           "***" = 0.05)

# Descriptive statistics --------------------------------------------------

# Empirical models ---------------------------------------------------------

## Baseline models: simple fixed effects ------------------------------------

# Estimate very simple models with only day and canton fixed effects
# Vary the specifications in this table by varying the temperature variable (min, max, or average) and include both max and min and precipitation
# These models contain all data available

# Simple Model 1: Min temperature

simple_model1 <- 
    feglm(approves_president ~ min_temperature | canton_dpa + interview_date, 
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(simple_model1)

# Simple Model 2: Max temperature

simple_model2 <- 
    feglm(approves_president ~ max_temperature | canton_dpa + interview_date, 
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(simple_model2)

# Simple Model 3: Average temperature

simple_model3 <- 
    feglm(approves_president ~ avg_temperature | canton_dpa + interview_date, 
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(simple_model3)

# Simple Model 4: Min and Max temperature, with precipitation

simple_model4 <- 
    feglm(approves_president ~ min_temperature + max_temperature + precipitation | canton_dpa + interview_date, 
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(simple_model4)

# Present these models as a markdown table for now 

simple_models <- list(simple_model1, simple_model2, simple_model3, simple_model4)

modelsummary(simple_models, stars = stars, output = "markdown")

# Fixed effects models with controls ---------------------------------------

# Estimate fixed effects models including various controls for presidential approval
# These do not have 2021 data because the controls are not available for that year

# Define a formula object with all the controls

# controls_formula <- 
#     "+ sex + age + ethnicity + urban_rural + non_religious + education + civil_status + labour_market + country_econ_situation 
#     + personal_econ_situation + voted_for_incumbent + ideology + corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_police + confidence_local_gov"

controls_formula <- 
    "+ sex + age + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + ideology + corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_police + confidence_local_gov | canton_dpa + interview_date"

# Model 1: Min temperature

model1_controls <- 
    feglm(paste("approves_president ~ min_temperature + ", controls_formula) %>% as.formula(), 
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model1_controls)

# Model 2: Max temperature

model2_controls <- 
    feglm(paste("approves_president ~ max_temperature + ", controls_formula) %>% as.formula(), 
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model2_controls)

# Model 3: Average temperature

model3_controls <- 
    feglm(paste("approves_president ~ avg_temperature + ", controls_formula) %>% as.formula(), 
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model3_controls)

# Model 4: Min, max and precipitation

model4_controls <- 
    feglm(paste("approves_president ~ min_temperature + max_temperature + precipitation + ", controls_formula) %>% as.formula(), 
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model4_controls)

# Present these models as a markdown table for now

models_controls <- list(model1_controls, model2_controls, model3_controls, model4_controls)

modelsummary(models_controls, stars = stars, output = "markdown")

# Heterogeneous temperature effects for different groups -------------------

# Estimate models with heterogeneous temperature effects for different groups using interaction terms.
# I only use the last specification (min, max, and precipitation) for these models.

# Model 1: Region (Amazon/Sierra(Mountains)/Coast)

model1_hetero <- 
    feglm(paste("approves_president ~ i(region, ref = 'Sierra', min_temperature) + i(region, ref = 'Sierra', max_temperature) + min_temperature + max_temperature + precipitation", controls_formula) %>% as.formula(), 
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model1_hetero)

# Model 2: country_econ_situation

model2_hetero <- 
    feglm(approves_president ~ country_econ_situation*(min_temperature + max_temperature) + precipitation + sex + age + urban_rural + education + labour_market  + personal_econ_situation + 
          ideology + corruption_perception + corruption_tolerance +  democracy_support + political_pride + confidence_police + confidence_local_gov | canton_dpa + interview_date, 
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model2_hetero)

# Model 3: Personal economic situation

model3_hetero <- 
    feglm(approves_president ~ personal_econ_situation*(min_temperature + max_temperature) +  precipitation + sex + age + urban_rural + education + labour_market + country_econ_situation + 
          ideology + corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_justice + confidence_local_gov | canton_dpa + interview_date,
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model3_hetero)

# Model 4: Ideology

model4_hetero <- 
    feglm(approves_president ~ (ideology*min_temperature) + (ideology*max_temperature) + precipitation + sex + age + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + 
          corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_justice + confidence_local_gov | canton_dpa + interview_date,
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model4_hetero)

# Present these models as a markdown table for now

models_hetero <- list(model1_hetero, model2_hetero, model3_hetero, model4_hetero)

modelsummary(models_hetero, stars = stars, output = "markdown")
