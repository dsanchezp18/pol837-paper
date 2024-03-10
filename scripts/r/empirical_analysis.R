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

# Simple Model 4: Precipitation

simple_model4 <- 
    feglm(approves_president ~ precipitation | canton_dpa + interview_date, 
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(simple_model4)

# Simple Model 5: Min and Max temperature, with precipitation

simple_model5 <- 
    feglm(approves_president ~ min_temperature + max_temperature + precipitation | canton_dpa + interview_date, 
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(simple_model5)

# Present these models as a markdown table for now 

simple_models <- list(simple_model1, simple_model2, simple_model3, simple_model4, simple_model5)

modelsummary(simple_models, stars = stars, output = "markdown")