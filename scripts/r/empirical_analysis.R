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
library(forcats)
library(modelsummary)
library(forcats)
library(marginaleffects)

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
           "***" = 0.01)

# Descriptive statistics --------------------------------------------------


# Validity checks ----------------------------------------------------------

# Regressions of weather variables on individual-level explanatory variables
# No variable should be statistically significant after fixed effects

controls_formula_validity <- 
    "+ sex + age + ethnicity + urban_rural + non_religious + education + civil_status + labour_market + country_econ_situation + personal_econ_situation + incumbent_vote + ideology + corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_police + confidence_local_gov + external_efficacy + internal_efficacy"

## Max temperature ---------------------------------------------------------

# Fixed effects, no controls

validity_max_temp_fe <- 
    feols(max_temperature ~ 1,
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          cluster = ~ canton_dpa)

summary(validity_max_temp_fe)

# Controls, no fixed effects

validity_max_temp_no_fe <- 
    feols(paste("max_temperature ~ ", controls_formula_validity) %>% as.formula(), 
          data = df,
          cluster = ~ canton_dpa)

summary(validity_max_temp_no_fe)

# Controls and fixed effects

validity_max_temp_fe_controls <- 
    feols(paste("max_temperature ~ ", controls_formula_validity) %>% as.formula(), 
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          cluster = ~ canton_dpa)

summary(validity_max_temp_fe_controls)

## Min temperature ---------------------------------------------------------

# Fixed effects, no controls

validity_min_temp_fe <- 
    feols(min_temperature ~ 1,
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          cluster = ~ canton_dpa)

summary(validity_min_temp_fe)

# Controls, no fixed effects

validity_min_temp_no_fe <- 
    feols(paste("min_temperature ~ ", controls_formula_validity) %>% as.formula(), 
          data = df,
          cluster = ~ canton_dpa)

summary(validity_min_temp_no_fe)

# Controls and fixed effects

validity_min_temp_fe_controls <- 
    feols(paste("min_temperature ~ ", controls_formula_validity) %>% as.formula(), 
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          cluster = ~ canton_dpa)

summary(validity_min_temp_fe_controls)

## Precipitation ----------------------------------------------------------

# Fixed effects, no controls

validity_precipitation_fe <- 
    feols(precipitation ~ 1,
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          cluster = ~ canton_dpa)

summary(validity_precipitation_fe)

# Controls, no fixed effects

validity_precipitation_no_fe <- 
    feols(paste("precipitation ~ ", controls_formula_validity) %>% as.formula(), 
          data = df,
          cluster = ~ canton_dpa)

summary(validity_precipitation_no_fe)

# Controls and fixed effects

validity_precipitation_fe_controls <- 
    feols(paste("precipitation ~ ", controls_formula_validity) %>% as.formula(), 
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          cluster = ~ canton_dpa)

summary(validity_precipitation_fe_controls)

## Present the results as a markdown table ---------------------------------

# Present only controls and fixed effets, listing all three weather variables

validity_results <- list(validity_max_temp_fe_controls, validity_min_temp_fe_controls, validity_precipitation_fe_controls)

modelsummary(validity_results, stars = stars, output = "markdown")

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

# Also estimate average marginal effects for these models

#apes_baseline <- lapply(simple_models, avg_slopes, type = "response")

#modelsummary(apes_baseline, stars = stars, output = "markdown")

## Fixed effects models with controls ---------------------------------------

# Estimate fixed effects models including various controls for presidential approval
# These do not have 2021 data because the controls are not available for that year

# Define a formula object with all the controls

controls_formula <- 
    "+ sex + age + ethnicity + urban_rural + non_religious + education + civil_status + labour_market + country_econ_situation + personal_econ_situation + incumbent_vote + ideology + corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_police + confidence_local_gov + external_efficacy + internal_efficacy"

# controls_formula <- 
#     "+ sex + age + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + ideology + corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_police + confidence_local_gov | canton_dpa + interview_date"

# Model 1: Min temperature

model1_controls <- 
    feglm(paste("approves_president ~ min_temperature + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

# Model 2: Max temperature

model2_controls <- 
    feglm(paste("approves_president ~ max_temperature + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

# Model 3: Average temperature

model3_controls <- 
    feglm(paste("approves_president ~ avg_temperature + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

# Model 4: Min, max and precipitation

model4_controls <- 
    feglm(paste("approves_president ~ min_temperature + max_temperature + precipitation + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

# Present these models as a markdown table for now

models_controls <- list(model1_controls, model2_controls, model3_controls, model4_controls)

modelsummary(models_controls, stars = stars, output = "markdown")

# Also estimate average marginal effects for these models

#apes_controls <- lapply(models_controls, avg_slopes, type = "response")

#modelsummary(apes_controls, stars = stars, output = "markdown", shape = term:contrast + statistic ~ model,)

## Non-linear effects -------------------------------------------------------

# Estimate quadratic, cubic, and quartic effects of temperature. Maximum temperature for brevity

# Model 1: Baseline quadratic effect (no controls)

model1_quad <- 
    feglm(approves_president ~ max_temperature + I(max_temperature^2),
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model1_quad)

# Model 2: Quadratic effect with controls

model2_quad <- 
    feglm(paste("approves_president ~ max_temperature + I(max_temperature^2) + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model2_quad)

# Model 3: Baseline cubic effect (no controls)

model3_cubic <- 
    feglm(approves_president ~ max_temperature + I(max_temperature^2) + I(max_temperature^3),
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model3_cubic)

# Model 4: Cubic effect with controls

model4_cubic <- 
    feglm(paste("approves_president ~ max_temperature + I(max_temperature^2) + I(max_temperature^3) + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model4_cubic)

# Model 5: Baseline quartic effect (no controls)

model5_quartic <- 
    feglm(approves_president ~ max_temperature + I(max_temperature^2) + I(max_temperature^3) + I(max_temperature^4),
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model5_quartic)

# Model 6: Quartic effect with controls

model6_quartic <- 
    feglm(paste("approves_president ~ max_temperature + I(max_temperature^2) + I(max_temperature^3) + I(max_temperature^4) + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model6_quartic)

# Model 7: log temperature

model7_log <- 
    feglm(approves_president ~ log(max_temperature),
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model7_log)

# Model 8: log temperature with controls

model8_log <- 
    feglm(paste("approves_president ~ log(max_temperature) + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model8_log)

# Model 9: sqrt temperature

model9_sqrt <- 
    feglm(approves_president ~ sqrt(max_temperature),
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model9_sqrt)

# Model 10: sqrt temperature with controls

model10_sqrt <- 
    feglm(paste("approves_president ~ sqrt(max_temperature) + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model10_sqrt)

# List all models 

non_linear_models <- list(model1_quad, model2_quad, model3_cubic, model4_cubic, model5_quartic, model6_quartic, model7_log, model8_log, model9_sqrt, model10_sqrt)

modelsummary(non_linear_models, stars = stars, output = "markdown")

## Heterogeneous temperature effects for different groups -------------------

# Estimate models with heterogeneous temperature effects for different groups using interaction terms.
# I only use the last specification (min, max, and precipitation) for these models.

# Model 1: Region (Amazon/Sierra(Mountains)/Coast), max temperature interaction

model1_hetero <- 
    feglm(paste("approves_president ~ i(region, ref = 'Sierra', max_temperature) + max_temperature", controls_formula) %>% as.formula(), 
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model1_hetero)

# Model 2: Region (Amazon/Sierra(Mountains)/Coast), min temperature interaction

model2_hetero <- 
    feglm(paste("approves_president ~ i(region, ref = 'Sierra', min_temperature) + min_temperature", controls_formula) %>% as.formula(), 
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model2_hetero)

# Model 3: Sex 

model3_hetero <- 
    feglm(approves_president ~ sex*(max_temperature) + age + ethnicity + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + ideology + 
          incumbent_vote + corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa
    )

summary(model3_hetero)

# Model 4: Ethnicity

model4_hetero <- 
    feglm(approves_president ~ i(ethnicity, ref = "Blanca", max_temperature) + max_temperature + age + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + ideology + 
          incumbent_vote + corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
            fixef = c("canton_dpa", "interview_date"),
            data = df,
            family = binomial(link = "logit"),
            cluster = ~ canton_dpa)

summary(model4_hetero)

# Model 5: Incumbent vote

model5_hetero <- 
    feglm(approves_president ~ i(incumbent_vote, ref = "Did not vote", max_temperature) + max_temperature + age + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + ideology + 
          corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
            fixef = c("canton_dpa", "interview_date"),
            data = df,
            family = binomial(link = "logit"),
            cluster = ~canton_dpa)

summary(model5_hetero)

# Model 6: Ideology (Max temp)

model6_hetero <- 
    feglm(approves_president ~ ideology*max_temperature  + age + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + 
          corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~canton_dpa)

summary(model6_hetero)

# Model 7: Ideology (Min temp)

model7_hetero <- 
    feglm(approves_president ~ ideology*min_temperature + age + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + 
          corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~canton_dpa)
        
summary(model7_hetero)

# Model 8: Ideology with both max and min 

model8_hetero <- 
    feglm(approves_president ~ ideology*(min_temperature+ max_temperature) + age + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + 
          corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~canton_dpa)

summary(model8_hetero)

# List all models 

hetero_models <- list(model1_hetero, model2_hetero, model3_hetero, model4_hetero, model5_hetero, model6_hetero, model7_hetero, model8_hetero)

modelsummary(hetero_models, stars = stars, output = "markdown")

## Log temperature with heterogeneous effects -----------------------------

# Estimate models with log temperature and heterogeneous effects for different groups using interaction terms.

# Model 1: Region (Amazon/Sierra(Mountains)/Coast), log temperature interaction

model1_log_hetero <- 
    feglm(paste("approves_president ~ i(region, ref = 'Sierra', log(max_temperature)) + log(max_temperature) + min_temperature + precipitation", controls_formula) %>% as.formula(), 
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model1_log_hetero)

# Model 2: Region, log min temperature interaction

model2_log_hetero <- 
    feglm(paste("approves_president ~ i(region, ref = 'Sierra', log(min_temperature)) + log(min_temperature) + min_temperature + max_temperature + precipitation", controls_formula) %>% as.formula(), 
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model2_log_hetero)

# Model 3: Sex, with log temperature

model3_log_hetero <- 
    feglm(approves_president ~ sex*(log(max_temperature)) + age + ethnicity + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + ideology + 
          incumbent_vote + corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa
    )

summary(model3_log_hetero)

# Model 4: Ethnicity, with log max temperature 

model4_hetero <- 
    feglm(approves_president ~ i(ethnicity, ref = "Blanca", log(max_temperature)) + log(max_temperature) + age + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + ideology + 
          incumbent_vote + corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
            fixef = c("canton_dpa", "interview_date"),
            data = df,
            family = binomial(link = "logit"),
            cluster = ~ canton_dpa)

summary(model4_hetero)

# Model 5: Incumbent vote, with log max temperature

model5_hetero <- 
    feglm(approves_president ~ i(incumbent_vote, ref = "Did not vote", log(max_temperature)) + log(max_temperature) + age + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + ideology + 
          corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
            fixef = c("canton_dpa", "interview_date"),
            data = df,
            family = binomial(link = "logit"),
            cluster = ~canton_dpa)

summary(model5_hetero)

# Model 6: Ideology (Max temp), with log max temperature

model6_hetero <- 
    feglm(approves_president ~ ideology*log(max_temperature)  + age + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + 
          corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~canton_dpa)

summary(model6_hetero)

# Model 7: Ideology (Min temp), with log min temperature

model7_hetero <- 
    feglm(approves_president ~ ideology*log(min_temperature) + age + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + 
          corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~canton_dpa)

summary(model7_hetero)

# Model 8: Ideology with both max and min, with log temperature

model8_hetero <- 
    feglm(approves_president ~ ideology*(log(min_temperature)+ log(max_temperature)) + age + non_religious + civil_status + urban_rural + education + labour_market + country_econ_situation + personal_econ_situation + 
          corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_local_gov,
          fixef = c("canton_dpa", "interview_date"),
          data = df,
          family = binomial(link = "logit"),
          cluster = ~canton_dpa)

summary(model8_hetero)

## Mechanism checks: life satisfaction as an explained variable --------------------------------------------------------

# Estimate the same models on life satisfaction as a dependent variable

# Baseline model, max temp

simple_model_ls_1 <- 
    feglm(satisfied_life ~ max_temperature | canton_dpa + interview_date, 
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(simple_model_ls_1)

# Baseline model, min temp

simple_model_ls_2 <- 
    feglm(satisfied_life ~ min_temperature | canton_dpa + interview_date, 
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(simple_model_ls_2)

# Baseline model, average temp

simple_model_ls_3 <- 
    feglm(satisfied_life ~ avg_temperature | canton_dpa + interview_date, 
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(simple_model_ls_3)

# Baseline model, min, max, and precipitation

simple_model_ls_4 <- 
    feglm(satisfied_life ~ min_temperature + max_temperature + precipitation | canton_dpa + interview_date, 
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(simple_model_ls_4)  

# Controls, max temp

model1_controls_ls <- 
    feglm(paste("satisfied_life ~ max_temperature + ", controls_formula) %>% as.formula(), 
          data = full_df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model1_controls_ls)

# Controls, min temp

model2_controls_ls <- 
    feglm(paste("satisfied_life ~ min_temperature + ", controls_formula) %>% as.formula(), 
          data = full_df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model2_controls_ls)

# Controls, average temp

model3_controls_ls <- 
    feglm(paste("satisfied_life ~ avg_temperature + ", controls_formula) %>% as.formula(), 
          data = full_df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model3_controls_ls)

# Controls, min, max, and precipitation

model4_controls_ls <- 
    feglm(paste("satisfied_life ~ min_temperature + max_temperature + precipitation + ", controls_formula) %>% as.formula(), 
          data = full_df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model4_controls_ls)

# List all models with controls

models_controls_ls <- list(model1_controls_ls, model2_controls_ls, model3_controls_ls, model4_controls_ls)

## Mechanism checks: controlling for life satisfaction as an explanatory variable --------------------------------------------------------

# Estimate the same models on presidential approval, but controlling for life satisfaction

# Models with controls, max temp plus life satisfaction

model1_controls_ls_cov <- 
    feglm(paste("approves_president ~ max_temperature + satisfied_life + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model1_controls_ls)

# Models with controls, min temp plus life satisfaction

model2_controls_ls_cov <- 
    feglm(paste("approves_president ~ min_temperature + satisfied_life + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model2_controls_ls)

# Models with controls, avg temp plus life satisfaction

model3_controls_ls_cov <- 
    feglm(paste("approves_president ~ avg_temperature + satisfied_life + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model3_controls_ls)

# Models with controls, min, max, and precipitation plus life satisfaction

model4_controls_ls_cov <- 
    feglm(paste("approves_president ~ min_temperature + max_temperature + precipitation + satisfied_life + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

summary(model4_controls_ls)

# List all models with controls

models_controls_ls_cov <- list(model1_controls_ls_cov, model2_controls_ls_cov, model3_controls_ls_cov, model4_controls_ls_cov)

# Clustering robustness checks ---------------------------------------------

# Present the models with clustering at the province, region and canton + interview_date 

## Validity checks ----------------------------------------------------------

# Controls and fixed effects for all variables, province clustering

modelsummary(validity_results, stars = stars, output = "markdown", vcov = ~province_dpa)

# Controls and fixed effects for all variables, region clustering

modelsummary(validity_results, stars = stars, output = "markdown", vcov = ~ region)

# Controls and fixed effects for all variables, canton + interview_date clustering

modelsummary(validity_results, stars = stars, output = "markdown", vcov = ~ canton_dpa + interview_date)

## Baseline, no controls ---------------------------------------------------

modelsummary(simple_models, 
             stars = stars, 
             output = "markdown",
             vcov = ~province_dpa)

modelsummary(simple_models,
             stars = stars,
             output = "markdown",
             vcov = ~ region)

modelsummary(simple_models,
             stars = stars,
             output = "markdown",
             vcov = ~ canton_dpa + interview_date)

## With controls -----------------------------------------------------------

modelsummary(models_controls, 
             stars = stars, 
             output = "markdown",
             vcov = ~province_dpa)

modelsummary(models_controls,
             stars = stars,
             output = "markdown",
             vcov = ~ region)

# Max temperature loses significance with region controls in this specification. 

modelsummary(models_controls,
             stars = stars,
             output = "markdown",
             vcov = ~ canton_dpa + interview_date)

## Heterogeneous effects ---------------------------------------------------

modelsummary(hetero_models, 
             stars = stars, 
             output = "markdown",
             vcov = ~province_dpa)

modelsummary(hetero_models,
             stars = stars,
             output = "markdown",
             vcov = ~ region)

modelsummary(hetero_models,
                stars = stars,
                output = "markdown",
                vcov = ~ canton_dpa + interview_date)

## Mechanism checks: LS as an explained variable ---------------------------

# LS as an explained variable, controls only, province clustering

modelsummary(models_controls_ls, 
             stars = stars, 
             output = "markdown",
             vcov = ~province_dpa)

# LS as an explained variable, controls only, region clustering

modelsummary(models_controls_ls,
             stars = stars,
             output = "markdown",
             vcov = ~ region)

# LS as an explained variable, controls only, canton + interview_date clustering

modelsummary(models_controls_ls,
             stars = stars,
             output = "markdown",
             vcov = ~ canton_dpa + interview_date)

## Mechanism checks: LS as an explanatory variable -------------------------

# LS as an explanatory variable, controls only, province clustering

modelsummary(models_controls_ls_cov, 
             stars = stars, 
             output = "markdown",
             vcov = ~province_dpa)

# LS as an explanatory variable, controls only, region clustering

modelsummary(models_controls_ls_cov,
             stars = stars,
             output = "markdown",
             vcov = ~ region)

# LS as an explanatory variable, controls only, canton + interview_date clustering

modelsummary(models_controls_ls_cov,
             stars = stars,
             output = "markdown",
             vcov = ~ canton_dpa + interview_date)

