# R Script: Tables and Model Plots for the Paper
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script prepares the tables and model plots for the paper text

# Preliminaries -----------------------------------------------------------

# Load libraries

library(readr)
library(dplyr)
library(ggplot2)
library(fixest)
library(forcats)
library(modelsummary)
library(haven)
library(forcats)
library(kableExtra)
library(marginaleffects)

# Load the full dataset

load("data/full_df.RData")

# Redefine the full dataset

full_df <- df

# The "main" dataset for the analysis is the full dataset without the 2021 observations
 
df <- 
    full_df %>%
    filter(year != 2021)

# Modelsummary parameters -------------------------------------------------

# Set the modelsummary parameters for the tables

# Values for p-value stars 

stars <- c("*" = 0.1,
           "**" = 0.05,
           "***" = 0.01)

# Goodness of fit mappings for tables (N, AIC, RMSE, FE)

gf_map <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 0),
  list("raw" = "rmse", "clean" = "RMSE", "fmt" = 3),
  list("raw" = "FE: canton_dpa", "clean" = "Canton fixed effects", "fmt" = 0),
  list("raw" = "FE: interview_date", "clean" = "Interview date fixed effects", "fmt" = 0)
)
# Descriptive statistics --------------------------------------------------

## Normal descriptive statistics --------------------------------------------------

# Normal descriptive statistics for the merged dataset

df_descriptive <- 
  df %>%
  transmute(`Presidential approval` = approves_president,
            `Daily minimum temperature (C)` = min_temperature, 
            `Daily maximum temperature (C)` = max_temperature, 
            `Daily average temperature (C)` = avg_temperature, 
            `Daily precipitation (mm)` = precipitation,
            `Female` = sex,
            `Age (years)` = age,
            `Rural status` = urban_rural,
            `Education` = education,
            `Labour market status` = labour_market,
            `Worse perception of personal economy` = personal_econ_situation,
            `Worse perception of country economy` = country_econ_situation,
            `Ideology score (0-10)` = ideology,
            `Support of democracy` = democracy_support,
            `Perception of corruption` = corruption_perception,
            `Tolerance to bribes` = corruption_tolerance,
            `Political pride score` = political_pride,
            `Trust in police score (0-7)` = confidence_police,
            `Trust in local government score (0-7)` = confidence_local_gov,
  )

notes_descriptive <- list(
  "Note: Descriptive statistics for variables used in the empirical analysis. For categorical variables, the percent of observations in the category out of the total sample is presented. For numerical (either ordinal or continuous) variables, the mean, standard deviation, minimum and maximum are presented. For both, the number of observations and the percentage of missing values."
)

datasummary((Education + Female + `Labour market status` + `Worse perception of personal economy` + `Worse perception of country economy` + `Perception of corruption` + `Tolerance to bribes` + All(df_descriptive)) ~ ((N) + Percent() + (`Missing (%)` = PercentMissing) + Mean + (`Std. dev.` = SD) + Min + Median + Max + Percent()),
            data = df_descriptive,
            fmt = 2,
            output = "data/output/descriptive_statistics.md",
            booktabs = TRUE,
            title = "Descriptive statistics for the matched AB data and weather variables",
            threeparttable = TRUE,
            notes = notes_descriptive) 

# Baseline models ---------------------------------------------------------

# Simple Model 1: Min temperature

simple_model1 <- 
    feglm(approves_president ~ min_temperature,
          fixef = c("canton_dpa", "interview_date"),
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

# Simple Model 2: Max temperature

simple_model2 <- 
    feglm(approves_president ~ max_temperature,
          fixef = c("canton_dpa", "interview_date"),
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

# Simple Model 3: Average temperature

simple_model3 <- 
    feglm(approves_president ~ avg_temperature,
          fixef = c("canton_dpa", "interview_date"),
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

# Simple Model 4: Min and Max temperature, with precipitation

simple_model4 <- 
    feglm(approves_president ~ min_temperature + max_temperature + precipitation,
          fixef = c("canton_dpa", "interview_date"),
          data = full_df,
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

# List of baseline models

simple_models <- list(simple_model1, simple_model2, simple_model3, simple_model4)

# Average marginal effects for the baseline models

apes_baseline <- lapply(simple_models, avg_slopes, type = "response")

# Define coefficients mapping

coefficients_baseline_models <- c(
  "min_temperature" = "Min. temperature (\u00B0C)",
  "max_temperature" = "Max. temperature (\u00B0C)",
  "avg_temperature" = "Avg. temperature (\u00B0C)",
  "precipitation" = "Precipitation (mm)")

# Notes for logit coefficients table

notes_baseline_models <- list(
  "Baseline models explaining presidential approval through daily weather variables and canton and interview date fixed effects. Standard errors shown in parentheses are clustered by canton.",
  "`***`p < 0.01, `**`p < 0.05, `*` p < 0.1."
)

# Modelsummary table of coefficients (to markdown file)

modelsummary(simple_models,
             coef_map = coefficients_baseline_models,
             stars = stars,
             output = "data/output/logit_coefficients_baseline.md",
             estimate = "{estimate}{stars}",
             booktabs = T,
             align = "lrrrr", 
             threeparttable = T,
             title = "Logit coefficients for baseline specifications",
             vcov = ~ canton_dpa,
             notes = notes_baseline_models,
             gof_map = gf_map)

# Notes for marginal effects table

notes_baseline_models_ame <- list(
  "Average partial effects for baseline models explaining presidential approval through daily weather variables and canton and interview date fixed effects. Standard errors shown in parentheses are clustered by canton.",
  "`***` p < 0.01, `**` p < 0.05, `*` p < 0.1.")

# Modelsummary table of marginal effects (to markdown file)

modelsummary(apes_baseline,
             output = "data/output/marginal_effects_baseline.md",
             coef_map = coefficients_baseline_models,
             stars = stars,
             booktabs = TRUE,
             align = "lrrrr",
             estimate = "{estimate}{stars}",
             title = "Average marginal effects for baseline models",
             threeparttable = TRUE,
             notes = notes_baseline_models_ame,
             gof_map = gf_map)

# Modelplot of marginal effects for the baseline models

# Rename the objects within the list apes_baseline for better labeling in the legend

names(apes_baseline) <- c("Min. temperature",
                          "Max. temperature",
                          "Avg. temperature",
                          "Temperatures and precipitation")

model_plot_baseline <-
    modelplot(apes_baseline,
              coef_map = coefficients_baseline_models,
              fatten = 1.5,
              size = 0.5) +
    theme_classic() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    labs(y = "",
         x = "Average marginal effect and 95% CI",
         color = "Model") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(color = guide_legend(nrow = 1))

# Save the model plot

ggsave("figures/model_plot_baseline.jpg", model_plot_baseline, width = 21, height = 5.5, units = "cm", dpi = 800)

# Models with controls ----------------------------------------------------

controls_formula <- 
    "sex + age + ethnicity + urban_rural + non_religious + education + civil_status + labour_market + country_econ_situation + personal_econ_situation + incumbent_vote + ideology + corruption_perception + corruption_tolerance + democracy_support + political_pride + confidence_police + confidence_police + confidence_local_gov + external_efficacy + internal_efficacy"

model1_controls <- 
    feglm(paste("approves_president ~ min_temperature +", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

model2_controls <- 
    feglm(paste("approves_president ~ max_temperature + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

model3_controls <- 
    feglm(paste("approves_president ~ avg_temperature + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

model4_controls <- 
    feglm(paste("approves_president ~ min_temperature + max_temperature + precipitation + ", controls_formula) %>% as.formula(), 
          data = df,
          fixef = c("canton_dpa", "interview_date"),
          family = binomial(link = "logit"),
          cluster = ~ canton_dpa)

models_controls <- list(model1_controls, model2_controls, model3_controls, model4_controls)

# Average marginal effects for the models with controls

apes_controls <- lapply(models_controls, avg_slopes, type = "response")

# Define coefficients mapping

coefficients_controls <- c(
  "min_temperature" = "Min. temperature (\u00B0C)",
  "max_temperature" = "Max. temperature (\u00B0C)",
  "avg_temperature" = "Avg. temperature (\u00B0C)",
  "precipitation" = "Precipitation (mm)",
  "sexFemale" = "Female",
  "age" = "Age",
  "ethnicityBlanca" = "White (ref. Mestizo)",
  "ethnicityIndígena" = "Indigenous",
  "ethnicityNegra" = "Black",
  "ethnicityMulata" = "Mulatto",
  "ethnicityOtra" = "Other ethnicity",
  "urban_ruralRural" = "Rural area",
  "non_religiousReligious" = "Religious",
  "civil_statusMarried/Common Law" = "Married (ref. single)",
  "civil_statusDivorced/Separated/Widowed" = "Divorced/Separated/Widowed",
  "educationPrimary" = "Primary education (ref. No education)",
  "educationSecondary" = "Secondary education",
  "educationSuperior" = "Higher education",
  "labour_marketNot in Labour Force" = "Not in Labour Force",
  "labour_marketUnemployed" = "Unemployed",
  "personal_econ_situationWorse" = "Perceived worse personal economy",
  "country_econ_situationWorse" = "Perceived worse country economy",
  "incumbent_voteIncumbent" = "Voted for incumbent",
  "ideology" = "Ideology score (0-10)",
  "democracy_supportSupports" = "Supports democracy",
  "political_pride" = "Political pride score (0-7)",
  "external_efficacy" = "External efficacy score (0-7)",
  "internal_efficacy" = "Internal efficacy score (0-7)",
  "corruption_perceptionCorrupt" = "Perceives corruption",
  "corruption_toleranceTolerant" = "Tolerates bribes",
  "confidence_police" = "Trust in police score (0-7)",
  "confidence_local_gov" = "Trust in local gov. (0-7)"
)

# Notes for logit coefficients table

notes_control_models <- list(
  "Models explaining presidential approval through daily weather variables and controls. Standard errors shown in parentheses are clustered by canton.",
  "`***` p < 0.01, `**` p < 0.05, `*` p < 0.1."
)

# Modelsummary table of coefficients (to markdown file)

modelsummary(models_controls,
             gof_map = gf_map,
             coef_map = coefficients_controls,
             stars = stars,
             estimate = "{estimate}{stars}",
             booktabs = TRUE,
             align = "lrrrr", 
             threeparttable = TRUE,
             title = "Logit coefficients for specifications with controls",
             vcov = ~ canton_dpa,
             notes = notes_control_models,
             output = "data/output/logit_coefficients_controls.md")

# Notes for marginal effects table

notes_control_models_apes <- list(
  "Average partial effects for models explaining presidential approval through daily weather variables, canton and interview date fixed effects, and political behaviour controls. Standard errors shown in parentheses are clustered by canton.",
  "`***` p < 0.01, `**` p < 0.05, `* p < 0.1."
)

# Coefficient mapping for marginal effects table

coefficients_controls_apes <- c(
  "min_temperature dY/dX" = "Min. temperature (\u00B0C)",
  "max_temperature dY/dX" = "Max. temperature (\u00B0C)",
  "avg_temperature dY/dX" = "Avg. temperature (\u00B0C)",
  "precipitation dY/dX" = "Precipitation (mm)",
  "sex Female - Male" = "Female",
  "age dY/dX" = "Age",
  "ethnicity Blanca - Mestizo" = "White (ref. Mestizo)",
  "ethnicity Indígena - Mestizo" = "Indigenous",
  "ethnicity Mulata - Mestizo" = "Mulatto",
  "ethnicity Negra - Mestizo" = "Black",
  "ethnicity Otra - Mestizo" = "Other",
  "civil_status Married/Common Law - Single" = "Married (ref. Single)",
  "civil_status Divorced/Separated/Widowed - Single" = "Divorced/Separated/Widowed",
  "urban_rural Rural - Urban" = "Rural area ",
  "education Primary - None" = "Primary education (ref. No education)",
  "education Secondary - None" = "Secondary education",
  "education Superior - None" = "Higher education",
  "labour_market Not in Labour Force - Employed" = "Not in Labour Force",
  "labour_market Unemployed - Employed" = "Unemployed",
  "personal_econ_situation Worse - Better or Same" = "Perceived worse personal economy",
  "country_econ_situation Worse - Better or Same" = "Perceived worse country economy",
  "incumbent_vote Incumbent - Did not vote for incumbent" = "Voted for incumbent",
  "ideology dY/dX" = "Ideology score (0-10)",
  "internal_efficacy dY/dX" = "Internal efficacy score (0-7)",
  "external_efficacy dY/dX" = "External efficacy score (0-7)",
  "democracy_support Supports - Does Not Support" = "Supports democracy",
  "political_pride dY/dX" = "Political pride score (0-7)",
  "corruption_perception Corrupt - Not Corrupt" = "Perceives corruption",
  "corruption_tolerance Tolerant - Not Tolerant" = "Tolerates bribes",
  "confidence_police dY/dX" = "Trust in police score (0-7)",
  "confidence_local_gov dY/dX" = "Trust in local gov. (0-7)"
)

# Modelsummary table of marginal effects (for display)

modelsummary(apes_controls,
             output = "default",
             coef_map = coefficients_controls_apes,
             stars = stars,
             align = "lrrrr",
             shape = term:contrast + statistic ~ model,
             estimate = "{estimate}{stars}",
             title = "Average marginal effects for models with controls",
             threeparttable = TRUE,
             booktabs = TRUE,
             notes = notes_control_models_apes,
             gof_map = gf_map)

# Modelsummary table of marginal effects (to markdown file)

modelsummary(apes_controls,
             output = "data/output/logit_marginal_effects_controls.md",
             coef_map = coefficients_controls_apes,
             stars = stars,
             align = "lrrrr",
             shape = term:contrast + statistic ~ model,
             estimate = "{estimate}{stars}",
             title = "Average marginal effects for models with controls",
             threeparttable = TRUE,
             booktabs = TRUE,
             notes = notes_control_models_apes,
             gof_map = gf_map)

# Modelplot of marginal effects

# Rename the model objects in the list 

names(apes_controls) <- c("Min. temperature",
                          "Max. temperature",
                          "Avg. temperature",
                          "Temperatures and precipitation")

# Coefficient map

coefficients_controls_apes <- c(
  "min_temperature" = "Min. temperature (\u00B0C)",
  "max_temperature" = "Max. temperature (\u00B0C)",
  "avg_temperature" = "Avg. temperature (\u00B0C)",
  "precipitation" = "Precipitation (mm)",
  "sex" = "Sex",
  "age" = "Age",
  "ethnicity" = "Ethnicity",
  "civil_status" = "Civil status",
  "urban_rural" = "Urban/rural",
  "education" = "Education",
  "labour_market" = "Labour market",
  "personal_econ_situation" = "Personal econ. situation",
  "country_econ_situation" = "Country econ. situation",
  "incumbent_vote" = "Incumbent vote",
  "ideology" = "Ideology score (0-10)",
  "internal_efficacy" = "Internal efficacy score (0-7)",
  "external_efficacy" = "External efficacy score (0-7)",
  "democracy_support" = "Democracy support",
  "political_pride" = "Political pride score (0-7)",
  "corruption_perception" = "Corruption perception",
  "corruption_tolerance" = "Corruption tolerance",
  "confidence_police" = "Confidence in police",
  "confidence_local_gov" = "Confidence in local gov."
)

# Grouping map

grouping_map <-
  c("dY/dX" = "Effect",
    "Female - Male" = "Female",
    "Blanca - Mestizo" = "White (ref. Mestizo)",
    "Indígena - Mestizo" = "Indigenous",
    "Mulata - Mestizo" = "Mulatto",
    "Negra - Mestizo" = "Black",
    "Otra - Mestizo" = "Other",
    "Married/Common Law - Single" = "Married (ref. Single)",
    "Divorced/Separated/Widowed - Single" = "Divorced/Separated/Widowed",
    "Religious - Not religious/Agnostic" = "Religious",
    "Rural - Urban" = "Rural area",
    "Primary - None" = "Primary education (ref. No education)",
    "Secondary - None" = "Secondary education",
    "Superior - None" = "Higher education",
    "Not in Labour Force - Employed" = "Not in Labour Force",
    "Unemployed - Employed" = "Unemployed",
    "Worse - Better or Same" = "Worse perception",
    "Incumbent - Did not vote for incumbent" = "Voted for incumbent",
    "Supports - Does Not Support" = "Supports democracy",
    "Corrupt - Not Corrupt" = "Perceives corruption",
    "Tolerant - Not Tolerant" = "Tolerates bribes"
  )

modelsummary(apes_controls,
             shape = term ~ contrast + statistic ~ model,
             coef_map = coefficients_controls_apes,
             group_map = grouping_map)

# Modelplot coef map

coefficients_controls_apes_map <-c(
"precipitation" = "Precipitation (mm)",
"sex" = "Sex",
"age" = "Age",
"urban_rural" = "Urban/rural",
"personal_econ_situation" = "Personal econ. situation",
"country_econ_situation" = "Country econ. situation",
"incumbent_vote" = "Incumbent vote",
"ideology" = "Ideology score (0-10)",
"internal_efficacy" = "Internal efficacy score (0-7)",
"external_efficacy" = "External efficacy score (0-7)",
"democracy_support" = "Democracy support",
"political_pride" = "Political pride score (0-7)",
"corruption_perception" = "Corruption perception",
"corruption_tolerance" = "Corruption tolerance",
"confidence_police" = "Confidence in police",
"confidence_local_gov" = "Confidence in local gov.",
"min_temperature" = "Min. temperature (\u00B0C)",
"max_temperature" = "Max. temperature (\u00B0C)",
"avg_temperature" = "Avg. temperature (\u00B0C)"
)

# Modelplot of marginal effects

modelplot_controls <- 
  modelplot(apes_controls, 
            coef_map = coefficients_controls_apes_map,
            fatten = 1.5,
            size = 0.5) +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  labs(y = "",
       x = "Average marginal effect and 95% CI",
       color = "Model") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size = 15)) +
  guides(color = guide_legend(nrow = 1)) 

modelplot_controls

ggsave("figures/logit_marginal_effects_controls.jpg", modelplot_controls, width = 12, height = 5, dpi = 800)