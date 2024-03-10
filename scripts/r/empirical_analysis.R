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

# Load the full dataset

df <- read_csv("data/full_df.csv", show_col_types = FALSE)

# Descriptive statistics --------------------------------------------------



# Empirical models ---------------------------------------------------------
