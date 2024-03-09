# R Script: Data Visualization
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script performs data visualization for the POL837 term research paper.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(lubridate)

# Load the full dataset

df <- read_csv("data/full_df.csv", show_col_types = FALSE)

# Survey responses --------------------------------------------------------

# Plotting survey responses through time

df %>% 
  group_by(interview_date, wave) %>% 
  summarise(count_of_responses = n()) %>%
  ggplot(aes(x = interview_date, y = count_of_responses, fill = wave)) +
  geom_col(color = "black") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")