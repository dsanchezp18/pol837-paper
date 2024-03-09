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

load("data/full_df.RData")

# Survey responses --------------------------------------------------------

# Plotting survey responses through time

df %>% 
  group_by(interview_date, wave) %>% 
  summarise(count_of_responses = n()) %>%
  ggplot(aes(x = interview_date, y = count_of_responses, fill = as.factor(wave))) +
  geom_col() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") + 
  scale_color_discrete(palette = "Set4") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Same but facetting for the waves (free scales)

interview_dates_barchart <-
  df %>% 
  filter(!is.na(interview_date), 
         !(interview_date %>% between(as.Date("2008-07-10"), as.Date("2008-12-12")))) %>% 
  group_by(interview_date, wave) %>% 
  summarise(count_of_responses = n()) %>%
  ggplot(aes(x = interview_date, y = count_of_responses)) +
  geom_col(fill = "forestgreen") +
  geom_vline(xintercept = as.Date("2008-01-01"), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.Date("2009-01-01"), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.Date("2012-01-01"), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.Date("2017-01-02"), linetype = "dashed", color = "blue", linewidth = 0.1) +
  facet_wrap(~wave, nrow = 2, scales = "free") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
  scale_color_discrete(palette = "Set4") +
 labs(x = "Survey Interview Date",
      y = "Count of Responses") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6, color = "black"),
        text = element_text(family = 'serif', color = "black"),
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
        plot.caption = element_text(hjust = 0),
        panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.3),
        panel.grid.minor.y = element_line(linetype = "dashed", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey80", colour = "black", linewidth = 1)
  )

interview_dates_barchart

# Exporting relevant figures ------------------------------------------------

# Interview dates barchart (facets)

ggsave("figures/interview_dates_barchart.png", 
        interview_dates_barchart, 
        width = 17, 
        height = 10,
        units = "cm", 
        dpi = 800)