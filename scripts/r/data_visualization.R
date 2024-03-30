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
library(tidyr)
library(ggplot2)
library(patchwork)
library(lubridate)
library(scales)
library(haven)
library(survey)

# Load the full dataset

load("data/full_df.RData")

# Load weather data on its own

temperature_min <- read_csv("data/weather/min_temperature.csv")

temperature_max <- read_csv("data/weather/max_temperature.csv")

precipitation <- read_csv("data/weather/precipitation.csv")

cantons_df <- read_csv("data/other/ecuador_cantons.csv")

# Quick data preparation for the weather data --------------------------------

temperature_df <- 
  temperature_min %>% 
  rename(min_temperature = value) %>%
  left_join(temperature_max %>% select(date, canton_id, max_temperature = value), by = c("date", "canton_id")) %>% 
  mutate(avg_temp = (min_temperature + max_temperature) / 2) %>% 
  left_join(cantons_df, by = "canton_id")

precipitation_df <- 
  precipitation %>% 
  rename(precipitation = value) %>% 
  left_join(cantons_df, by = "canton_id")

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
 labs(x = "Survey Interview Date",
      y = "Count of Responses") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6, color = "black"),
        text = element_text(color = "black"),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(fill = NA, linewidth = 1, linetype = "solid"),
        plot.caption = element_text(hjust = 0),
        panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.3),
        panel.grid.minor.y = element_line(linetype = "dashed", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey80", linewidth = 1)
  )

interview_dates_barchart

ggsave("figures/interview_dates_barchart.png", 
        interview_dates_barchart, 
        width = 17, 
        height = 8,
        units = "cm", 
        dpi = 800)

# Temperature visualization --------------------------------------------------

# Get the overall sd for the temperatures

temperature_sd <- 
  temperature_df %>% 
  summarise(sd_avg_temp = sd(avg_temp, na.rm = T),
            sd_min_temp = sd(min_temperature, na.rm = T),
            sd_max_temp = sd(max_temperature, na.rm = T)) %>% 
  data.frame()
  
# Monthly mean temperatures, all country (Polar coordinates)

ecuador_monthly_mean_temps <-
  temperature_df %>% 
  group_by(month = month(date, label = T)) %>% 
  summarise(avg_temp = mean(avg_temp, na.rm = T),
            min_temp = mean(min_temperature, na.rm = T),
            max_temp = mean(max_temperature, na.rm = T),
            sd_avg_temp = sd(avg_temp, na.rm = T),
            sd_min_temp = sd(min_temperature, na.rm = T),
            sd_max_temp = sd(max_temperature, na.rm = T)) %>%
  pivot_longer(cols = c(avg_temp, min_temp, max_temp, sd_avg_temp, sd_min_temp, sd_max_temp), 
               names_to = "temperature_type", 
               values_to = "value") %>% 
  mutate(base = case_when(
    temperature_type == "avg_temp" ~ 19.5,
    temperature_type == "min_temp" ~ 12.5,
    temperature_type == "max_temp" ~ 23
  )) %>%
  arrange(temperature_type, month)

ecuador_monthly_mean_temps %>% 
  filter(temperature_type %in% c("avg_temp", "min_temp", "max_temp")) %>%
  ggplot(aes(x = as.factor(month), y = value - base, group = as.factor(month))) +
  geom_col() +
  #facet_wrap(~temperature_type) +
  facet_grid(cols = vars(temperature_type)) +   
  labs(title = "Monthly Average Temperature for Ecuador",
       x = "Month",
       y = "Average Temperature (C)")  + 
  coord_polar()

# Standard deviation of the monthly mean temperatures, all country (Polar coordinates)

ecuador_monthly_mean_temps %>% 
  filter(temperature_type %in% c("sd_min_temp", "sd_max_temp")) %>%
  ggplot(aes(x = as.factor(month), y = value, group = as.factor(month))) +
  geom_col() +
  facet_wrap(~temperature_type) +
  labs(title = "Standard Deviation of Monthly Average Temperature for Ecuador",
       x = "Month",
       y = "Standard Deviation (C)")  + 
  coord_polar()

# Do a time series of mean temperatures at the national level

ecuador_monthly_mean_temps <-
  temperature_df %>%
  mutate(month_year = floor_date(date, "month")) %>%
  group_by(month_year) %>% 
  summarise(`Minimum` = mean(min_temperature, na.rm = T),
            `Maximum` = mean(max_temperature, na.rm = T)) %>% 
  pivot_longer(cols = c(`Minimum`, `Maximum`), 
               names_to = "temperature_type", 
               values_to = "value")

ecuador_monthly_mean_temps_fig <-
  ecuador_monthly_mean_temps %>% 
  ggplot(aes(x = month_year, y = value, color = temperature_type)) +
  geom_line() +
  labs(x = "",
       y = "Temperature (\u00B0C)",
       color = "Type of temperature") +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "6 months",
               expand = c(0,0), 
               limits = c(as.Date("2008-01-01"), as.Date("2024-07-01"))) +
  scale_y_continuous(breaks = seq(12, 28, by = 1),
                     labels = comma) +
  scale_color_manual(values = c("#E60F2D", "#56589e")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        text = element_text(family = 'serif', color = "black", size = 8),
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
        plot.caption = element_text(hjust = 0),
        panel.grid.major = element_line(linetype = "dashed", linewidth = 0.3),
        panel.grid.minor = element_line(linetype = "dashed", linewidth = 0.3),
        strip.background = element_rect(fill = "grey80", colour = "black", linewidth = 1),
        legend.position = c(0.9, 0.5)
  )

ecuador_monthly_mean_temps_fig

# Survey-robust data viz ----------------------------------------------------

# Presidencial approval rating ------------------------------------------------

# Using the approval rating variable and the survey design weights, calculate the mean approval rating for each wave

# Create the survey design 

df_des <-
  df %>%
  filter(!is.na(upm), !is.na(estratopri))

ab_des <- svydesign(ids = ~ upm,
                    strata = ~ estratopri,
                    weights = ~ weight1500, 
                    data = df_des,
                    nest = T,
                    na.action = "na.exclude")

# Approval rating by year

approval_rating_by_year <-
  svyby(~ approves_president, ~ year, design = ab_des, svymean, na.rm = T)

# Create the plot with the second column (se) as the 95% confidence interval error bars

approval_rating_by_year_fig <- 
  approval_rating_by_year %>% 
  ggplot(aes(x = year, 
             y = approves_president, 
             ymin = approves_president - 1.96 * se, 
             ymax = approves_president + 1.96 * se)) +
  geom_point() + 
  geom_errorbar(width = 0.5) +
  geom_line(linetype = "dotted") + 
  scale_x_continuous(breaks = seq(2008, 2023, by = 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Year",
       y = "Approval Rating",
       caption = "Note: Error bars represent 95% confidence intervals adjusted for survey design effects.") +
  theme_bw() +
  annotate("rect", xmin = 2007.5, xmax = 2017, ymin = 0.05, ymax = 0.8, fill = "green", alpha = 0.2) +
  annotate("label", x= 2013, y = 0.55, label = "Rafael Correa tenure", color = "black", size = 3) +
  annotate("rect", xmin = 2017, xmax = 2020.5, ymin = 0.05, ymax = 0.8, fill = "gray60", alpha = 0.2) +
  annotate("label", x= 2018.8, y = 0.55, label = "Lenín Moreno\ntenure", color = "black", size = 3) +
  annotate("rect", xmin = 2020.5, xmax = 2023.5, ymin = 0.05, ymax = 0.8, fill = "blue", alpha = 0.2) +
  annotate("label", x= 2022, y = 0.7, label = "Guillermo Lasso\ntenure", color = "black", size = 3) +
  theme(axis.text.x = element_text(color = "black"),
        text = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
        plot.caption = element_text(hjust = 0),
        panel.grid.major = element_line(linetype = "dashed", linewidth = 0.3),
        panel.grid.minor = element_line(linetype = "dashed", linewidth = 0.3),
        strip.background = element_rect(fill = "grey80", colour = "black", linewidth = 1)
  )

approval_rating_by_year_fig

ggsave("figures/approval_rating_by_year.jpg", 
        plot = approval_rating_by_year_fig, 
        width = 17, 
        height = 8,
        units = "cm", 
        dpi = 800)

# Exporting relevant figures ------------------------------------------------

# Interview dates barchart (facets)

ggsave("figures/interview_dates_barchart.png", 
        interview_dates_barchart, 
        width = 17, 
        height = 10,
        units = "cm", 
        dpi = 800)

# Temperature time series (national level)

ggsave("figures/ecuador_monthly_mean_temps_fig.png", 
        ecuador_monthly_mean_temps_fig, 
        width = 17, 
        height = 10,
        units = "cm", 
        dpi = 800)