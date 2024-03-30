# R Script: Canton weather data preparation
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script downloads weather data for Canton.

# Preliminaries ----------------------------------------------------------------

# Load libraries
library(dplyr, warn.conflicts = F)

# Set a larger timeout options

options(timeout = 300)

# Download 2023 weather rasters ------------------------------------------------

url <- c("https://downloads.psl.noaa.gov//Datasets/cpc_global_temp/tmax.2023.nc",
         "https://downloads.psl.noaa.gov//Datasets/cpc_global_temp/tmin.2023.nc",
         "https://downloads.psl.noaa.gov//Datasets/cpc_global_precip/precip.2023.nc")

file_paths <- file.path("data/weather/nc", basename(url))

# Download the files

mapply(download.file, url, file_paths, mode = "wb")
