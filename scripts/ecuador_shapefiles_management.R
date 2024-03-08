# R Script: Ecuador Metadata Download
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script downloads and processes shapefiles for cantons and other geographies in Ecuador.
# Source: INEC (https://www.ecuadorencifras.gob.ec/documentos/web-inec/Geografia_Estadistica/Micrositio_geoportal/index.html)

# Preliminaries ----------------------------------------------------------------

# Load libraries

library(sf)
library(janitor)
library(dplyr)
library(readr)

# Downloading the files ---------------------------------------------------------

# Declare download URLs

shapefile_url <- "https://www.ecuadorencifras.gob.ec//documentos/web-inec/Cartografia/Clasificador_Geografico/2012/SHP.zip"

metadata_url <- "https://www.ecuadorencifras.gob.ec//documentos/web-inec/Cartografia/Clasificador_Geografico/2012/LISTADOS.zip"

# Download the zip file containing metadata and DPA (División Política Administrativa) metadata

download.file(shapefile_url, "data/other/ecuador_dpa_shapefile.zip")

# Unzip the files --------------------------------------------------------------

# Unzip the metadata

unzip("data/other/ecuador_dpa_shapefile.zip", exdir = "data/other")

# Delete unnecessary files ------------------------------------------------------

file.remove("data/other/ecuador_dpa_shapefile.zip")

# Extracting province identifiers and names -------------------------------------

# Read the shapefile for provinces

ecuador_provinces <- 
  st_read("data/other/SHP/nxprovincias.shp")  %>% 
  st_simplify(preserveTopology = T, dTolerance = 100)  %>% 
  as_tibble()  %>% 
  clean_names()  %>%
  select(prov_id = dpa_provin, prov_name = dpa_despro) %>% 
  distinct(prov_id, .keep_all = T)

# Extracting canton identifiers and names ---------------------------------------

# Read the shapefile for cantons, join with province names and get identifier for provinces

ecuador_cantons <- 
  st_read("data/other/SHP/nxcantones.shp")  %>% 
  st_simplify(preserveTopology = T, dTolerance = 100)  %>% 
  as_tibble()  %>% 
  clean_names()  %>%
  select(canton_id = dpa_canton, canton_name = dpa_descan, prov = dpa_despro) %>% 
  distinct(canton_id, .keep_all = T) %>% 
  left_join(ecuador_provinces, by = c("prov" = "prov_name"))

# Export metadata ----------------------------------------------------

write_csv(ecuador_cantons, "data/other/ecuador_cantons.csv")