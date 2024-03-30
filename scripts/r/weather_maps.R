# R Script: Weather data NC files download
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script downloads weather data for Canton.

# Preliminaries ----------------------------------------------------------------

# Load libraries

library(sf, warn.conflicts = F)
library(terra, warn.conflicts = F)
library(dplyr, warn.conflicts = F)
library(viridis, warn.conflicts = F) # color palette for easy visualization
library(ggplot2, warn.conflicts = F)

# Load weather rasters ---------------------------------------------------------

# Max temperature

temp_max_2023 <- rast("data/weather/nc/tmax.2023.nc")

# Min temperature

temp_min_2023 <- rast("data/weather/nc/tmin.2023.nc")

# Precipitation

precip_2023 <- rast("data/weather/nc/precip.2023.nc")

# Load the canton shape file --------------------------------------------------

canton_shp <- 
    st_read("data/other/SHP/nxcantones.shp")  %>% 
    st_simplify(preserveTopology = T, dTolerance = 100) %>% 
    rename(prov_id = DPA_PROVIN) %>% 
    filter(!prov_id %in% "20") # Filter out Galápagos Islands since there are no relevant data for these canton

# Reproject the canton shapefile to match the raster's CRS

canton_shp <- st_transform(canton_shp, crs(temp_max_2023))

# Computation of averages for Ecuador ---------------------------------------

# Rotate the raster, then crop it to the canton shapefile and compute the mean

mean_tmax_ecu <- 
    temp_max_2023 %>% 
    rotate() %>% 
    crop(canton_shp, mask = T) %>% 
    app(mean)

mean_tmin_ecu <-
    temp_min_2023 %>%
    rotate() %>% 
    crop(canton_shp, mask = T) %>% 
    app(mean)

mean_precip_ecu <-
    precip_2023 %>%
    rotate() %>% 
    crop(canton_shp, mask = T) %>% 
    app(mean)

# Maps ------------------------------------------------------------------------

# Maximum temperature

plot(mean_tmax_ecu, main = "Average Maximum Temperature (°C)", axes = T, col = terrain.colors(100), legend = T)
plot(canton_shp$geometry, add = T)


# In ggplot ------------------------------------------------------------------

# Convert the raster to a data frame

mean_tmax_ecu_df <- as.data.frame(mean_tmax_ecu, xy = T)

mean_tmin_ecu_df <- as.data.frame(mean_tmin_ecu, xy = T)

mean_precip_ecu_df <- as.data.frame(mean_precip_ecu, xy = T)

# Bind all together

temperature_df <- 
    bind_rows(
        mean_tmax_ecu_df %>% mutate(type = "Max. Temperature"),
        mean_tmin_ecu_df %>% mutate(type = "Min. Temperature"),
    )

# Temperature chart

map <- 
    ggplot() +
    geom_raster(data = temperature_df, aes(x = x, y = y, fill = mean)) + 
    scale_fill_gradientn(colors = terrain.colors(100)) +
    geom_sf(data = canton_shp, color = "black", fill = "transparent") +
    facet_wrap(~type) +
    labs(title = "Average daily temperatures for 2023",
         x = "Longitude", 
         y = "Latitude", 
         fill = "Average temperatures (\u00B0C)") + 
    theme_minimal() + 
    theme(text = element_text(size = 12, family = "serif"),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          legend.position = "bottom",
          legend.direction = "horizontal",
          panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = "transparent"))

map

ggsave("figures/temperature_map.png", 
       plot = map,
       height = 10,
       width = 17,
       units = "cm",
       dpi = 800)

# Precipitation chart

ggplot() +
    geom_raster(data = mean_precip_ecu_df, aes(x = x, y = y, fill = mean)) + 
    scale_fill_gradientn(colors = terrain.colors(100)) +
    geom_sf(data = canton_shp, color = "black", fill = "transparent") +
    labs(title = "Average Precipitation (mm)")