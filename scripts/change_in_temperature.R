
library(raster)
library(here)
library(dplyr)
library(sf)
library(tmap)

rm(list = ls())

proj2 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
data(World)
World <- as(World, "sf") %>% 
  sf::st_transform(proj2) %>% 
  mutate(N = 1) %>% 
  sf::st_union(by = N) %>% 
  sf::as_Spatial()

rasters <- list.files(path = here("raw_data"), pattern = "tsamax", full.names = T)

rpre <- raster(rasters) %>% 
  mean() %>% 
  rotate() %>% 
  mask(World, inverse = T)

r2050 <- raster(here("raw_data", "tsamax2050.nc")) %>% 
  mean() %>% 
  rotate() %>% 
  mask(World, inverse = T)

r <- r2050-rpre

saveRDS(r, here("data", "tsdiff.rds"))
