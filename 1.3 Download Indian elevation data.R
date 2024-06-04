rm(list = ls())
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(readxl)
library(stringr)
library(lubridate)
library(purrr)
library(fuzzyjoin)
library(expss)
library(sf)
library(terra)
library(lwgeom)
library(elevatr)

setwd('/Users/emmettsexton/Dropbox/Mac (2)/Desktop/BSE/Thesis/')

# Download elevation raster
india_poly <- st_read(dsn = "./01 Data processing/01 Raw data/Indian administrative maps", "gadm41_IND_0")

india_elev <- get_elev_raster(india_poly, z = 7)
india_elev_ras <- rast(india_elev)

writeRaster(india_elev_ras, "./01 Data processing/03 Temp files/indian_elevation.tif", filetype = "GTiff", overwrite = TRUE)

