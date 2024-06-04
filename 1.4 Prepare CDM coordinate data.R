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

cdm <- read_excel("./01 Data processing/01 Raw data/CDM/CDM coordinates.xlsx", sheet = "CDM projects")
cdm <- cdm %>% filter(!is.na(Latitude) & is.na(Drop))

# Strip spaces
cdm <- cdm %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., " ", "")))

# Standardize the punctuation symbols
cdm <- cdm %>%
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "’", "'"))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "′", "'"))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "´", "'"))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "´", "'"))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "º", "°"))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "˚", "°"))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "”", "\""))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "″", "\""))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "’’", "\""))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "′′", "\""))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "´´", "\""))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "‟", "\""))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "\"", "\""))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "'′", "\""))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "'´", "\""))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "''", "\"")))

cdm <- cdm %>%
  mutate(across(c(Latitude, Longitude), ~if_else(str_ends(., "'"), str_replace(., "'", "'00\""), .)))

# Handle degree decimal formatted lat-longs
cdm_deg_dec <- cdm %>% filter(str_ends(Latitude, "°"))

cdm_deg_dec <- cdm_deg_dec %>%
  mutate(across(c(Latitude, Longitude), ~as.numeric(str_replace(., "°", "")))) %>% 
  mutate(start_year = year(std_start)) %>% 
  select(start_year, ref_num = cdm_reference_number, lat = Latitude, long = Longitude, num_sites = `Number of sites`)

# Handle degree-minute-second formatted lat-longs
cdm_dms <- cdm %>% filter(!str_ends(Latitude, "°"))

cdm_dms <- cdm_dms %>%
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "°", "_"))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "'", "_"))) %>% 
  mutate(across(c(Latitude, Longitude), ~str_replace_all(., "\"", ""))) 
  
# Convert degree-minute-second notation to decimal degree
convertDMS_to_decimal <- function(dms) {
  parts <- unlist(strsplit(dms, "_"))
  
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  
  decimal_degrees <- degrees + (minutes / 60) + (seconds / 3600)
  return(decimal_degrees)
}

vect_convert <- Vectorize(convertDMS_to_decimal)

cdm_dms <- cdm_dms %>% 
  mutate(lat = as.numeric(vect_convert(Latitude)),
         long = as.numeric(vect_convert(Longitude))) %>% 
  mutate(start_year = year(std_start)) %>% 
  select(start_year, ref_num = cdm_reference_number, lat, long, num_sites = `Number of sites`)

# Bind the decimal degree and DMS formatted lat-longs    
cdm_proc <- bind_rows(cdm_deg_dec, cdm_dms)

cdm_proc <- cdm_proc %>% 
  mutate(num_sites = case_when(is.na(num_sites) ~ 1,
                               TRUE ~ num_sites))

# Save out projects that started before the two most recent Economic Census (2005 and 2013)
cdm_05 <- cdm_proc %>% 
  filter(start_year <= 2005) %>% 
  select(-start_year)
write_csv(cdm_05, "./01 Data Processing/03 Temp files/cdm_05.csv")

cdm_13 <- cdm_proc %>% 
  filter(start_year <= 2013) %>% 
  select(-start_year)
write_csv(cdm_13, "./01 Data Processing/03 Temp files/cdm_13.csv")

# Save geo reference for CDM projects started in 2013 or prior
cdm_13 <- cdm_13 %>% 
  select(-ref_num, -num_sites) %>% 
  filter(long > 60)

cdm_13 <- st_as_sf(cdm_13, coords = c("long", "lat"))

dist_poly <- st_read(dsn = "./01 Data processing/01 Raw data/SHRUG/shrug-pc11dist-poly-shp/", "district")
cdm_13 <- cdm_13 %>% st_set_crs(st_crs(dist_poly))

st_write(cdm_13, dsn = "./01 Data processing/03 Temp files/", layer = "cdm_13_geo", driver = "ESRI Shapefile", append = FALSE)



