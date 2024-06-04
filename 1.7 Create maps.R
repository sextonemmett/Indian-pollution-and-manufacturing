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
library(ggspatial)
library(showtext)


setwd('/Users/emmettsexton/Dropbox/Mac (2)/Desktop/BSE/Thesis/')

# Load balanced panel of districts for 2005 and 2013 EC
short_bal_panel_df <- st_read(dsn = "./01 Data processing/04 Output/", layer = "short_bal_panel_df_50_pct")

df_2013 <- short_bal_panel_df %>% 
  filter(year == 2013) %>% 
  mutate(`Average Wind Speed` = ws,
         `Average Black Carbon` = poll*1000000000) %>% 
  rename(`Manufacturing Employment Share` = mnf_mp_)

# Load all districts
dist_poly <- st_read(dsn = "./01 Data processing/01 Raw data/SHRUG/shrug-pc11dist-poly-shp/", "district")

# Create sf object that has all districts, with NAs for those not in the balanced panel
# This is so I create a map of all of India even though I plan to run the regressions on a subset of districts
dist_not_in_panel <- dist_poly$pc11_d_id[!(dist_poly$pc11_d_id %in% df_2013$pc11_d_)]
not_in_panel <- dist_poly %>% 
  filter(pc11_d_id %in% dist_not_in_panel)

all_dist <- bind_rows(df_2013, not_in_panel)

# Load CDM coordinates
cdm <- st_read(dsn = "./01 Data processing/03 Temp files/", layer = "cdm_13_geo")

font_add_google("EB Garamond")
showtext_auto()

# Map manufacturing shares and CDM projects 
manuf_map <- ggplot() +
  geom_sf(data = all_dist, aes(fill = `Manufacturing Employment Share`)) +
  geom_sf(data = cdm, color = "green", size = 1) +
  scale_fill_viridis_c(option = "plasma") +
  labs(fill = "% of non-agricultural employment",
       title = "FIGURE III.
Manufacturing employment share by Indian district in 2013",
       subtitle = "(Location of CDM projects represented by green circles)",
       caption = "
Note: 
Data only shown for districts in the Full Sample.")+
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 0, size = 9),
        plot.title = element_text(hjust = 0.5, size = 11),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        legend.position = c(0.8, 0.2),
        legend.title = element_text(size = 10),
        text = element_text(family = "EB Garamond"))

manuf_map

# Map wind speed
wind_map <- ggplot(data = all_dist) +
  geom_sf(aes(fill = `Average Wind Speed`)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(fill = "Meters/second",
       title = "FIGURE I.
Average wind speed by Indian district in 2013", 
       caption = "
Note: 
Data only shown for districts in the Full Sample.")+
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 0, size = 9),
        plot.title = element_text(hjust = 0.5, size = 11),
        legend.position = c(0.7, 0.2),
        legend.title = element_text(size = 10),
        text = element_text(family = "EB Garamond"))

wind_map

# Map pollution levels
poll_map <- ggplot(data = all_dist) +
  geom_sf(aes(fill = `Average Black Carbon`)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(fill = "μg/m^3",
       title = "FIGURE II.
Average black carbon by Indian district in 2013", 
       caption = "
Note: 
Data only shown for districts in the Full Sample.")+
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 0, size = 9),
        plot.title = element_text(hjust = 0.5, size = 11),
        legend.position = c(0.7, 0.2),
        legend.title = element_text(size = 10),
        text = element_text(family = "EB Garamond"))

poll_map

