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

# Create paste function to make looping through file names cleaner
`%+%` <- function(left, right) {
  paste0(left, right)
}

for (cutoff in c(1, 50, 60, 70, 80, 90)){
  print(cutoff)
  
  df_05 <- st_read(dsn = "./01 Data processing/03 Temp files/", "short_dist_05_bal_" %+% cutoff %+% "_pct")
  df_13 <- st_read(dsn = "./01 Data processing/03 Temp files/", "short_dist_13_bal_" %+% cutoff %+% "_pct")
  
  df_05 <- df_05 %>% 
    rename(poll = pl_2005, ws = wn_2005, lai = la_2005, rain = rn_2005)
  df_13 <- df_13 %>%
    rename(poll = pl_2013, ws = wn_2013, lai = la_2013, rain = rn_2013)
  
  bal_panel_df <- rbind(df_05, df_13)

  bal_panel_df <- bal_panel_df %>% 
    mutate(centroid = st_centroid(geometry)) %>% 
    mutate(lat = st_coordinates(centroid)[,2],
           lon = st_coordinates(centroid)[,1]) %>% 
    select(-centroid) %>% 
    filter(!is.na(rain)) %>% 
    mutate(log_poll = log(poll),
           log_ws = log(ws),
           manuf_emp = mnf_mp_*100,
           indus_emp = inds_m_*100,
           serv_emp = srv_mp_*100)
  
  # http://www.statoids.com/uin.html
  bal_panel_df <- bal_panel_df %>% 
    mutate(zone = case_when(pc11_s_ %in% c('04','07','06','02','01','03','08') ~ 'North',
                            pc11_s_ %in% c('18','12','14','17','15','13','16','11') ~ 'North East',
                            pc11_s_ %in% c('22','23','05','09') ~ 'Central',
                            pc11_s_ %in% c('10','20','21', '19') ~ 'East',
                            pc11_s_ %in% c('25','26','30','24','27') ~ 'West',
                            pc11_s_ %in% c('28','29','32','34','33') ~ 'South'))
  
  st_write(bal_panel_df, dsn = './01 Data processing/04 Output/', layer = 'short_bal_panel_df_' %+% cutoff %+% '_pct', driver = "ESRI Shapefile", append = FALSE)
  
  bal_panel_no_geo <- st_drop_geometry(bal_panel_df)
  write_csv(bal_panel_no_geo, file = './01 Data processing/04 Output/short_bal_panel_no_geo_' %+% cutoff %+% '_pct.csv')
}

