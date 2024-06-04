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
library(tictoc)
tic()

### Create district-level panels by year ###

# Create paste function to make looping through file names cleaner
`%+%` <- function(left, right) {
  paste0(left, right)
}

setwd('/Users/emmettsexton/Dropbox/Mac (2)/Desktop/BSE/Thesis/')

# SHRUG data is from https://www.devdatalab.org/shrug_download/
# Load a polygon for each shrid (SRHUG is using 2011 Population Census as a reference)
shrid_poly <- st_read(dsn = "./01 Data processing/01 Raw data/SHRUG/shrug-shrid-poly-shp/", "shrid2_open")
dist_poly <- st_read(dsn = "./01 Data processing/01 Raw data/SHRUG/shrug-pc11dist-poly-shp/", "district")

# Load Indian elevation data to extract average elevation by district later
indian_elev <- rast("./01 Data processing/03 Temp files/indian_elevation.tif")

short_or_long <- c('short', 'long')
# Use loop to create two panels for two time periods:
# 1998+2005+2013 (long) or only 2005+2013 (short)
for (panel in short_or_long){
  print(panel)
  
  if (panel == 'long'){
    years <- c('98','05','13')
    # Load SHRIDs in 1998, 2005 and 2013 Economic Censuses
    shrids <- read_csv("./01 Data processing/03 Temp files/shrids_in_98_05_13_ec.csv")
    shrids <- shrids$x
  } else{
    years <- c('05','13')
    # Load SHRIDs in 2005 and 2013 Economic Censuses
    shrids <- read_csv("./01 Data processing/03 Temp files/shrids_in_05_13_ec.csv")
    shrids <- shrids$x
  }

  # Loop through each year of the panel
  for (year in years) {
    if (year == '98'){
      full_year = '19' %+% year
    } else{
      full_year = '20' %+% year
    }
    
    print(year)
    
    # Read in Economic Census data
    ec <- read.csv('./01 Data processing/01 Raw data/SHRUG/shrug-ec' %+% year %+% '-csv/ec' %+% year %+% '_shrid.csv')
    ec <- ec %>% 
      rename(ec_emp_all = !!sym('ec'%+% year %+% '_emp_all'),
             ec_emp_manuf = !!sym('ec'%+% year %+% '_emp_manuf'),
             ec_emp_services = !!sym('ec'%+% year %+% '_emp_services'),
             ec_emp_mining = !!sym('ec'%+% year %+% '_emp_shric_4'),
             ec_emp_coke     = !!sym('ec'%+% year %+% '_emp_shric_19'),
             ec_emp_petro    = !!sym('ec'%+% year %+% '_emp_shric_20'),
             ec_emp_chemical = !!sym('ec'%+% year %+% '_emp_shric_22'),
             ec_emp_cement   = !!sym('ec'%+% year %+% '_emp_shric_24'),
             ec_emp_iron_steel = !!sym('ec'%+% year %+% '_emp_shric_25'),
             ec_emp_prec_metal = !!sym('ec'%+% year %+% '_emp_shric_26'),
             ec_emp_metal_cast = !!sym('ec'%+% year %+% '_emp_shric_27'),
             ec_emp_power = !!sym('ec'%+% year %+% '_emp_shric_33')) %>% 
      mutate(ec_heavy_industry = ec_emp_mining + ec_emp_coke + ec_emp_petro + ec_emp_chemical + ec_emp_cement + ec_emp_iron_steel + ec_emp_prec_metal + ec_emp_metal_cast + ec_emp_power)
    
    # Filter to shrids using consistent SHRIDS across years and create variables for state and district
    bal_ec_temp <- ec %>% 
      filter(shrid2 %in% shrids) %>% 
      select(shrid2, ec_emp_all, ec_emp_manuf, ec_heavy_industry, ec_emp_services) %>% 
      mutate(pc11_s_id = str_sub(shrid2, 4, 5),
             pc11_d_id = str_sub(shrid2, 7, 9))
    
    # Merge shrid polygons to the EC data using the shrid2 ID
    bal_ec <- left_join(bal_ec_temp, shrid_poly, by = "shrid2")
    sf_bal_ec <- st_as_sf(bal_ec)
    
    rm(bal_ec, bal_ec_temp, ec)
    gc()
    
    # Calculate area of each shrid and each district
    sf_use_s2(FALSE)
    sf_bal_ec <- sf_bal_ec %>% 
      mutate(shrid_area = st_area(geometry))
    dist_poly <- dist_poly %>% 
      mutate(dist_area = st_area(geometry))
    sf_use_s2(TRUE)
    
    shrid_area_emp <- sf_bal_ec %>% 
      select(shrid2, ec_emp_all, ec_emp_manuf, ec_heavy_industry, ec_emp_services, pc11_s_id, pc11_d_id, shrid_area)
    shrid_area_emp <- st_drop_geometry(shrid_area_emp)
    rm(sf_bal_ec)
    
    dist_emp <- inner_join(dist_poly, shrid_area_emp, by = c("pc11_s_id", "pc11_d_id"))
    
    # Aggregate employment total and manufacturing employment to the district level
    dist_emp <- dist_emp %>% 
      group_by(pc11_s_id, pc11_d_id, d_name, dist_area, geometry) %>% 
        summarize(emp_tot = sum(ec_emp_all, na.rm = TRUE),
                  emp_manuf = sum(ec_emp_manuf, na.rm = TRUE),
                  emp_services = sum(ec_emp_services, na.rm = TRUE),
                  emp_industry = sum(ec_heavy_industry, na.rm = TRUE),
                  aggr_shrid_area = sum(shrid_area, na.rm = TRUE)) %>% 
      ungroup()
    
    # Calculate area of each district that is covered by a shrid in the balanced panel
    dist_bal <- dist_emp %>% 
      mutate(pct_area_covered = as.numeric(aggr_shrid_area/dist_area)) %>% 
      mutate(manuf_emp_shr = emp_manuf/emp_tot,
             indus_emp_shr = emp_industry/emp_tot,
             serv_emp_shr = emp_services/emp_tot) %>% 
      mutate(ID = row_number()) %>% 
      select(-emp_tot, -emp_manuf, -emp_services, -emp_industry, -dist_area, -aggr_shrid_area)
    
    # Load pollution, wind, rain, and vegetation rasters for India 
    poll_ras <- rast('./01 Data processing/01 Raw data/NASA/Pollution/pol_' %+% full_year %+% '.tif')
    wind_ras <- rast('./01 Data processing/01 Raw data/NASA/Wind/wind_' %+% full_year %+% '.tif')
    lai_ras <- rast('./01 Data processing/01 Raw data/NASA/Rain/rain_' %+% full_year %+% '.tif')
    rain_ras <- rast('./01 Data processing/01 Raw data/NASA/Leaf area index/lai_' %+% full_year %+% '.tif')
    
    # Find mean values across raster pixels within each district polygon, using area weighted average
    avg_poll <- extract(poll_ras, dist_bal, fun = mean)  
    avg_wind <- extract(wind_ras, dist_bal, fun = mean)  
    avg_lai <- extract(lai_ras, dist_bal, fun = mean)  
    avg_rain <- extract(rain_ras, dist_bal, fun = mean)  
    
    # Merge average values on to main district-level dataframe
    dist_bal <- full_join(dist_bal, avg_poll, by = "ID")
    dist_bal <- full_join(dist_bal, avg_wind, by = "ID")
    dist_bal <- full_join(dist_bal, avg_lai, by = "ID")
    dist_bal <- full_join(dist_bal, avg_rain, by = "ID")
    
    # Extract average elevation by district
    avg_elev <- extract(indian_elev, dist_bal, fun = mean)
    avg_elev <- avg_elev %>% 
      rename(elevation = filea50979fa9fe1)
    
    # Merge average elevation back on to main district-level data
    dist_bal <- full_join(dist_bal, avg_elev, by = "ID")
    
    dist_bal <- dist_bal %>% 
      mutate(year = full_year)
    
    if (year %in% c('05', '13')){
      
      # Count the number of CDM projects across districts
      # Load CDM project coordinates
      cdm_in <- read_csv('./01 Data Processing/03 Temp files/cdm_' %+% year %+% '.csv')
      cdm_in <- cdm_in %>% 
        select(-ref_num, -num_sites)
      
      cdm <- st_as_sf(cdm_in, coords = c("long", "lat"))
      cdm <- cdm %>% st_set_crs(st_crs(dist_bal))
      
      # Set buffer of 0.2° around CDM projects 
      cdm_buff <- st_buffer(cdm, dist = 0.2)
      
      # Calculate intersections of these buffers with the district boundaries
      buff_int <- st_intersects(dist_bal, cdm_buff)
      
      int_df <- data.frame(ID = integer(0), cdm_proj = integer(0))
      for (i in 1:length(buff_int)){
        # Count number of buffer intersections for each district
        temp_df <- data.frame(ID = i, cdm_proj = length(buff_int[[i]]))
        int_df <- bind_rows(int_df, temp_df)
      }
      
      # Merge the number of CDM project buffer intersections back on for each district
      dist_bal <- full_join(int_df, dist_bal, by = "ID")
    }
    
    for (cutoff in c(1, 50, 60, 70, 80, 90)){
      print(cutoff)
      # Subset to districts that have areas that are mostly covered by the SHRIDs present in all four EC years
      dist_bal_out <- dist_bal %>% 
        filter(pct_area_covered > cutoff/100 & !is.na(pct_area_covered)) %>% 
        select(-ID) 
      
        st_write(
          dist_bal_out,
          dsn = "./01 Data processing/03 Temp files/",
          layer = panel %+% '_dist_' %+% year %+% '_bal_' %+% cutoff %+% '_pct',
          driver = "ESRI Shapefile",
          append = FALSE
        )
      rm(dist_bal_out)
    } 
    
    rm(dist_bal)
    gc()
  } # End yearly loop
} # End short or long panel loop

print(toc())