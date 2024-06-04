rm(list = ls())
library(tidyr)
library(dplyr)
library(readr)
library(fixest)
library(modelsummary)
library(kableExtra)
library(gt)
library(sf)
library(ggplot2)

setwd('/Users/emmettsexton/Dropbox/Mac (2)/Desktop/BSE/Thesis/')

# Load PC consumption and poverty rates
rural <- read_csv('./01 Data processing/01 Raw data/SHRUG/shrug-secc-cons-rural-csv/secc_cons_rural_pc11dist.csv')
urban <- read_csv('./01 Data processing/01 Raw data/SHRUG/shrug-secc-cons-urban-csv/secc_cons_urban_pc11dist.csv')

rural <- rural %>% 
  select(pc11_s_ = pc11_state_id, 
         pc11_d_ = pc11_district_id,
         rural_pov_rate = secc_pov_rate_rural)
urban <- urban %>% 
  select(pc11_s_ = pc11_state_id, 
         pc11_d_ = pc11_district_id,
         urban_pov_rate = secc_pov_rate_urban)

df_short_50 <- read_csv("./01 Data processing/04 Output/short_bal_panel_no_geo_50_pct.csv")

df <- df_short_50 %>% 
  filter(year == "2013")

df <- left_join(df, rural, by = c('pc11_s_', 'pc11_d_'))
df <- left_join(df, urban, by = c('pc11_s_', 'pc11_d_'))

summ_pct <- df %>% 
  group_by(zone) %>% 
  select(zone, rain, lai, elevatn, poll, ws, manuf_emp, serv_emp, indus_emp, rural_pov_rate, urban_pov_rate) %>% 
  summarise_all(mean, na.rm =TRUE) %>% 
  ungroup() %>% 
  mutate(poll = poll*1000000000,
         rain = rain*1000)

df_long <- summ_pct %>%
  pivot_longer(-zone, names_to = "Variable", values_to = "value") %>%
  pivot_wider(names_from = zone, values_from = value)

write_csv(df_long, './01 Data Processing/04 Output/Zone summary stats.csv')
