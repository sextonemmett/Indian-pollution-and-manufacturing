rm(list = ls())
library(tidyr)
library(dplyr)
library(readr)
library(fixest)
library(modelsummary)
library(kableExtra)
library(gt)
library(car)

setwd('/Users/emmettsexton/Dropbox/Mac (2)/Desktop/BSE/Thesis/')

df_short_50 <- read_csv("./01 Data processing/04 Output/short_bal_panel_no_geo_50_pct.csv")
state <- read_csv('./01 Data processing/04 Output/State data with CDM projects.csv')

# All India, region FE
all_india_z_fe <- feols(manuf_emp ~ rain + lai + elevatn|zone + year|log_poll ~ ws, 
                      data = df_short_50, 
                      panel.id = c('pc11_d_', 'year'),
                      vcov = conley())
summary(all_india_z_fe, stage = 1:2)

# Regional manufacturing regressions
z_n <- feols(manuf_emp ~ rain + lai + elevatn| zone + year|log_poll ~ ws, 
                      data = df_short_50 %>% filter(zone == 'North'), 
                      panel.id = c('pc11_d_', 'year'),
                      vcov = conley())
summary(z_n, stage = 1:2)

z_ne <- feols(manuf_emp ~ rain + lai + elevatn| zone + year|log_poll ~ ws, 
                      data = df_short_50 %>% filter(zone == 'North East'), 
                      panel.id = c('pc11_d_', 'year'),
                      vcov = conley())
summary(z_ne, stage = 1:2)


z_e <- feols(manuf_emp ~ rain + lai + elevatn| zone + year|log_poll ~ ws, 
                      data = df_short_50 %>% filter(zone == 'East'), 
                      panel.id = c('pc11_d_', 'year'),
                      vcov = conley())
summary(z_e, stage = 1:2)

z_c <- feols(manuf_emp ~ rain + lai + elevatn| zone + year|log_poll ~ ws, 
                      data = df_short_50 %>% filter(zone == 'Central'), 
                      panel.id = c('pc11_d_', 'year'),
                      vcov = conley())
summary(z_c, stage = 1:2)

z_s <- feols(manuf_emp ~ rain + lai + elevatn| zone + year|log_poll ~ ws, 
                      data = df_short_50 %>% filter(zone == 'South'), 
                      panel.id = c('pc11_d_', 'year'),
                      vcov = conley())
summary(z_s, stage = 1:2)

z_w <- feols(manuf_emp ~ rain + lai + elevatn| zone + year|log_poll ~ ws, 
                      data = df_short_50 %>% filter(zone == 'West'), 
                      panel.id = c('pc11_d_', 'year'),
                      vcov = conley())
summary(z_w, stage = 1:2)

## CDM regressions ##
state <- state %>% mutate(log_gdp = log(gdp))
state_reg <- feols(workers_pc ~ mean_rainfall + mean_lai + mean_elevation + log_gdp | year |log_bc + log_bc:cum_cdm_prj ~ avgwindspeed*cum_cdm_prj,
                   data = state, 
                   panel.id = c('state_code', 'year'),
                   vcov = conley())

summary(state_reg, stage = 1:2)

all_india_cdm <- feols(manuf_emp ~ rain + lai + elevatn|zone + year|log_poll + log_poll:cdm_prj ~ ws*cdm_prj, 
                        data = df_short_50, 
                        panel.id = c('pc11_d_', 'year'),
                        vcov = conley())
summary(all_india_cdm, stage = 1:2)


  