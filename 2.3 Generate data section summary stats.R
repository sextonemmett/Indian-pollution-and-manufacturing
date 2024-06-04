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

# Load processed data
cnt <- read_csv('./02 Analysis/00 Processed country and state data/completed_data_w_gdp.csv')
cnt <- cnt %>% 
  mutate(b_carbon = b_carbon*1000000000) %>% 
  filter(year > 1997 & year < 2023)

state <- read_csv('./02 Analysis/00 Processed country and state data/completed_state_df.csv')
state <- state %>% 
  mutate(b_carbon = avgpollution*1000000000) %>% 
  rename(m_percent = workers_pc)

dist <- read_csv('./01 Data processing/04 Output/short_bal_panel_no_geo_50_pct.csv')
dist <- dist %>%   
  mutate(b_carbon = poll*1000000000) %>% 
  rename(m_percent = manuf_emp)

summary_stats_df <- function(df, column) {
  data <- df[[column]]
  stats <- data.frame(
    "25%" = quantile(data, probs = 0.25, na.rm = TRUE),
    "50%" = median(data, na.rm = TRUE),
    "Mean" = mean(data, na.rm = TRUE),
    "75%" = quantile(data, probs = 0.75, na.rm = TRUE),
    "SD" = sd(data, na.rm = TRUE)
  )
  return(stats)
}

dataframes <- list(cnt, state, dist)

summ_poll_list <- lapply(dataframes, summary_stats_df, column = "b_carbon")
summary_poll <- do.call(rbind, summ_poll_list)
row.names(summary_poll) <- c("cnt", "state", "dist")
write.csv(summary_poll, './02 Analysis/02 Output/Pollution summary stats.csv')

summ_manuf_list <- lapply(dataframes, summary_stats_df, column = "m_percent")
summary_manuf <- do.call(rbind, summ_manuf_list)
row.names(summary_manuf) <- c("cnt", "state", "dist")
write.csv(summary_manuf, './02 Analysis/02 Output/Manufacturing summary stats.csv')













