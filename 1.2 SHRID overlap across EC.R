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

setwd('/Users/emmettsexton/Dropbox/Mac (2)/Desktop/BSE/Thesis/')

# Load Economic Censuses for 1990, 1998, 2005, and 2013
ec_90 <- read.csv('./01 Data processing/01 Raw data/SHRUG/shrug-ec90-csv/ec90_shrid.csv')
ec_98 <- read.csv('./01 Data processing/01 Raw data/SHRUG/shrug-ec98-csv/ec98_shrid.csv')
ec_05 <- read.csv('./01 Data processing/01 Raw data/SHRUG/shrug-ec05-csv/ec05_shrid.csv')
ec_13 <- read.csv('./01 Data processing/01 Raw data/SHRUG/shrug-ec13-csv/ec13_shrid.csv')

# Find shrids that are present in all four ECs
shrids_90 <- ec_90$shrid2
shrids_98 <- ec_98$shrid2
shrids_05 <- ec_05$shrid2
shrids_13 <- ec_13$shrid2

overlap_90_98 <- shrids_90[shrids_90 %in% shrids_98]
overlap_90_98_05 <- overlap_90_98[overlap_90_98 %in% shrids_05]
shrids_in_all_ec <- overlap_90_98_05[overlap_90_98_05 %in% shrids_13]

write.csv(shrids_in_all_ec, "./01 Data processing/03 Temp files/shrids_in_all_ec.csv", row.names = FALSE)

overlap_05_13 <- shrids_05[shrids_05 %in% shrids_13]
write.csv(overlap_05_13, "./01 Data processing/03 Temp files/shrids_in_05_13_ec.csv", row.names = FALSE)

overlap_98_05_13 <- shrids_98[shrids_98 %in% overlap_05_13]
write.csv(overlap_98_05_13, "./01 Data processing/03 Temp files/shrids_in_98_05_13_ec.csv", row.names = FALSE)

