rm(list = ls())
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(lubridate)
library(purrr)
library(fuzzyjoin)

setwd('/Users/emmettsexton/Dropbox/Mac (2)/Desktop/BSE/Thesis/')

# CDM data source: https://unepccc.org/cdm-ji-pipeline/
cdm <- read_excel('./01 Data processing/01 Raw data/CDM/cdm-pipeline.xlsx', skip = 3, sheet = 'CDM_Projects', col_types = 'text')

# Data exploration
cdm %>% distinct(`Unique project ID`) %>% nrow()
cdm %>% distinct(Ref.) %>% nrow()
cdm %>% count(`PA/PoA`)
cdm %>% count(Status)
types = cdm %>% count(Type)

# Subset to projects in India where the State variable populated
india <- cdm %>% 
  filter(`Host country` == 'India' & Status == 'Registered')

india %>% count(Status)

india <- india %>% 
  filter(!is.na(`Province / State`) & !(`Province / State` %in% c('many', 'Many')))

# Filter out wind projects because that is our instrument
indian_types <- india %>% count(Type)
india <- india %>% 
  filter(Type != 'Wind')

india %>% count(Status)
india %>% count(`PA/PoA`)

# Grab columns of interest
india = india %>% 
  mutate(std_end = as.Date(as.numeric(`End of last crediting period`), origin = "1899-12-30"),
         std_start = as.Date(as.numeric(`Start 1st period`), origin = "1899-12-30")) %>% 
  mutate(`Total issuance (kCERs)` = as.numeric(`Total issuance (kCERs)`)) %>% 
  select(title = Title, cdm_reference_number = `Ref.`, cdm_uniq_id = `Unique project ID`, state = `Province / State`, kcers = `Total issuance (kCERs)`,  std_start, std_end) %>% 
  filter(kcers > 0)

write.csv(india, './01 Data processing/03 Output/List of Indian non-wind projects.csv', row.names = FALSE)

ggplot(india %>% filter(kcers < 1000), aes(x=kcers)) + geom_histogram(bins = 30)  

states = india %>% count(state)           

# Where a project is across multiple states, I split the kcers evenly across states
india_split <- india %>%
  mutate(state = str_replace(state, "Andaman and Nicobar", "Andaman and Nicobar Islands")) %>% 
  mutate(state = str_replace(state, "Orissa", "Odisha")) %>% 
  mutate(states = str_split(state, " & ")) %>% 
  unnest(cols = c(states)) %>% 
  group_by(cdm_reference_number) %>% 
    mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(kcers = kcers/n,
         num_proj = 1/n,
         start_year = year(std_start)) %>% 
  select(-state, -n) 

states_split = india_split %>% count(states)   

cdm_summ <- india_split %>% 
  group_by(states, start_year) %>% 
    summarize(kcers = sum(kcers),
              projects = sum(num_proj)) %>% 
  ungroup() %>% 
  rename(State = states)

sum(cdm_summ$kcers)
sum(cdm_summ$projects)

write.csv(cdm_summ, file = "./01 Data processing/CDM projects by kCER, state, and year.csv", row.names = FALSE)

# Add state codes
state_codes <- read_excel('./01 Data processing/01 Raw data/Geo/Indian state codes.xlsx')
merged <- stringdist_left_join(cdm_summ, state_codes, by = "State", max_dist = 2)
merged <- merged %>% 
  select(year = start_year, state_code = Code_2, cdm_prj = projects)

# Read in states data
state_df <- read_csv('./02 Analysis/00 Processed country and state data/completed_state_df.csv')

states_w_cdm <- left_join(state_df, merged, by = c('year', 'state_code'))
states_w_cdm <- states_w_cdm %>% 
  mutate(cdm_prj = replace_na(cdm_prj, 0)) %>% 
  group_by(state_code) %>% 
  arrange(year) %>% 
    mutate(cum_cdm_prj = cumsum(cdm_prj)) %>% 
  ungroup()
  
write.csv(states_w_cdm, file = "./01 Data processing/04 Output/State data with CDM projects.csv", row.names = FALSE)



