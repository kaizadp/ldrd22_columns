## Picarro Processing Script

## This script cleans the raw Picarro data and extracts CO2/CH4 concentrations (ppm)
## and computes gas fluxes
## This script load the {picarro.data} package and `source` additional functions from the picarro_data.R script.

## Code created by BBL
## edits by KFP, May 2022


####################### #
####################### #

# 1. setup ----
## load packages
library(tidyverse) # to clean/tidy data, and plot data
library(lubridate) # to work with dates

# My 'picarro.data' package isn't on CRAN (yet) so need to install it via:
# devtools::install_github("PNNL-TES/picarro.data")
library(picarro.data)

## load the picarro processing functions
source("code/1a-picarro_data.R")

## set Picarro Path
# this is the directory where the Picarro data files are being stored
PICARROPATH = "data/respiration/2022/04"


#
# 2. process the Picarro data ----

core_key = read.csv("data/corekey.csv") %>% mutate(Core = as.character(Core))

valve_key = read.csv("data/valve_map.csv") %>%
  filter(Start_Time != "" & Stop_Time != "" & Stop_Date != "") %>% 
  dplyr::mutate(Start_datetime = mdy_hm(paste(Start_Date, Start_Time), tz = "America/Los_Angeles"),
                Stop_datetime = mdy_hm(paste(Stop_Date, Stop_Time), tz = "America/Los_Angeles")) %>% 
  left_join(core_key %>% mutate(Core = as.character(Core)))


# import and combine all the Picarro (raw) data files  
picarro_raw = sapply(list.files(path = PICARROPATH, pattern = "dat$", recursive = TRUE,full.names = TRUE),
                     read.table, header=TRUE, simplify = FALSE) %>% bind_rows()  

# clean the Picarro data
picarro_clean = clean_picarro_data(picarro_raw %>% mutate(CO2_dry = CO2))

# Match Picarro data with the valve key data
pcm = match_picarro_data(picarro_clean, valve_key)
picarro_clean_matched = pcm$pd
picarro_match_count = pcm$pmc
valve_key_match_count = pcm$vkmc


# get CO2 concentrations (ppm)
ghg_ppm =
  subset(merge(picarro_clean_matched, 
               valve_key %>% dplyr::select(Core, Start_datetime, Stop_datetime, Treatment)),
         DATETIME <= Stop_datetime & DATETIME >= Start_datetime & Core == Core) %>% 
  dplyr::select(-Start_datetime, -Stop_datetime) %>% 
  left_join(core_key) %>% 
  dplyr::select(Core, DATETIME, MPVPosition, CH4_dry, CO2_dry, Elapsed_seconds, 
                Core_assignment, Sample_number)




ghg_ppm_max = 
  ghg_ppm %>% 
  group_by(Core, MPVPosition, Core_assignment, Sample_number) %>% 
  mutate(CO2_max = CO2_dry == max(CO2_dry)) %>% 
  filter(CO2_max) %>% 
  dplyr::select(-CO2_max)

  filter(MPVPosition < 2 & Sample_number < 100)

ghg_ppm_max %>% 
  distinct(Sample_number)

# compute fluxes
ghg_fluxes = compute_ghg_fluxes(picarro_clean_matched, valve_key)


#
# 3. make graphs ----

# ghg_ppm %>% 
#   ggplot(aes(x = DATETIME, y = CO2_dry))+
#   geom_point()+
#   facet_wrap(~Core_assignment)  


ghg_ppm_max %>% 
  ggplot(aes(x = DATETIME, y = CO2_dry))+
  geom_point()+ 
  geom_line()+
  facet_wrap(~Core_assignment)  

ghg_fluxes %>% 
  ggplot(aes(x = DATETIME, y = flux_co2_umol_s))+
  geom_point()+ 
  geom_line()+
  facet_wrap(~Core)  
