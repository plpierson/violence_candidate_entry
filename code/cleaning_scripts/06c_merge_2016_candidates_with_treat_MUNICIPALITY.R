rm(list=ls())

library(tidyverse)
library(sf)
library(readxl)
library(lubridate)
library(numform)
library(here)


# READ IN ASSASSINATIONS DATA ------------------
viol <- read_excel(here("data", "raw_data", "assassinations_data", "za_assassinations copy.xlsx"))


# CLEAN ASSASSINATIONS DATA ------------------
viol$year <- year(viol$date_of_attack)
viol$month <- format(viol$date_of_attack, "%m")
viol$month <- f_num(viol$month, digits=0)
viol$sitting <- as.numeric(viol$sitting)
viol$electoral_cycle <- as.integer(viol$electoral_cycle)

# FILTER TO LOOK EXCLUSIVELY AT ATTACKS ON LC/MC WARD POLITICIANS AND/OR CANDIDATES ------------------
viol_mun <- viol %>% 
  filter(seat_type=="LC Ward" | seat_type=="MC Ward" | seat_type=="MC Ward candidate" |
           seat_type=="LC Ward candidate" | seat_type=="ANC ward candidate" |
           seat_type=="LC PR" | seat_type=="LC PR; DC 40" | seat_type=="LC PR; DC PR" |
           seat_type=="LC PR; LC PR candidate" | seat_type=="MC PR") %>% 
  filter(date_of_attack>="2000-12-05" & date_of_attack<="2016-08-03") 

#viol_ward_sitting <- viol_ward %>% 
#  filter(sitting == 1)

viol_2011_to_2016 <- viol_mun %>% 
  filter(date_of_attack > "2011-05-18" & date_of_attack < "2016-08-03")



print(unique(viol_2011_to_2016$local_municipality_id))




#NOW READ IN THE CLEANED CANDIDATES DATA AND CREATE A VARIABLE FOR ASSASSINATION TREATMENT
data_2016 <- readRDS(here("data", "processed_data", "candidates_clean_2016_MUNICIPALITY.RDS"))


# CREATE DUMMY VARIABLE FOR ANY EXPOSED UNITS ------------------
data_2016 <- data_2016 %>% 
  mutate(treat = case_when(
    local_municipality_id == "KZN211" ~ 1,
    local_municipality_id == "KZN212" ~ 1,
    local_municipality_id == "NMA" ~ 1,
    local_municipality_id == "KZN214" ~ 1,
    local_municipality_id == "KZN238" ~ 1,
    local_municipality_id == "KZN252" ~ 1,
    local_municipality_id == "KZN265" ~ 1,
    local_municipality_id == "ETH" ~ 1,
    local_municipality_id == "LIM472" ~ 1,
    local_municipality_id == "KZN244" ~ 1,
    local_municipality_id == "JHB" ~ 1,
    local_municipality_id == "EC156" ~ 1,
    local_municipality_id == "NW373" ~ 1,
    local_municipality_id == "NC451" ~ 1,
    local_municipality_id == "KZN242" ~ 1,
    local_municipality_id == "KZN236" ~ 1,
    local_municipality_id == "GT481" ~ 1,
    local_municipality_id == "KZN234" ~ 1,
    local_municipality_id == "KZN216" ~ 1
  )) 



# check to make sure this worked properly: 
length(which(data_2016$treat==1)) ###16


# WRITE OUT DATA TO PROCESSED_DATA FILE
saveRDS(data_2016, here("data", "processed_data", "data_2016_with_assassinations_merged_MUNICIPALITY.rds"))



