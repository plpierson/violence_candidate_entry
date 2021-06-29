rm(list=ls())

library(tidyverse)
library(here)
library(readxl)
library(stringr)
library(naniar) #useful for dealing with various forms of NA values
library(sf)
library(areal)
library(lubridate)

options(scipen = 999) # get rid of scientific notation 


##### READ IN LGE 2016 FILES
ec <- read_excel(here("data", "raw_data", "lge2016_age_gender.xlsx"), sheet = "EC")
fs <- read_excel(here("data", "raw_data", "lge2016_age_gender.xlsx"), sheet = "FS")
gt <- read_excel(here("data", "raw_data", "lge2016_age_gender.xlsx"), sheet = "GT")
kz <- read_excel(here("data", "raw_data", "lge2016_age_gender.xlsx"), sheet = "KZ")
lm <- read_excel(here("data", "raw_data", "lge2016_age_gender.xlsx"), sheet = "LM")
mp <- read_excel(here("data", "raw_data", "lge2016_age_gender.xlsx"), sheet = "MP")
nc <- read_excel(here("data", "raw_data", "lge2016_age_gender.xlsx"), sheet = "NC")
nw <- read_excel(here("data", "raw_data", "lge2016_age_gender.xlsx"), sheet = "NW")
wc <- read_excel(here("data", "raw_data", "lge2016_age_gender.xlsx"), sheet = "WC")

  # merge provincial excel sheets together into one dataframe
data <- rbind(ec, fs, gt, kz, lm, mp, nc, nw, wc)

glimpse(data)


  # filter out PR positions
data <- data %>% 
  filter(`PR List OrderNo / Ward No` >= 200000)

  # tidy data
data <- data %>% 
  separate(Municipality, c("local_municipality_id", "local_municipality_name"), sep = "-",
           extra = "merge", fill = "right") %>% 
  rename(party_name = Party,
         ward_id = `PR List OrderNo / Ward No`,
         id_number = IDNumber,
         first_name = Fullname,
         last_name = Surname) %>% 
  mutate(electoral_cycle = 2016) %>% 
  mutate(electoral_cycle = as.factor(electoral_cycle),
         party_name = as.factor(party_name),
         ward_id = as.factor(ward_id)) %>% 
  unite(full_name, sep = " ", na.rm = TRUE, first_name:last_name, remove = FALSE) %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  mutate(date_of_birth = substr(id_number, 1, 6)) %>% 
  mutate(date_of_birth = paste(19, date_of_birth, sep = "")) %>% 
  mutate(date_of_birth = ymd(date_of_birth)) %>% 
  mutate(gender = substr(id_number, 7, 10)) %>% 
  mutate(gender = as.factor(ifelse(gender <= 4999, "F", "M"))) %>% 
  mutate(age = floor(lubridate::time_length(difftime(as.Date("2016-08-03"), date_of_birth), "years")))


# ok, what does the data look like?
glimpse(data)

length(unique(data$local_municipality_id)) #213
length(unique(data$local_municipality_name)) #212...hmmm, what is the discrepancy here?

table(data$local_municipality_id)
table(data$local_municipality_name)


length(unique(data$party_name)) # 206 distinct parties fielded ward candidates in 2016 LGE
length(unique(data$ward_id)) # 4392 wards across the 213 municipalities
length(unique(data$full_name)) # 25614 distinct candidates out of 36937 total...so a number of individuals ran for 
# office in more than one ward

summary(data$gender) # no NA...almost exactly 2x as meny men running in ward elections
summary(data$age) # no NA...though an 88 year old ran somewhere!



  #### NOW AGGREGATE THE DATA TO GET SUMMARY VALUES AT THE MUNICIPAL LEVEL 
data_summary <- data %>% 
  group_by(ward_id) %>% 
  mutate(sum_candidates = n()) %>% 
  ungroup() %>% 
  group_by(ward_id) %>% 
  mutate(num_male = sum(length(which(gender=="M")))) %>% 
  ungroup() %>% 
  group_by(ward_id) %>% 
  mutate(mean_age = mean(age, na.rm = TRUE)) %>% 
  mutate(prop_female = 1 - (num_male / sum_candidates)) %>% 
  distinct(ward_id, .keep_all = TRUE) %>% 
  select(local_municipality_id, local_municipality_name, electoral_cycle, sum_candidates, num_male, mean_age, prop_female)


  # so how does the data look?
head(data_summary)
sum(data_summary$sum_candidates) # 36937...this comports with raw data
sum(data_summary$num_male) # 24612...this comports with raw data
summary(data_summary$mean_age)
summary(data_summary$prop_female)



  # trim extraneous variables to match cleanly with cleaned 2011 data
data_summary <- data_summary %>% 
  select(-c(local_municipality_id, local_municipality_name))


# WRITE TO PROCESSED_DATA FOLDER -----------------------
saveRDS(data_summary, here("data", "processed_data", "candidates_clean_2016.RDS"))




##### below this line not needed for present analysis

#######################################################################
####### READ IN 2016 SHAPE FILE AND MERGE WITH CANDIDATES DATA ########
#######################################################################
ward_2016_shape_file <- st_read(here("data", "gis_data_raw", "wards2016.shp"))

ward_2016_shape_file <- ward_2016_shape_file %>% 
  rename(province = ProvinceNa,
         local_municipality_id = LocalMunic,
         ward_id = WardID) %>% 
  mutate(ward_id = as.factor(ward_id)) %>% 
  select(local_municipality_id, ward_id, geometry)



  # merge GIS polygons with candidates data
data_final <- left_join(data_summary, ward_2016_shape_file, by = "ward_id") 

# WRITE TO PROCESSED_DATA FOLDER -----------------------
saveRDS(data_final, here("data", "processed_data", "01c_candidates_clean_2016.RDS"))
