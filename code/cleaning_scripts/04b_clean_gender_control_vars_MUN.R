rm(list=ls())

library(tidyverse)
library(here)
library(sf)
library(areal)

options(scipen = 999)

data2006 <- read.csv(here("data", "raw_data", "control_variables", "census_2001_gender_MUN.csv"))

data2006 <- data2006 %>% 
  separate(local_municipality_id, c("local_municipality_id", "extra"), sep = ":") %>% 
  select(-c("extra"))

# NOW SYNC THESE VALUES WITH 2006 MUN BOUNDARIES (OFFICIAL MUN BOUNDARIES MOST TEMPORALLY PROXIMATE TO 2006) 
mun_2006 <- st_read(here("data", "gis_data_raw", "LocalMunicipalities2006.shp"))
mun_2006 <- st_transform(mun_2006, 6148)

mun_2006 <- mun_2006 %>% 
  rename(local_municipality_id = CAT_B) %>% 
  select(local_municipality_id, geometry)

# merge control variables dataset with municipal shape file
data2006_v2 <- left_join(mun_2006, data2006, by = "local_municipality_id")

# now read in 2016 shape files
mun_2016 <- st_read(here("data", "gis_data_raw", "ecc03798-c4f1-42e2-a0e6-573c0c07c8542020410-1-1psvop3.0vyd.shp"))
mun_2016 <- st_transform(mun_2016, 6148)

# rename ward_id variable for subsequent merger
mun_2016 <- mun_2016 %>% 
  rename(local_municipality_id = LocalMunic)

# clean GIS geometries to prepare for merge
data2006_v2 <- st_as_sf(data2006_v2)
mun_2016 <- st_as_sf(mun_2016)

data2006_v2 <- st_make_valid(data2006_v2)
mun_2016 <- st_make_valid(mun_2016)

# areal interpolation to 2016 ward boundaries
df_areal_2006 <- aw_interpolate(mun_2016, 
                                tid = "local_municipality_id", 
                                source = data2006_v2,
                                sid = "local_municipality_id",
                                weight = "sum", 
                                output = "tibble",
                                intensive = c("Male", "Female", "Total"))





#################################################################
#################################################################
############# NOW DO THE SAME THING FOR 2011 DATA ###############
#################################################################
#################################################################

data2011 <- read.csv(here("data", "raw_data", "control_variables", "census_2011_gender_MUN.csv"))

data2011 <- data2011 %>% 
  separate(local_municipality_id, c("local_municipality_id", "extra"), sep = ":") %>% 
  select(-c("extra"))

# NOW SYNC THESE VALUES WITH 2006 MUN BOUNDARIES (OFFICIAL MUN BOUNDARIES MOST TEMPORALLY PROXIMATE TO 2006) 
mun_2011 <- st_read(here("data", "gis_data_raw", "MDB_Local_Municipal_Boundary_2011.shp"))
mun_2011 <- st_transform(mun_2011, 6148)

mun_2011 <- mun_2011 %>% 
  rename(local_municipality_id = LocalMunic) %>% 
  select(local_municipality_id, geometry)

# merge control variables dataset with municipal shape file
data2011_v2 <- left_join(mun_2011, data2011, by = "local_municipality_id")

# clean GIS geometries to prepare for merge
data2011_v2 <- st_as_sf(data2011_v2)

data2011_v2 <- st_make_valid(data2011_v2)

# areal interpolation to 2016 ward boundaries
df_areal_2011 <- aw_interpolate(mun_2016, 
                                tid = "local_municipality_id", 
                                source = data2011_v2,
                                sid = "local_municipality_id",
                                weight = "sum", 
                                output = "tibble",
                                intensive = c("Male", "Female", "Total"))



#################################################################
#################################################################
############## NOW READ IN AND INSPECT 2016 DATA ################
#################################################################
#################################################################

data2016 <- read.csv(here("data", "raw_data", "control_variables", "census_2016_gender_MUN.csv"))

data2016 <- data2016 %>% 
  separate(local_municipality_id, c("local_municipality_id", "extra"), sep = " :") %>% 
  select(-c("extra"))

data2016 <- data2016 %>% 
  pivot_wider(id_cols = local_municipality_id,
              names_from = Sex,
              values_from = Count)




####################################################################
########## NOW PREPARE TO MERGE THESE DATASETS TOGETHER ############
####################################################################


##### select relevant variables
df_areal_2006 <- df_areal_2006 %>% 
  select(local_municipality_id, Female, Male, Total)

df_areal_2011 <- df_areal_2011 %>% 
  select(local_municipality_id, Female, Male, Total)


####add electoral cycle variable
df_areal_2006 <- df_areal_2006 %>% 
  mutate(electoral_cycle = 2006) 

df_areal_2011 <- df_areal_2011 %>% 
  mutate(electoral_cycle = 2011)

data2016 <- data2016 %>% 
  mutate(electoral_cycle = 2016)

####pull together
data_full <- rbind(df_areal_2006, df_areal_2011)
data_full <- rbind(data_full, data2016)

data_full$Female <- round(data_full$Female, 0)
data_full$Male <- round(data_full$Male, 0)
data_full$Total <- round(data_full$Total, 0)

####create new variable for proportion of ward population that is female
data_full <- data_full %>% 
  rename(female = Female, 
         male = Male, 
         total_pop = Total) %>% 
  mutate(prop_pop_female = female/total_pop)

# EXPORT DATA TO PROCESSED_DATA SUBDIRECTORY 
saveRDS(data_full, here("data", "processed_data", "data_controls_gender_MUN.rds"))
