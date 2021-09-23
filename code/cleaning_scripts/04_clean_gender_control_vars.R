rm(list=ls())

library(tidyverse)
library(here)

options(scipen = 999)

data2006 <- read.csv(here("data", "raw_data", "sa_census_2001_in_2011_boundaries_gender.csv"))
data2011 <- read.csv(here("data", "raw_data", "sa_census_2011_gender.csv"))
data2016 <- read.csv(here("data", "raw_data", "sa_census_2011_in_2016_boundary_gender.csv"))

data2006 <- data2006 %>% 
  separate(ward_id, c("ward_id", "extra"), sep = ":") %>% 
  select(-c("extra"))

data2011 <- data2011 %>% 
  separate(ward_id, c("ward_id", "extra"), sep = ":") %>% 
  select(-c("extra"))

data2016 <- data2016 %>% 
  separate(ward_id, c("ward_id", "extra"), sep = " ") %>% 
  select(-c("extra"))

# NOW SYNC THESE VALUES WITH 2011 WARD BOUNDARIES (OFFICIAL WARD BOUNDARIES MOST TEMPORALLY PROXIMATE TO 2006) 
ward_2011 <- st_read(here("data", "gis_data_raw", "wards2011.shp"))
ward_2011 <- st_transform(ward_2011, 6148)

ward_2011 <- ward_2011 %>% 
  rename(ward_id = WardID) %>% 
  select(ward_id, geometry)

# raw 2011 shape file has duplicates...drop these
ward_2011 <- ward_2011 %>% 
  distinct(ward_id, .keep_all = TRUE)

data2006_v2 <- left_join(ward_2011, data2006, by = "ward_id")

# now read in 2016 shape files
ward_2016 <- st_read(here("data", "gis_data_raw", "wards2016.shp"))
ward_2016 <- st_transform(ward_2016, 6148)

# rename ward_id variable for subsequent merger
ward_2016 <- ward_2016 %>% 
  rename(ward_id = WardID)

# clean GIS geometries to prepare for merge
data2006_v2 <- st_as_sf(data2006_v2)
ward_2016 <- st_as_sf(ward_2016)

data2006_v2 <- st_make_valid(data2006_v2)
ward_2016 <- st_make_valid(ward_2016)

# areal interpolation to 2016 ward boundaries
df_areal_2006 <- aw_interpolate(ward_2016, 
                           tid = "ward_id", 
                           source = data2006_v2,
                           sid = "ward_id",
                           weight = "sum", 
                           output = "tibble",
                           intensive = c("male", "female", "total_pop"))



##### NOW DO THE SAME FOR 2011
data2011_v2 <- left_join(ward_2011, data2011, by = "ward_id")

# clean GIS geometries to prepare for merge
data2011_v2 <- st_as_sf(data2011_v2)

data2011_v2 <- st_make_valid(data2011_v2)

# areal interpolation to 2016 ward boundaries
df_areal_2011 <- aw_interpolate(ward_2016, 
                                tid = "ward_id", 
                                source = data2011_v2,
                                sid = "ward_id",
                                weight = "sum", 
                                output = "tibble",
                                intensive = c("male", "female", "total_pop"))



##### select relevant variables
df_areal_2006 <- df_areal_2006 %>% 
  select(ward_id, female, male, total_pop)

df_areal_2011 <- df_areal_2011 %>% 
  select(ward_id, female, male, total_pop)


####add electoral cycle variable
df_areal_2006 <- df_areal_2006 %>% 
  mutate(electoral_cycle = 2006) %>% 
  select(ward_id, female, male, total_pop, electoral_cycle)


df_areal_2011 <- df_areal_2011 %>% 
  mutate(electoral_cycle = 2011)


data2016 <- data2016 %>% 
  mutate(electoral_cycle = 2016)

####pull together
data_full <- rbind(df_areal_2006, df_areal_2011)
data_full <- rbind(data_full, data2016)

data_full$female <- round(data_full$female, 0)
data_full$male <- round(data_full$male, 0)
data_full$total_pop <- round(data_full$total_pop, 0)

####create new variable for proportion of ward population that is female
data_full <- data_full %>% 
  mutate(prop_pop_female = female/total_pop)

# EXPORT DATA TO PROCESSED_DATA SUBDIRECTORY 
saveRDS(data_full, here("data", "processed_data", "data_controls_gender_by_ward.rds"))
