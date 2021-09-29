library(tidyverse)
library(here)
library(readxl)
library(sf)
library(areal)

rm(list=ls())

options(scipen = 999) # get rid of scientific notation for variables



# IMPORT CONTROL VARIABLES ----------------------------

# employment
employ <- read.csv(here("data", "raw_data", "control_variables", "ward_2011_employment_status.csv"))

employ <- employ[235:4511,]

names(employ)
employ <- employ %>% 
  rename(ward_id = Official.employment.status) 

employ$ward_id <- gsub(":.*", "", employ$ward_id)
employ <- employ %>% 
  mutate_if(is.numeric, round, 0)

employ <- employ %>% 
  mutate(unemploy_rate = (Unemployed + Discouraged.work.seeker) / (Total - Not.applicable))




# first language
lang <- read.csv(here("data", "raw_data", "control_variables", "ward_2011_first_language.csv"))     

names(lang)
lang <- lang %>% 
  rename(ward_id = Language..first.)

lang$ward_id <- gsub(":.*", "", lang$ward_id)
lang <- lang %>% 
  mutate_if(is.numeric, round, 0)

lang <- lang %>% 
  mutate(sum = rowSums(across(where(is.numeric)))) %>% 
  mutate(prop_english = (English / sum))




# main dwelling type
dwell <- read_excel(here("data", "raw_data", "control_variables", "ward_2011_main_dwelling.xls"))

dwell <- dwell %>% 
  rename(ward_id = "Type of main dwelling - main")

dwell$ward_id <- gsub(":.*", "", dwell$ward_id)
dwell <- dwell %>% 
  mutate_if(is.numeric, round, 0)

dwell <- dwell %>% 
  mutate(prop_informal = (`Informal dwelling/shack in back yard` + `Informal dwelling/shack NOT in back yard; e.g. in an informal/squatter settlement or on farm`) / (Total - `Unspecified` -`Not applicable`))


# population groups
pop <- read.csv(here("data", "raw_data", "control_variables", "ward_2011_population_groups.csv"))    

pop <- pop %>% 
  rename(ward_id = Population.group)

pop$ward_id <- gsub(":.*", "", pop$ward_id)

pop <- pop %>% 
  mutate_if(is.numeric, round, 0)

pop <- pop %>% 
  mutate(prop_black = `Black.African` / Total)



# SUBSET EACH DATAFRAME TO INCLUDE ONLY THOSE VARIABLES NEEDED ----------------------------

employ_sub <- employ %>% 
  select(ward_id, unemploy_rate)

lang_sub <- lang %>% 
  select(ward_id, prop_english)

dwell_sub <- dwell %>% 
  select(ward_id, prop_informal)

pop_sub <- pop %>% 
  select(ward_id, prop_black)


# MERGE TOGETHER USING RBIND ----------------------------
df_controls <- employ_sub %>% 
  inner_join(lang_sub, by = "ward_id")

df_controls <- df_controls %>% 
  inner_join(dwell_sub, by = "ward_id")

df_controls <- df_controls %>% 
  left_join(pop_sub, by = "ward_id")



# NOW SYNC THESE VALUES WITH 2011 WARD BOUNDARIES (OFFICIAL WARD BOUNDARIES MOST TEMPORALLY PROXIMATE TO 2006) 
ward_2011 <- st_read(here("data", "gis_data_raw", "wards2011.shp"))
ward_2011 <- st_transform(ward_2011, 6148)

ward_2011 <- ward_2011 %>% 
  rename(ward_id = WardID) %>% 
  select(ward_id, geometry)

# raw 2011 shape file has duplicates...clean these below
ward_2011 <- ward_2011 %>% 
  distinct(ward_id, .keep_all = TRUE)

controls_2011 <- left_join(ward_2011, df_controls, by = "ward_id")

# now read in 2016 shape files
ward_2016 <- st_read(here("data", "gis_data_raw", "wards2016.shp"))
ward_2016 <- st_transform(ward_2016, 6148)

# rename ward_id variable for subsequent merger
ward_2016 <- ward_2016 %>% 
  rename(ward_id = WardID)

# clean GIS geometries to prepare for merge
controls_2011 <- st_as_sf(controls_2011)
ward_2011 <- st_as_sf(ward_2011)

controls_2011 <- st_make_valid(controls_2011)
ward_2016 <- st_make_valid(ward_2016)

# areal interpolation to 2016 ward boundaries
df_areal <- aw_interpolate(ward_2016, 
                           tid = "ward_id", 
                           source = controls_2011,
                           sid = "ward_id",
                           weight = "sum", 
                           output = "tibble",
                           intensive = c("unemploy_rate", "prop_english", "prop_informal", "prop_black"))


# select variables we would like to keep
df_areal_2 <- df_areal %>% 
  select(ward_id, prop_black, prop_english, prop_informal, unemploy_rate)



# EXPORT DATA TO PROCESSED_DATA SUBDIRECTORY 
saveRDS(df_areal_2, here("data", "processed_data", "data_2011_controls.rds"))





#####################################################################################################
#####################################################################################################
############################ CONTROL VARIABLES AT MUNICIPAL LEVEL ###################################
#####################################################################################################
#####################################################################################################

# employment
employ_mun <- read.csv(here("data", "raw_data", "control_variables", "census_2011_unemployment_mun.csv"))  

employ_mun <- employ_mun %>% 
  rename(local_municipality_id = Geography) 

employ_mun$local_municipality_id <- gsub(":.*", "", employ_mun$local_municipality_id)
employ_mun <- employ_mun %>% 
  mutate_if(is.numeric, round, 0)

employ_mun <- employ_mun %>%    
  mutate(unemploy_rate = (Unemployed + Discouraged.work.seeker) / (Total - Not.applicable))


# first language
lang_mun <- read.csv(here("data", "raw_data", "control_variables", "census_2011_language_mun.csv"))  

names(lang_mun)

lang_mun$local_municipality_id <- gsub(":.*", "", lang_mun$local_municipality_id)
lang_mun <- lang_mun %>% 
  mutate_if(is.numeric, round, 0)

lang_mun <- lang_mun %>% 
  mutate(sum = rowSums(across(where(is.numeric)))) %>% 
  mutate(prop_english = (English / sum))


# main dwelling type
dwell_mun <- read.csv(here("data", "raw_data", "control_variables", "census_2011_type_of_dwelling_mun.csv")) 

dwell_mun <- dwell_mun %>% 
  rename(local_municipality_id = Geography)

dwell_mun$local_municipality_id <- gsub(":.*", "", dwell_mun$local_municipality_id)
dwell_mun <- dwell_mun %>% 
  mutate_if(is.numeric, round, 0)

dwell_mun <- dwell_mun %>% 
  mutate(prop_informal = (Informal.dwelling..shack..in.backyard. + Informal.dwelling..shack..not.in.backyard..e.g..in.an.informal.squatter.settlement.or.on.a.farm.) / Total)


#population groups
pop_mun <- read.csv(here("data", "raw_data", "control_variables", "census_2011_ethnicity_mun.csv"))   

pop_mun <- pop_mun %>% 
  rename(local_municipality_id = Geography)

pop_mun$local_municipality_id <- gsub(":.*", "", pop_mun$local_municipality_id)

pop_mun <- pop_mun %>% 
  mutate_if(is.numeric, round, 0)

pop_mun <- pop_mun %>% 
  mutate(prop_black = Black.African / Total)


# SUBSET EACH DATAFRAME TO INCLUDE ONLY THOSE VARIABLES NEEDED ----------------------------

employ_mun_sub <- employ_mun %>% 
  select(local_municipality_id, unemploy_rate)

lang_mun_sub <- lang_mun %>% 
  select(local_municipality_id, prop_english)

dwell_mun_sub <- dwell_mun %>% 
  select(local_municipality_id, prop_informal)

pop_mun_sub <- pop_mun %>% 
  select(local_municipality_id, prop_black)


# MERGE TOGETHER USING RBIND ----------------------------
df_controls_mun <- employ_mun_sub %>% 
  inner_join(lang_mun_sub, by = "local_municipality_id")

df_controls_mun <- df_controls_mun %>% 
  inner_join(dwell_mun_sub, by = "local_municipality_id")

df_controls_mun <- df_controls_mun %>% 
  left_join(pop_mun_sub, by = "local_municipality_id")




# NOW SYNC THESE VALUES WITH 2011 MUN BOUNDARIES (OFFICIAL MUN BOUNDARIES MOST TEMPORALLY PROXIMATE TO 2006) 
mun_2011 <- st_read(here("data", "gis_data_raw", "MDB_Local_Municipal_Boundary_2011.shp"))
mun_2011 <- st_transform(mun_2011, 6148)

mun_2011 <- mun_2011 %>% 
  rename(local_municipality_id = LocalMunic) %>% 
  select(local_municipality_id, geometry)


# merge
controls_mun_2011 <- left_join(mun_2011, df_controls_mun, by = "local_municipality_id")

# now read in 2016 shape files
mun_2016 <- st_read(here("data", "gis_data_raw", "ecc03798-c4f1-42e2-a0e6-573c0c07c8542020410-1-1psvop3.0vyd.shp"))
mun_2016 <- st_transform(mun_2016, 6148)

# rename ward_id variable for subsequent merger
mun_2016 <- mun_2016 %>% 
  rename(local_municipality_id = LocalMunic)

# clean GIS geometries to prepare for merge
controls_mun_2011 <- st_as_sf(controls_mun_2011)
mun_2016 <- st_as_sf(mun_2016)

controls_mun_2011 <- st_make_valid(controls_mun_2011)
mun_2016 <- st_make_valid(mun_2016)

# areal interpolation to 2016 ward boundaries
df_areal <- aw_interpolate(mun_2016, 
                           tid = "local_municipality_id", 
                           source = controls_mun_2011,
                           sid = "local_municipality_id",
                           weight = "sum", 
                           output = "tibble",
                           intensive = c("unemploy_rate", "prop_english", "prop_informal", "prop_black"))


# select variables we would like to keep
df_areal_2 <- df_areal %>% 
  select(local_municipality_id, prop_black, prop_english, prop_informal, unemploy_rate)


# EXPORT DATA TO PROCESSED_DATA SUBDIRECTORY 
saveRDS(df_areal_2, here("data", "processed_data", "data_2011_controls_MUN.rds"))
