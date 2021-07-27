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
viol_ward <- viol %>% 
  filter(seat_type=="LC Ward" | seat_type=="MC Ward" | seat_type=="MC Ward candidate" | seat_type=="LC Ward candidate") %>% 
  filter(date_of_attack>="2000-12-05" & date_of_attack<="2016-08-03") 

viol_ward_sitting <- viol_ward %>% 
  filter(sitting == 1)

# CREATE DUMMY VARIABLES FOR ASSASSINATION EXPOSURE ------------------
viol_2000_to_2006 <- viol_ward_sitting %>% 
  filter(date_of_attack > "2000-12-06" & date_of_attack < "2006-03-01")


# IDENTIFY WARDS THAT CHANGED BETWEEN 2000 LGE AND 2006 LGE ------------------
viol_2000_to_2006_ward_ids <- viol_2000_to_2006$ward_id
#these are the ward_ids for all wards that experienced an assassination attempt after the 2000 LGEs and
#prior to the 2006 LGEs. Some of these ward_ids remain unchanged between the two election cycles, while 
#others were changed due to redistricting, administrative rejigging, etc. First, let's see which ones don't change.

# READ IN 2006 SHAPE FILE WITH WARD IDs ------------------
ward2006 <- st_read(here("data", "gis_data_raw", "Wards2006.shp"))

ward2006 <- ward2006 %>% 
  rename(ward_id = WARD_ID)

ward2006 <- st_transform(ward2006, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)

# identify wards that did not change
ward_ids_unchanged <- ward2006 %>% 
  filter(ward_id %in% viol_2000_to_2006_ward_ids)
# woohoo! conveniently, none of these changed



# next step is to extract the centroid coordinates for each of these observations...to do this, we will take the centroid
# of each treated ward
centroids <- as.data.frame(st_coordinates(st_centroid(ward_ids_unchanged$geometry)))

# convert centroid vector to an sf object
pnts_sf <- st_as_sf(centroids, coords = c("X", "Y"), crs = st_crs(6148))

# find where thse centroid coordinates intersect with wards in elec_data (the 2006 vd results that are in 2016 ward boundaries)
# first, need to merge elec_data with ward 2016 shape file

ward_2016 <- st_read(here("data", "gis_data_raw", "Wards2016.shp"))

ward_2016 <- st_transform(ward_2016, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)
names(ward_2016)

ward_2016 <- ward_2016 %>% 
  select(-c(OBJECTID, Shape__Are, Shape__Len, DistrictMu, District_1, ProvinceNa, WardNumber, Year)) %>% 
  rename(province_id = ProvinceCo,
         local_municipality_id = LocalMunic,
         ward_id = WardID, 
         local_municipality_name = LocalMun_1)


# transform to sf object 
ward_2016 <- st_as_sf(ward_2016)


# now find intersection of 2006 LGE ward centroids with 2016 ward boundaries
pnts <- pnts_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts <- pnts %>% 
  rename(ward_id = area)

pnts <- as.data.frame(pnts)
pnts <- pnts %>% 
  select(ward_id)





#NOW READ IN THE CLEANED CANDIDATES DATA AND CREATE A VARIABLE FOR ASSASSINAATION TREATMENT
data_2006 <- readRDS(here("data", "processed_data", "candidates_clean_2006_interpolated_to_2016.RDS"))

# CREATE DUMMY VARIABLE FOR ANY EXPOSED UNITS ------------------
data_2006 <- data_2006 %>% 
  mutate(treat = ifelse(ward_id %in% pnts$ward_id, 1, 0))


# check to make sure this worked properly: 
length(which(data_2006$treat==1)) ###18 woohoo!


# WRITE OUT DATA TO PROCESSED_DATA FILE
saveRDS(data_2006, here("data", "processed_data", "data_2006_with_assassinations_merged.rds"))
