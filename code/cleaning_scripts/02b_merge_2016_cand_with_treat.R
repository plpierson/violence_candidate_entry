rm(list=ls())

library(tidyverse)
library(here)
library(sf)
library(readxl)
library(numform)


# READ IN ASSASSINATIONS DATA
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

viol_2011_to_2016 <- viol_ward_sitting %>% 
  filter(date_of_attack > "2011-05-18" & date_of_attack < "2016-08-03")


##### NOW READ IN SHAPE FILES
  # READ IN 2016 WARD BOUNDARY SHAPE FILE
ward_2016 <- st_read(here("data", "gis_data_raw", "wards2016.shp"))

  #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)
ward_2016 <- st_transform(ward_2016, 6148)

names(ward_2016)

ward_2016 <- ward_2016 %>% 
  select(-c(OBJECTID, Shape__Are, Shape__Len, DistrictMu, District_1, ProvinceNa, WardNumber, Year, LocalMun_1, LocalMunic)) %>% 
  rename(province_id = ProvinceCo,
         ward_id = WardID)


  # READ IN 2011 WARD BOUNDARY SHAPE FILE
ward_2011 <- st_read(here("data", "gis_data_raw", "wards2011.shp"))

names(ward_2011)
ward_2011 <- ward_2011 %>% 
  rename(ward_id = WardID)

ward_2011 <- ward_2011 %>% 
  distinct(ward_id, .keep_all = TRUE)

ward_2011 <- st_transform(ward_2011, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)


##########################################################
########### NOW WE CAN MANUALLY SYNC COORDINATES #########
##########################################################

#### WARD 1
print(viol_2011_to_2016$ward_id)
pnts1 <- ward_2011 %>% 
  filter(ward_id == "52101010")

centroids1 <- as.data.frame(st_coordinates(st_centroid(pnts1$geometry)))

interpolate_centroids1 <- st_as_sf(centroids1, coords = c("X", "Y"), crs = st_crs(6148))

interpolate1 <- interpolate_centroids1 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts1$ward_id
interpolate1$area
#so 52101010 is now 52102006


#### WARD 2
print(viol_2011_to_2016$ward_id)
pnts2 <- ward_2011 %>% 
  filter(ward_id == "52605010")

centroids2 <- as.data.frame(st_coordinates(st_centroid(pnts2$geometry)))

interpolate_centroids2 <- st_as_sf(centroids2, coords = c("X", "Y"), crs = st_crs(6148))

interpolate2 <- interpolate_centroids2 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts2$ward_id
interpolate2$area
#so 52605010 is still 52605010


#### WARD 3
print(viol_2011_to_2016$ward_id)
pnts3 <- ward_2011 %>% 
  filter(ward_id == "94702010")

centroids3 <- as.data.frame(st_coordinates(st_centroid(pnts3$geometry)))

interpolate_centroids3 <- st_as_sf(centroids3, coords = c("X", "Y"), crs = st_crs(6148))

interpolate3 <- interpolate_centroids3 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts3$ward_id
interpolate3$area
#so 94702010 is still 94702010


#### WARD 4
print(viol_2011_to_2016$ward_id)
pnts4 <- ward_2011 %>% 
  filter(ward_id == "79800119")

centroids4 <- as.data.frame(st_coordinates(st_centroid(pnts4$geometry)))

interpolate_centroids4 <- st_as_sf(centroids4, coords = c("X", "Y"), crs = st_crs(6148))

interpolate4 <- interpolate_centroids4 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts4$ward_id
interpolate4$area
#so 79800119 is still 79800119


#### WARD 5
print(viol_2011_to_2016$ward_id)
pnts5 <- ward_2011 %>% 
  filter(ward_id == "79800051")

centroids5 <- as.data.frame(st_coordinates(st_centroid(pnts5$geometry)))

interpolate_centroids5 <- st_as_sf(centroids5, coords = c("X", "Y"), crs = st_crs(6148))

interpolate5 <- interpolate_centroids5 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts5$ward_id
interpolate5$area
#so 79800051 is still 79800051


#### WARD 6
print(viol_2011_to_2016$ward_id)
pnts6 <- ward_2011 %>% 
  filter(ward_id == "59500039")

centroids6 <- as.data.frame(st_coordinates(st_centroid(pnts6$geometry)))

interpolate_centroids6 <- st_as_sf(centroids6, coords = c("X", "Y"), crs = st_crs(6148))

interpolate6 <- interpolate_centroids6 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts6$ward_id
interpolate6$area
#so 59500039 is still 59500039



#### WARD 7
print(viol_2011_to_2016$ward_id)
pnts7 <- ward_2011 %>% 
  filter(ward_id == "59500072")

centroids7 <- as.data.frame(st_coordinates(st_centroid(pnts7$geometry)))

interpolate_centroids7 <- st_as_sf(centroids7, coords = c("X", "Y"), crs = st_crs(6148))

interpolate7 <- interpolate_centroids7 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts7$ward_id
interpolate7$area
#so 59500072 is still 59500072


#### WARD 8
print(viol_2011_to_2016$ward_id)
pnts8 <- ward_2011 %>% 
  filter(ward_id == "52304008")

centroids8 <- as.data.frame(st_coordinates(st_centroid(pnts8$geometry)))

interpolate_centroids8 <- st_as_sf(centroids8, coords = c("X", "Y"), crs = st_crs(6148))

interpolate8 <- interpolate_centroids8 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts8$ward_id
interpolate8$area
#so 52304008 is now 52307019



#### WARD 9
print(viol_2011_to_2016$ward_id)
pnts9 <- ward_2011 %>% 
  filter(ward_id == "52306002")

centroids9 <- as.data.frame(st_coordinates(st_centroid(pnts9$geometry)))

interpolate_centroids9 <- st_as_sf(centroids9, coords = c("X", "Y"), crs = st_crs(6148))

interpolate9 <- interpolate_centroids9 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts9$ward_id
interpolate9$area
#so 52306002 is now 52307012


#NOW READ IN THE CLEANED CANDIDATES DATA AND CREATE A VARIABLE FOR ASSASSINATION TREATMENT
cand_data <- readRDS(here("data", "processed_data", "candidates_clean_2016.RDS"))


# CREATE DUMMY VARIABLE FOR ANY TREATED UNITS ------------------
cand_data <- cand_data %>% 
  mutate(treat = case_when(
    ward_id == "52102006" ~ 1,
    ward_id == "52605010" ~ 1,
    ward_id == "94702010" ~ 1,
    ward_id == "79800119" ~ 1,
    ward_id == "79800051" ~ 1,
    ward_id == "59500039" ~ 1,
    ward_id == "59500072" ~ 1,
    ward_id == "52307019" ~ 1,
    ward_id == "52307012" ~ 1
  )) %>% 
  mutate(target_party = case_when(
    ward_id == "52102006" ~ "ANC",
    ward_id == "52605010" ~ "ANC",
    ward_id == "94702010" ~ "Mpumalanga Party",
    ward_id == "79800119" ~ "ANC",
    ward_id == "79800051" ~ "ANC",
    ward_id == "59500039" ~ "IFP",
    ward_id == "59500072" ~ "ANC",
    ward_id == "52307019" ~ "ANC",
    ward_id == "52307012" ~ "ANC"
  )) %>% 
  mutate(target_name = case_when(
    ward_id == "52102006" ~ "Mziwendoda David Ncwane",
    ward_id == "52605010" ~ "Goodwill Bhekithemba Nyembe",
    ward_id == "94702010" ~ "Fanie Motshele Mogotji",
    ward_id == "79800119" ~ "Mbuyiselo Dokolwane",
    ward_id == "79800051" ~ "Siphiwe Zulu",
    ward_id == "59500039" ~ "Thembokwakhe Emmanuel Xulu",
    ward_id == "59500072" ~ "Mthembeni Shezi",
    ward_id == "52307019" ~ "Jimmy Jabulane Lembethe",
    ward_id == "52307012" ~ "Vincent Phelelani Gumbi"
  )) %>% 
  mutate(target_gender = case_when(
    ward_id == "52102006" ~ "male",
    ward_id == "52605010" ~ "male",
    ward_id == "94702010" ~ "male",
    ward_id == "79800119" ~ "male",
    ward_id == "79800051" ~ "male",
    ward_id == "59500039" ~ "male",
    ward_id == "59500072" ~ "male",
    ward_id == "52307019" ~ "male",
    ward_id == "52307012" ~ "male"
  )) %>% 
  mutate(local_municipality_id = case_when(
    ward_id == "52102006" ~ "KZN211",
    ward_id == "52605010" ~ "KZN265",
    ward_id == "94702010" ~ "LIM472",
    ward_id == "79800119" ~ "JHB",
    ward_id == "79800051" ~ "JHB",
    ward_id == "59500039" ~ "ETH",
    ward_id == "59500072" ~ "ETH",
    ward_id == "52307019" ~ "KZN234",
    ward_id == "52307012" ~ "KZN236"
  ))



# check to make sure this worked properly: 
length(which(cand_data$treat==1)) ###9 woohoo!


#export
saveRDS(cand_data, 
        here("data", "processed_data", "data_2016_with_assassinations_merged.rds"))
