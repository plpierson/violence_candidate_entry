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

viol_2006_to_2011 <- viol_ward_sitting %>% 
  filter(date_of_attack > "2006-03-01" & date_of_attack < "2011-05-18")

# MANUALLY LINK 2006 WARD IDS TO 2016 WARD IDS USING CENTROID COORDINATES ------------------

#### WARD 1

print(viol_2006_to_2011$ward_id)
pnts1 <- ward2006 %>% 
  filter(ward_id == "52306006")

centroids1 <- as.data.frame(st_coordinates(st_centroid(pnts1$geometry)))

interpolate_centroids1 <- st_as_sf(centroids1, coords = c("X", "Y"), crs = st_crs(6148))

interpolate1 <- interpolate_centroids1 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts1$ward_id
interpolate1$area
  #so 52306006 is now 52307015


##### WARD 2

print(viol_2006_to_2011$ward_id)
pnts2 <- ward2006 %>% 
  filter(ward_id == "52901005")

centroids2 <- as.data.frame(st_coordinates(st_centroid(pnts2$geometry)))

interpolate_centroids2 <- st_as_sf(centroids2, coords = c("X", "Y"), crs = st_crs(6148))

interpolate2 <- interpolate_centroids2 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts2$ward_id
interpolate2$area
#so 52901005 is now still 52901005

##### WARD 3
print(viol_2006_to_2011$ward_id)
pnts3 <- ward2006 %>% 
  filter(ward_id == "52405005")

centroids3 <- as.data.frame(st_coordinates(st_centroid(pnts3$geometry)))

interpolate_centroids3 <- st_as_sf(centroids3, coords = c("X", "Y"), crs = st_crs(6148))

interpolate3 <- interpolate_centroids3 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts3$ward_id
interpolate3$area
#so 52405005 is still 52405005

##### WARD 4
print(viol_2006_to_2011$ward_id)
pnts4 <- ward2006 %>% 
  filter(ward_id == "52405009")

centroids4 <- as.data.frame(st_coordinates(st_centroid(pnts4$geometry)))

interpolate_centroids4 <- st_as_sf(centroids4, coords = c("X", "Y"), crs = st_crs(6148))

interpolate4 <- interpolate_centroids4 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts4$ward_id
interpolate4$area
#so 52405009 is now 52405003


##### WARD 5
print(viol_2006_to_2011$ward_id)
pnts5 <- ward2006 %>% 
  filter(ward_id == "52804015")

centroids5 <- as.data.frame(st_coordinates(st_centroid(pnts5$geometry)))

interpolate_centroids5 <- st_as_sf(centroids5, coords = c("X", "Y"), crs = st_crs(6148))

interpolate5 <- interpolate_centroids5 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts5$ward_id
interpolate5$area
#so 52804015 is still 52804015

##### WARD 6
print(viol_2006_to_2011$ward_id)
pnts6 <- ward2006 %>% 
  filter(ward_id == "52603005")

centroids6 <- as.data.frame(st_coordinates(st_centroid(pnts6$geometry)))

interpolate_centroids6 <- st_as_sf(centroids6, coords = c("X", "Y"), crs = st_crs(6148))

interpolate6 <- interpolate_centroids6 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts6$ward_id
interpolate6$area
#so 52603005 is still 52603005

##### WARD 7
print(viol_2006_to_2011$ward_id)
pnts7 <- ward2006 %>% 
  filter(ward_id == "52401003")

centroids7 <- as.data.frame(st_coordinates(st_centroid(pnts7$geometry)))

interpolate_centroids7 <- st_as_sf(centroids7, coords = c("X", "Y"), crs = st_crs(6148))

interpolate7 <- interpolate_centroids7 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts7$ward_id
interpolate7$area
#so 52401003 is now 52401001


##### WARD 8
print(viol_2006_to_2011$ward_id)
pnts8 <- ward2006 %>% 
  filter(ward_id == "83007003")

centroids8 <- as.data.frame(st_coordinates(st_centroid(pnts8$geometry)))

interpolate_centroids8 <- st_as_sf(centroids8, coords = c("X", "Y"), crs = st_crs(6148))

interpolate8 <- interpolate_centroids8 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts8$ward_id
interpolate8$area
#so 83007003 is now 83007002



##### WARD 9
print(viol_2006_to_2011$ward_id)
pnts9 <- ward2006 %>% 
  filter(ward_id == "19100086")

centroids9 <- as.data.frame(st_coordinates(st_centroid(pnts9$geometry)))

interpolate_centroids9 <- st_as_sf(centroids9, coords = c("X", "Y"), crs = st_crs(6148))

interpolate9 <- interpolate_centroids9 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts9$ward_id
interpolate9$area
#so 19100086 is still 19100086


##### WARD 10
print(viol_2006_to_2011$ward_id)
pnts10 <- ward2006 %>% 
  filter(ward_id == "59200068")

centroids10 <- as.data.frame(st_coordinates(st_centroid(pnts10$geometry)))

interpolate_centroids10 <- st_as_sf(centroids10, coords = c("X", "Y"), crs = st_crs(6148))

interpolate10 <- interpolate_centroids10 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts10$ward_id
interpolate10$area
#so 59200068 is now 59500075


##### WARD 11
print(viol_2006_to_2011$ward_id)
pnts11 <- ward2006 %>% 
  filter(ward_id == "52402007")

centroids11 <- as.data.frame(st_coordinates(st_centroid(pnts11$geometry)))

interpolate_centroids11 <- st_as_sf(centroids11, coords = c("X", "Y"), crs = st_crs(6148))

interpolate11 <- interpolate_centroids11 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts11$ward_id
interpolate11$area
#so 52402007 is still 52402007


##### WARD 12
print(viol_2006_to_2011$ward_id)
pnts12 <- ward2006 %>% 
  filter(ward_id == "52902004")

centroids12 <- as.data.frame(st_coordinates(st_centroid(pnts12$geometry)))

interpolate_centroids12 <- st_as_sf(centroids12, coords = c("X", "Y"), crs = st_crs(6148))

interpolate12 <- interpolate_centroids12 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts12$ward_id
interpolate12$area
#so 52902004 is now 52902021


##### WARD 13
print(viol_2006_to_2011$ward_id)
pnts13 <- ward2006 %>% 
  filter(ward_id == "52304007")

centroids13 <- as.data.frame(st_coordinates(st_centroid(pnts13$geometry)))

interpolate_centroids13 <- st_as_sf(centroids13, coords = c("X", "Y"), crs = st_crs(6148))

interpolate13 <- interpolate_centroids13 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts13$ward_id
interpolate13$area
#so 52304007 is now 52307022


##### WARD 14
print(viol_2006_to_2011$ward_id)
pnts14 <- ward2006 %>% 
  filter(ward_id == "64005007")

centroids14 <- as.data.frame(st_coordinates(st_centroid(pnts14$geometry)))

interpolate_centroids14 <- st_as_sf(centroids14, coords = c("X", "Y"), crs = st_crs(6148))

interpolate14 <- interpolate_centroids14 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts14$ward_id
interpolate14$area
#so 64005007 is now 74804007


##### WARD 15
print(viol_2006_to_2011$ward_id)
pnts15 <- ward2006 %>% 
  filter(ward_id == "52105003")

centroids15 <- as.data.frame(st_coordinates(st_centroid(pnts15$geometry)))

interpolate_centroids15 <- st_as_sf(centroids15, coords = c("X", "Y"), crs = st_crs(6148))

interpolate15 <- interpolate_centroids15 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts15$ward_id
interpolate15$area
#so 52105003 is now 52106030


##### WARD 16
print(viol_2006_to_2011$ward_id)
pnts16 <- ward2006 %>% 
  filter(ward_id == "52405011")

centroids16 <- as.data.frame(st_coordinates(st_centroid(pnts16$geometry)))

interpolate_centroids16 <- st_as_sf(centroids16, coords = c("X", "Y"), crs = st_crs(6148))

interpolate16 <- interpolate_centroids16 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts16$ward_id
interpolate16$area
#so 52405011 is now 52405006












# IDENTIFY WARDS THAT CHANGED BETWEEN 2006 LGE AND 2011 LGE ------------------
viol_2006_to_2011_ward_ids <- viol_2006_to_2011$ward_id
#these are the ward_ids for all wards that experienced an assassination attempt after the 2006 LGEs and
#prior to the 2011 LGEs. Some of these ward_ids remain unchanged between 2006 and 2011, while others 
#change due to redistricting, administrative rejigging, etc. First, let's see which ones don't change


# READ IN 2011 SHAPE FILE WITH WARD IDs ------------------
ward2011 <- st_read(here("data", "gis_data_raw", "Wards2011.shp"))

ward2011 <- ward2011 %>% 
  rename(ward_id = WardID)

# there are duplicates in the original file...filter appropriately
ward2011 <- ward2011 %>% 
  distinct(ward_id, .keep_all = TRUE)

ward2011 <- st_transform(ward2011, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)


# identify wards that did not change
ward_ids_unchanged <- ward2011 %>% 
  filter(ward_id %in% viol_2006_to_2011_ward_ids)

#upon visible inspection, we see that the two ward_ids that appear in 2006 but no longer appear in 2011 are
#the following: ward_id 64005007 and ward_id 59200068

#in order to get the appropriate centroid locations, I will first take the values that synced correctly and pull their
#centroid coordinates from the ward2011 shape file
centroids_vector_1 <- as.data.frame(st_coordinates(st_centroid(ward_ids_unchanged$geometry)))


#now need to extract the appropriate centroid coordinates for the two wards that did not sync with 2011 ward_ids...
#these two wards will need to be connected to the ward2006 shape files in order to extract the correct location
ward2006 <- st_read(here("data", "gis_data_raw", "Wards2006.shp"))
ward2006 <- ward2006 %>% 
  rename(ward_id = WARD_ID)

ward2006 <- st_transform(ward2006, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)
ward_id_changes <- ward2006 %>% 
  filter(ward_id == "64005007" | ward_id == "59200068") 

#now extract the centroid coordinates for these two wards
centroids_vector_2 <- as.data.frame(st_coordinates(st_centroid(ward_id_changes$geometry)))

#combine centroid vectors together to create one vector for all 17 ward observations that were treated between 2006 and 2011
centroids <- rbind(centroids_vector_1, centroids_vector_2)

#convert centroid vector to an sf_object 
pnts_sf <- st_as_sf(centroids, coords = c("X", "Y"), crs = st_crs(6148))

#find where these centroid coordinates intersect with wards in elec_data (the 2011 vd results that are in 2016 ward boundaries)
# first, need to merge elec_data with ward 2016 shape file

# read in 2016 geometry
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



#NOW READ IN THE CLEANED CANDIDATES DATA AND CREATE A VARIABLE FOR ASSASSINATION TREATMENT
data_2011 <- readRDS(here("data", "processed_data", "candidates_clean_2011_extrapolated_to_2016.RDS"))


# CREATE DUMMY VARIABLE FOR ANY EXPOSED UNITS ------------------
data_2011 <- data_2011 %>% 
  mutate(treat = ifelse(ward_id %in% pnts$ward_id, 1, 0))

# now need to manually fill in target_party, name, and gender data for these 16 wards
print(pnts$ward_id)

    #now the problem here is that 6 of these ward_ids no longer exist in 2016...so I am going to manually go 
    #thru and for each missing one find out where its centroid lands in the 2016 geometry

pnts_missing_1 <- ward2011 %>% 
  filter(ward_id == "52106030")

centroids_missing_1 <- as.data.frame(st_coordinates(st_centroid(pnts_missing_1$geometry)))

centroids_vector_2 <- as.data.frame(st_coordinates(st_centroid(ward_id_changes$geometry)))



pnts_sf <- st_as_sf(centroids, coords = c("X", "Y"), crs = st_crs(6148))



pnts_missing_1_interpolate <- pnts_missing_1 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts <- pnts %>% 
  rename(ward_id = area)

pnts <- as.data.frame(pnts)
pnts <- pnts %>% 
  select(ward_id)





data_2011 <- data_2011 %>% 
  mutate(target_party = case_when(
    ward_id == "19100086" ~ "ANC",
    ward_id == "52804015" ~ "IFP",
    ward_id == "83007003" ~ "ANC",
    ward_id == "52106030" ~ "",
    ward_id == "52307021" ~ "",
    ward_id == "52307015" ~ "",
    ward_id == "52401001" ~ "",
    ward_id == "52402007" ~ "IFP",
    ward_id == "52405005" ~ "IFP",
    ward_id == "52405009" ~ "IFP",
    ward_id == "52405011" ~ "IFP",
    ward_id == "52603005" ~ "ANC",
    ward_id == "52901005" ~ "IFP",
    ward_id == "52902004" ~ "ANC",
    ward_id == "59500075" ~ "",
    ward_id == "74804007" ~ ""
  )) %>% 
  mutate(target_name = case_when(
    ward_id == "19100086" ~ "Xolani Ronald Sotashe",
    ward_id == "52804015" ~ "Mfanafuthi Elliot Maphumulo",
    ward_id == "83007003" ~ "Thandi Pauline Mtsweni",
    ward_id == "52106030" ~ "",
    ward_id == "52307021" ~ "",
    ward_id == "52307015" ~ "",
    ward_id == "52401001" ~ "Petrous Nxele",
    ward_id == "52402007" ~ "Dumelani Jerome Zulu",
    ward_id == "52405005" ~ "Enoch Sibongiseni Shange",
    ward_id == "52405009" ~ "Emmanuel Ntuthuko Ngcobo",
    ward_id == "52405011" ~ "Mnziwami Benard Mbatha",
    ward_id == "52603005" ~ "Mfana Alpheus Xulu",
    ward_id == "52901005" ~ "Simon Dingindawo Shange",
    ward_id == "52902004" ~ "Preva Khumalo",
    ward_id == "59500075" ~ "",
    ward_id == "74804007" ~ ""
  )) %>% 
  mutate(target_gender = case_when(
    ward_id == "19100086" ~ "male",
    ward_id == "52804015" ~ "male",
    ward_id == "83007003" ~ "female",
    ward_id == "52106030" ~ "",
    ward_id == "52307021" ~ "",
    ward_id == "52307015" ~ "",
    ward_id == "52401001" ~ "",
    ward_id == "52402007" ~ "male",
    ward_id == "52405005" ~ "male",
    ward_id == "52405009" ~ "male",
    ward_id == "52405011" ~ "male",
    ward_id == "52603005" ~ "male",
    ward_id == "52901005" ~ "male",
    ward_id == "52902004" ~ "male",
    ward_id == "59500075" ~ "",
    ward_id == "74804007" ~ ""
  ))





# check to make sure this worked properly: 
length(which(data_2011$treat==1)) ###16 woohoo!


# WRITE OUT DATA TO PROCESSED_DATA FILE
saveRDS(data_2011, here("data", "processed_data", "data_2011_with_assassinations_merged.rds"))
