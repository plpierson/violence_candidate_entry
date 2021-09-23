rm(list=ls())

library(tidyverse)
library(sf)
library(readxl)
library(lubridate)
library(numform)
library(here)

options(scipen = 999) # prevent defaulting to scientific notation for longer numeric variable values (such as ID numbers)



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

# SUBSET TO ELECTORAL CYCLE BETWEEN 2000 AND 2006 ------------------
viol_2000_to_2006 <- viol_ward_sitting %>% 
  filter(date_of_attack > "2000-12-06" & date_of_attack < "2006-03-01")


# MANUALLY LINK 2006 WARD IDS TO 2016 WARD IDS USING CENTROID COORDINATES ------------------

# first we will need to read in 2006 and 2016 geometries
# read in 2006 geomtry
ward2006 <- st_read(here("data", "gis_data_raw", "Wards2006.shp"))
ward2006 <- ward2006 %>% 
  rename(ward_id = WARD_ID)

ward2006 <- st_transform(ward2006, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)


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



#### WARD 1
print(viol_2000_to_2006$ward_id)
pnts1 <- ward2006 %>% 
  filter(ward_id == "59200089")

centroids1 <- as.data.frame(st_coordinates(st_centroid(pnts1$geometry)))

interpolate_centroids1 <- st_as_sf(centroids1, coords = c("X", "Y"), crs = st_crs(6148))

interpolate1 <- interpolate_centroids1 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts1$ward_id
interpolate1$area
#so 59200089 is now 59500089


#### WARD 2
print(viol_2000_to_2006$ward_id)
pnts2 <- ward2006 %>% 
  filter(ward_id == "59200062")

centroids2 <- as.data.frame(st_coordinates(st_centroid(pnts2$geometry)))

interpolate_centroids2 <- st_as_sf(centroids2, coords = c("X", "Y"), crs = st_crs(6148))

interpolate2 <- interpolate_centroids2 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts2$ward_id
interpolate2$area
#so 59200062 is now 59500062


#### WARD 3
print(viol_2000_to_2006$ward_id)
pnts3 <- ward2006 %>% 
  filter(ward_id == "59200075")

centroids3 <- as.data.frame(st_coordinates(st_centroid(pnts3$geometry)))

interpolate_centroids3 <- st_as_sf(centroids3, coords = c("X", "Y"), crs = st_crs(6148))

interpolate3 <- interpolate_centroids3 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts3$ward_id
interpolate3$area
#so 59200075 is now 59500075


#### WARD 4
print(viol_2000_to_2006$ward_id)
pnts4 <- ward2006 %>% 
  filter(ward_id == "52205001")

centroids4 <- as.data.frame(st_coordinates(st_centroid(pnts4$geometry)))

interpolate_centroids4 <- st_as_sf(centroids4, coords = c("X", "Y"), crs = st_crs(6148))

interpolate4 <- interpolate_centroids4 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts4$ward_id
interpolate4$area
#so 52205001 is still 52205001


#### WARD 5
print(viol_2000_to_2006$ward_id)
pnts5 <- ward2006 %>% 
  filter(ward_id == "59200039")

centroids5 <- as.data.frame(st_coordinates(st_centroid(pnts5$geometry)))

interpolate_centroids5 <- st_as_sf(centroids5, coords = c("X", "Y"), crs = st_crs(6148))

interpolate5 <- interpolate_centroids5 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts5$ward_id
interpolate5$area
#so 59200039 is now 59500039


#### WARD 6
print(viol_2000_to_2006$ward_id)
pnts6 <- ward2006 %>% 
  filter(ward_id == "59200082")

centroids6 <- as.data.frame(st_coordinates(st_centroid(pnts6$geometry)))

interpolate_centroids6 <- st_as_sf(centroids6, coords = c("X", "Y"), crs = st_crs(6148))

interpolate6 <- interpolate_centroids6 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts6$ward_id
interpolate6$area
#so 59200082 is now 59500082

#### WARD 7
print(viol_2000_to_2006$ward_id)
pnts7 <- ward2006 %>% 
  filter(ward_id == "52801003")

centroids7 <- as.data.frame(st_coordinates(st_centroid(pnts7$geometry)))

interpolate_centroids7 <- st_as_sf(centroids7, coords = c("X", "Y"), crs = st_crs(6148))

interpolate7 <- interpolate_centroids7 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts7$ward_id
interpolate7$area
#so 52801003 is now 52801004


#### WARD 8
print(viol_2000_to_2006$ward_id)
pnts8 <- ward2006 %>% 
  filter(ward_id == "52306007")

centroids8 <- as.data.frame(st_coordinates(st_centroid(pnts8$geometry)))

interpolate_centroids8 <- st_as_sf(centroids8, coords = c("X", "Y"), crs = st_crs(6148))

interpolate8 <- interpolate_centroids8 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts8$ward_id
interpolate8$area
#so 52306007 is now 52307016


#### WARD 9
print(viol_2000_to_2006$ward_id)
pnts9 <- ward2006 %>% 
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


#### WARD 10
print(viol_2000_to_2006$ward_id)
pnts10 <- ward2006 %>% 
  filter(ward_id == "52304004")

centroids10 <- as.data.frame(st_coordinates(st_centroid(pnts10$geometry)))

interpolate_centroids10 <- st_as_sf(centroids10, coords = c("X", "Y"), crs = st_crs(6148))

interpolate10 <- interpolate_centroids10 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts10$ward_id
interpolate10$area
#so 52304004 is now 52307019


#### WARD 11
print(viol_2000_to_2006$ward_id)
pnts11 <- ward2006 %>% 
  filter(ward_id == "52702015")

centroids11 <- as.data.frame(st_coordinates(st_centroid(pnts11$geometry)))

interpolate_centroids11 <- st_as_sf(centroids11, coords = c("X", "Y"), crs = st_crs(6148))

interpolate11 <- interpolate_centroids11 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts11$ward_id
interpolate11$area
#so 52702015 is still 52702015


#### WARD 12
print(viol_2000_to_2006$ward_id)
pnts12 <- ward2006 %>% 
  filter(ward_id == "52901015")

centroids12 <- as.data.frame(st_coordinates(st_centroid(pnts12$geometry)))

interpolate_centroids12 <- st_as_sf(centroids12, coords = c("X", "Y"), crs = st_crs(6148))

interpolate12 <- interpolate_centroids12 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts12$ward_id
interpolate12$area
#so 52901015 is still 52901015


#### WARD 13
print(viol_2000_to_2006$ward_id)
pnts13 <- ward2006 %>% 
  filter(ward_id == "59200012")

centroids13 <- as.data.frame(st_coordinates(st_centroid(pnts13$geometry)))

interpolate_centroids13 <- st_as_sf(centroids13, coords = c("X", "Y"), crs = st_crs(6148))

interpolate13 <- interpolate_centroids13 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts13$ward_id
interpolate13$area
#so 59200012 is now 59500012


#### WARD 14
print(viol_2000_to_2006$ward_id)
pnts14 <- ward2006 %>% 
  filter(ward_id == "19100036")

centroids14 <- as.data.frame(st_coordinates(st_centroid(pnts14$geometry)))

interpolate_centroids14 <- st_as_sf(centroids14, coords = c("X", "Y"), crs = st_crs(6148))

interpolate14 <- interpolate_centroids14 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts14$ward_id
interpolate14$area
#so 19100036 is still 19100036


#### WARD 15
print(viol_2000_to_2006$ward_id)
pnts15 <- ward2006 %>% 
  filter(ward_id == "19100035")

centroids15 <- as.data.frame(st_coordinates(st_centroid(pnts15$geometry)))

interpolate_centroids15 <- st_as_sf(centroids15, coords = c("X", "Y"), crs = st_crs(6148))

interpolate15 <- interpolate_centroids15 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts15$ward_id
interpolate15$area
#so 19100035 is still 19100035



#### WARD 16
print(viol_2000_to_2006$ward_id)
pnts16 <- ward2006 %>% 
  filter(ward_id == "19100075")

centroids16 <- as.data.frame(st_coordinates(st_centroid(pnts16$geometry)))

interpolate_centroids16 <- st_as_sf(centroids16, coords = c("X", "Y"), crs = st_crs(6148))

interpolate16 <- interpolate_centroids16 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts16$ward_id
interpolate16$area
#so 19100075 is still 19100075


#### WARD 17
print(viol_2000_to_2006$ward_id)
pnts17 <- ward2006 %>% 
  filter(ward_id == "19100034")

centroids17 <- as.data.frame(st_coordinates(st_centroid(pnts17$geometry)))

interpolate_centroids17 <- st_as_sf(centroids17, coords = c("X", "Y"), crs = st_crs(6148))

interpolate17 <- interpolate_centroids17 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts17$ward_id
interpolate17$area
#so 19100034 is still 19100034


#### WARD 18
print(viol_2000_to_2006$ward_id)
pnts18 <- ward2006 %>% 
  filter(ward_id == "52302008")

centroids18 <- as.data.frame(st_coordinates(st_centroid(pnts18$geometry)))

interpolate_centroids18 <- st_as_sf(centroids18, coords = c("X", "Y"), crs = st_crs(6148))

interpolate18 <- interpolate_centroids18 %>% mutate(
  intersection = as.integer(st_intersects(geometry, ward_2016)),
  area = if_else(is.na(intersection), '', ward_2016$ward_id[intersection])
)

pnts18$ward_id
interpolate18$area
#so 52302008 is now 52308008



#NOW READ IN THE CLEANED CANDIDATES DATA AND CREATE A VARIABLE FOR ASSASSINAATION TREATMENT
data_2006 <- readRDS(here("data", "processed_data", "candidates_clean_2006_interpolated_to_2016.RDS"))



# CREATE DUMMY VARIABLE FOR ANY EXPOSED UNITS ------------------
data_2006 <- data_2006 %>% 
  mutate(treat = case_when(
    ward_id == "59500089" ~ 1,
    ward_id == "59500062" ~ 1,
    ward_id == "59500075" ~ 1,
    ward_id == "52205001" ~ 1,
    ward_id == "59500039" ~ 1,
    ward_id == "59500082" ~ 1,
    ward_id == "52801004" ~ 1,
    ward_id == "52307016" ~ 1,
    ward_id == "52307012" ~ 1,
    ward_id == "52307019" ~ 1,
    ward_id == "52702015" ~ 1,
    ward_id == "52901015" ~ 1,
    ward_id == "59500012" ~ 1,
    ward_id == "19100036" ~ 1,
    ward_id == "19100035" ~ 1,
    ward_id == "19100075" ~ 1,
    ward_id == "19100034" ~ 1,
    ward_id == "52308008" ~ 1
  )) %>% 
  mutate(target_party = case_when(
    ward_id == "59500089" ~ "IFP",
    ward_id == "59500062" ~ "ANC",
    ward_id == "59500075" ~ "ANC",
    ward_id == "52205001" ~ "IFP",
    ward_id == "59500039" ~ "IFP",
    ward_id == "59500082" ~ "ANC",
    ward_id == "52801004" ~ "IFP",
    ward_id == "52307016" ~ "IFP",
    ward_id == "52307012" ~ "IFP",
    ward_id == "52307019" ~ "Independent",
    ward_id == "52702015" ~ "ANC",
    ward_id == "52901015" ~ "ANC",
    ward_id == "59500012" ~ "ANC",
    ward_id == "19100036" ~ "ANC",
    ward_id == "19100035" ~ "ANC",
    ward_id == "19100075" ~ "ANC",
    ward_id == "19100034" ~ "ANC",
    ward_id == "52308008" ~ "ANC"
  )) %>% 
  mutate(target_name = case_when(
    ward_id == "59500089" ~ "Bhekinkosi Mhlongo",
    ward_id == "59500062" ~ "James Msikelwa Mthembu",
    ward_id == "59500075" ~ "Khuzimpi Raphael Shezi",
    ward_id == "52205001" ~ "Vika Ndlovu",
    ward_id == "59500039" ~ "Lueky Mzwamandla Zondo",
    ward_id == "59500082" ~ "Vincent Zwelakhe Madlala",
    ward_id == "52801004" ~ "Phineas Dumakwake Mthethwa",
    ward_id == "52307016" ~ "Mandlakayise Clement Mabaso",
    ward_id == "52307012" ~ "Sipho Patrick Bhengu",
    ward_id == "52307019" ~ "Kasaven Kathervaloo Chetty",
    ward_id == "52702015" ~ "Busakahle Alson Thabethe",
    ward_id == "52901015" ~ "Richard Mthembu",
    ward_id == "59500012" ~ "Siphiwe Wilberforce Phansi",
    ward_id == "19100036" ~ "Depoutch Elese",
    ward_id == "19100035" ~ "Sindephi Lose Cuba",
    ward_id == "19100075" ~ "Wandile Andie Nkwele",
    ward_id == "19100034" ~ "Mteto David Ntlanganiso",
    ward_id == "52308008" ~ "Mayiboyi Vincent Madlala"
  )) %>% 
  mutate(target_gender = case_when(
    ward_id == "59500089" ~ "male",
    ward_id == "59500062" ~ "male",
    ward_id == "59500075" ~ "male",
    ward_id == "52205001" ~ "male",
    ward_id == "59500039" ~ "male",
    ward_id == "59500082" ~ "male",
    ward_id == "52801004" ~ "male",
    ward_id == "52307016" ~ "male",
    ward_id == "52307012" ~ "male",
    ward_id == "52307019" ~ "male",
    ward_id == "52702015" ~ "male",
    ward_id == "52901015" ~ "male",
    ward_id == "59500012" ~ "male",
    ward_id == "19100036" ~ "male",
    ward_id == "19100035" ~ "male",
    ward_id == "19100075" ~ "male",
    ward_id == "19100034" ~ "male",
    ward_id == "52308008" ~ "male"
  )) %>% 
  mutate(local_municipality_id = case_when(
    ward_id == "59500089" ~ "ETH",
    ward_id == "59500062" ~ "ETH",
    ward_id == "59500075" ~ "ETH",
    ward_id == "52205001" ~ "KZN225",
    ward_id == "59500039" ~ "ETH",
    ward_id == "59500082" ~ "ETH",
    ward_id == "52801004" ~ "KZN281",
    ward_id == "52307016" ~ "KZN236",
    ward_id == "52307012" ~ "KZN236",
    ward_id == "52307019" ~ "KZN234",
    ward_id == "52702015" ~ "KZN272",
    ward_id == "52901015" ~ "KZN291",
    ward_id == "59500012" ~ "ETH",
    ward_id == "19100036" ~ "CPT",
    ward_id == "19100035" ~ "CPT",
    ward_id == "19100075" ~ "CPT",
    ward_id == "19100034" ~ "CPT",
    ward_id == "52308008" ~ "KZN232"
  ))





# check to make sure this worked properly: 
length(which(data_2006$treat==1)) ###18 woohoo!


# WRITE OUT DATA TO PROCESSED_DATA FILE
saveRDS(data_2006, here("data", "processed_data", "data_2006_with_assassinations_merged.rds"))
