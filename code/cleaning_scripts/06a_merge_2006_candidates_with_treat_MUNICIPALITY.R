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

viol_2000_to_2006 <- viol_mun %>% 
  filter(date_of_attack > "2000-12-05" & date_of_attack < "2006-03-01")

# MANUALLY LINK 2006 WARD IDS TO 2016 WARD IDS USING CENTROID COORDINATES ------------------


# first we will need to read in 2006 and 2016 geometries
# read in 2006 geometry
mun2006 <- st_read(here("data", "gis_data_raw", "LocalMunicipalities2006.shp"))
mun2006 <- mun2006 %>% 
  rename(local_municipality_id = CAT_B)

mun2006 <- st_transform(mun2006, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)

mun2006$local_municipality_id <- str_replace(mun2006$local_municipality_id, "KZ", "KZN")

# read in 2016 geometry
mun_2016 <- st_read(here("data", "gis_data_raw", "ecc03798-c4f1-42e2-a0e6-573c0c07c8542020410-1-1psvop3.0vyd.shp"))

mun_2016 <- st_transform(mun_2016, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)
names(mun_2016)

mun_2016 <- mun_2016 %>% 
  select(-c(OBJECTID, ProvinceNa, DistrictMu, District_1, Year, Shape__Are, Shape__Len)) %>% 
  rename(province_id = ProvinceCo,
         local_municipality_id = LocalMunic,
         local_municipality_name = LocalMun_1)

# transform to sf object 
mun_2016 <- st_as_sf(mun_2016)




#### MUN 1
print(viol_2000_to_2006$local_municipality_id)
# ETH is still ETH

#### MUN 2
# ETH is still ETH

#### MUN 3 
# ETH is still ETH

#### MUN 4
#NW405 is now GT484

#### MUN 5 
print(viol_2000_to_2006$local_municipality_id)

pnts5 <- mun2006 %>% 
  filter(local_municipality_id == "KZN225")

centroids5 <- as.data.frame(st_coordinates(st_centroid(pnts5$geometry)))

interpolate_centroids5 <- st_as_sf(centroids5, coords = c("X", "Y"), crs = st_crs(6148))

interpolate5 <- interpolate_centroids5 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts5$local_municipality_id
interpolate5$area
#so KZN225 is still KZN225

#### MUN 6 
print(viol_2000_to_2006$local_municipality_id)

# ETH is still ETH


#### MUN 7
# ETH is still ETH

#### MUN 8
print(viol_2000_to_2006$local_municipality_id)

pnts8 <- mun2006 %>% 
  filter(local_municipality_id == "KZN281")

centroids8 <- as.data.frame(st_coordinates(st_centroid(pnts8$geometry)))

interpolate_centroids8 <- st_as_sf(centroids8, coords = c("X", "Y"), crs = st_crs(6148))

interpolate8 <- interpolate_centroids8 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts8$local_municipality_id
interpolate8$area
#so KZN281 is still KZN281


#### MUN 9
print(viol_2000_to_2006$local_municipality_id)

pnts9 <- mun2006 %>% 
  filter(local_municipality_id == "KZN236")

centroids9 <- as.data.frame(st_coordinates(st_centroid(pnts9$geometry)))

interpolate_centroids9 <- st_as_sf(centroids9, coords = c("X", "Y"), crs = st_crs(6148))

interpolate9 <- interpolate_centroids9 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts9$local_municipality_id
interpolate9$area
#so KZN236 is now KZN237


#### MUN 10
print(viol_2000_to_2006$local_municipality_id)

#so KZN236 is now KZN237


#### MUN 11
print(viol_2000_to_2006$local_municipality_id)

pnts11 <- mun2006 %>% 
  filter(local_municipality_id == "KZN265")

centroids11 <- as.data.frame(st_coordinates(st_centroid(pnts11$geometry)))

interpolate_centroids11 <- st_as_sf(centroids11, coords = c("X", "Y"), crs = st_crs(6148))

interpolate11 <- interpolate_centroids11 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts11$local_municipality_id
interpolate11$area
#so KZN265 is still KZN265

#### MUN 12
print(viol_2000_to_2006$local_municipality_id)

pnts12 <- mun2006 %>% 
  filter(local_municipality_id == "KZN234")

centroids12 <- as.data.frame(st_coordinates(st_centroid(pnts12$geometry)))

interpolate_centroids12 <- st_as_sf(centroids12, coords = c("X", "Y"), crs = st_crs(6148))

interpolate12 <- interpolate_centroids12 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts12$local_municipality_id
interpolate12$area
#so KZN234 is now KZN237


#### MUN 13
print(viol_2000_to_2006$local_municipality_id)

pnts13 <- mun2006 %>% 
  filter(local_municipality_id == "KZN272")

centroids13 <- as.data.frame(st_coordinates(st_centroid(pnts13$geometry)))

interpolate_centroids13 <- st_as_sf(centroids13, coords = c("X", "Y"), crs = st_crs(6148))

interpolate13 <- interpolate_centroids13 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts13$local_municipality_id
interpolate13$area
#so KZN272 is still KZN272

#### MUN 14
print(viol_2000_to_2006$local_municipality_id)

#ETH is still ETH


#### MUN 15
print(viol_2000_to_2006$local_municipality_id)

pnts15 <- mun2006 %>% 
  filter(local_municipality_id == "KZN291")

centroids15 <- as.data.frame(st_coordinates(st_centroid(pnts15$geometry)))

interpolate_centroids15 <- st_as_sf(centroids15, coords = c("X", "Y"), crs = st_crs(6148))

interpolate15 <- interpolate_centroids15 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts15$local_municipality_id
interpolate15$area
#so KZN291 is still KZN291



#### MUN 16
print(viol_2000_to_2006$local_municipality_id)

# ETH is still ETH

#### MUN 17 
# ETH is still ETH


#### MUN 18
print(viol_2000_to_2006$local_municipality_id)

pnts18 <- mun2006 %>% 
  filter(local_municipality_id == "KZN232")

centroids18 <- as.data.frame(st_coordinates(st_centroid(pnts18$geometry)))

interpolate_centroids18 <- st_as_sf(centroids18, coords = c("X", "Y"), crs = st_crs(6148))

interpolate18 <- interpolate_centroids18 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts18$local_municipality_id
interpolate18$area
#so KZN232 is now KZN238


#### MUN 19
print(viol_2000_to_2006$local_municipality_id)

# CPT is still CPT


#### MUN 20
print(viol_2000_to_2006$local_municipality_id)

pnts20 <- mun2006 %>% 
  filter(local_municipality_id == "KZN244")

centroids20 <- as.data.frame(st_coordinates(st_centroid(pnts20$geometry)))

interpolate_centroids20 <- st_as_sf(centroids20, coords = c("X", "Y"), crs = st_crs(6148))

interpolate20 <- interpolate_centroids20 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts20$local_municipality_id
interpolate20$area
#so KZN244 is still KZN244



#### MUN 21
print(viol_2000_to_2006$local_municipality_id)

pnts21 <- mun2006 %>% 
  filter(local_municipality_id == "EC137")

centroids21 <- as.data.frame(st_coordinates(st_centroid(pnts21$geometry)))

interpolate_centroids21 <- st_as_sf(centroids21, coords = c("X", "Y"), crs = st_crs(6148))

interpolate21 <- interpolate_centroids21 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts21$local_municipality_id
interpolate21$area
#so EC137 is still EC137



#### MUN 22
print(viol_2000_to_2006$local_municipality_id)

# CPT is still CPT

#### MUN 23
# CPT is still CPT


#### MUN 24
# CPT is still CPT


#### MUN 25
# TSH is still TSH 


#### MUN 26
# KZN291 is still KZN291


#### MUN 27
print(viol_2000_to_2006$local_municipality_id)

pnts27 <- mun2006 %>% 
  filter(local_municipality_id == "KZN221")

centroids27 <- as.data.frame(st_coordinates(st_centroid(pnts27$geometry)))

interpolate_centroids27 <- st_as_sf(centroids27, coords = c("X", "Y"), crs = st_crs(6148))

interpolate27 <- interpolate_centroids27 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts27$local_municipality_id
interpolate27$area
#so KZN221 is still KZN221


#### MUN 28 
#KZN232 is now KZN238


#NOW READ IN THE CLEANED CANDIDATES DATA AND CREATE A VARIABLE FOR ASSASSINATION TREATMENT
data_2006 <- readRDS(here("data", "processed_data", "candidates_clean_2006_interpolated_to_2016_MUNICIPALITY.RDS"))


# CREATE DUMMY VARIABLE FOR ANY EXPOSED UNITS ------------------
data_2006 <- data_2006 %>% 
  mutate(treat = case_when(
    local_municipality_id == "ETH" ~ 1,
    local_municipality_id == "GT484" ~ 1,
    local_municipality_id == "KZN225" ~ 1,
    local_municipality_id == "KZN281" ~ 1,
    local_municipality_id == "KZN237" ~ 1,
    local_municipality_id == "KZN265" ~ 1,
    local_municipality_id == "KZN272" ~ 1,
    local_municipality_id == "KZN291" ~ 1,
    local_municipality_id == "KZN238" ~ 1,
    local_municipality_id == "CPT" ~ 1,
    local_municipality_id == "KZN244" ~ 1,
    local_municipality_id == "EC137" ~ 1,
    local_municipality_id == "TSH" ~ 1,
    local_municipality_id == "KZN221" ~ 1
  )) 



# check to make sure this worked properly: 
length(which(data_2006$treat==1)) ###14


# WRITE OUT DATA TO PROCESSED_DATA FILE
saveRDS(data_2006, here("data", "processed_data", "data_2006_with_assassinations_merged_MUNICIPALITY.rds"))



