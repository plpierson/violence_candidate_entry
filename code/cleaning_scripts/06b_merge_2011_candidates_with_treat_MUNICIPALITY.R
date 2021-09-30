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

viol_2006_to_2011 <- viol_mun %>% 
  filter(date_of_attack > "2006-03-01" & date_of_attack < "2011-05-18")

# MANUALLY LINK 2006 WARD IDS TO 2016 WARD IDS USING CENTROID COORDINATES ------------------


# first we will need to read in 2006 and 2016 geometries
# read in 2006 geomtry
mun2011 <- st_read(here("data", "gis_data_raw", "MDB_Local_Municipal_Boundary_2011.shp"))
mun2011 <- mun2011 %>% 
  rename(local_municipality_id = LocalMunic)

mun2011 <- st_transform(mun2011, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)

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
print(viol_2006_to_2011$local_municipality_id)
pnts1 <- mun2006 %>% 
  filter(local_municipality_id == "KZN226")

centroids1 <- as.data.frame(st_coordinates(st_centroid(pnts1$geometry)))

interpolate_centroids1 <- st_as_sf(centroids1, coords = c("X", "Y"), crs = st_crs(6148))

interpolate1 <- interpolate_centroids1 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts1$local_municipality_id
interpolate1$area
#so KZN226 is now KZN226


#### MUN 2
print(viol_2006_to_2011$local_municipality_id)

#ETH is still ETH

#### MUN 3
print(viol_2006_to_2011$local_municipality_id)
pnts3 <- mun2006 %>% 
  filter(local_municipality_id == "KZN236")

centroids3 <- as.data.frame(st_coordinates(st_centroid(pnts3$geometry)))

interpolate_centroids3 <- st_as_sf(centroids3, coords = c("X", "Y"), crs = st_crs(6148))

interpolate3 <- interpolate_centroids3 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts3$local_municipality_id
interpolate3$area
#so KZN236 is now KZN237


#### MUN 4
print(viol_2006_to_2011$local_municipality_id)
pnts4 <- mun2006 %>% 
  filter(local_municipality_id == "KZN291")

centroids4 <- as.data.frame(st_coordinates(st_centroid(pnts4$geometry)))

interpolate_centroids4 <- st_as_sf(centroids4, coords = c("X", "Y"), crs = st_crs(6148))

interpolate4 <- interpolate_centroids4 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts4$local_municipality_id
interpolate4$area
#so KZN291 is still KZN291


#### MUN 5
print(viol_2006_to_2011$local_municipality_id)
pnts5 <- mun2006 %>% 
  filter(COMMON_NAM == "Carletonville")

centroids5 <- as.data.frame(st_coordinates(st_centroid(pnts5$geometry)))

interpolate_centroids5 <- st_as_sf(centroids5, coords = c("X", "Y"), crs = st_crs(6148))

interpolate5 <- interpolate_centroids5 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts5$local_municipality_id
interpolate5$area
#so Carletonville/CBLC8 is now GT484


#### MUN 6
print(viol_2006_to_2011$local_municipality_id)
pnts6 <- mun2006 %>% 
  filter(local_municipality_id == "FS171")

centroids6 <- as.data.frame(st_coordinates(st_centroid(pnts6$geometry)))

interpolate_centroids6 <- st_as_sf(centroids6, coords = c("X", "Y"), crs = st_crs(6148))

interpolate6 <- interpolate_centroids6 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts6$local_municipality_id
interpolate6$area
#so FS171 is now MAN


#### MUN 7
print(viol_2006_to_2011$local_municipality_id)
pnts7 <- mun2006 %>% 
  filter(local_municipality_id == "KZN245")

centroids7 <- as.data.frame(st_coordinates(st_centroid(pnts7$geometry)))

interpolate_centroids7 <- st_as_sf(centroids7, coords = c("X", "Y"), crs = st_crs(6148))

interpolate7 <- interpolate_centroids7 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts7$local_municipality_id
interpolate7$area
#so KZN245 is still KZN245


#### MUN 8

#KZN245 is still KZN245

#### MUN 9

#ETH is still ETH


#### MUN 10
print(viol_2006_to_2011$local_municipality_id)
pnts10 <- mun2006 %>% 
  filter(local_municipality_id == "NW373")

centroids10 <- as.data.frame(st_coordinates(st_centroid(pnts10$geometry)))

interpolate_centroids10 <- st_as_sf(centroids10, coords = c("X", "Y"), crs = st_crs(6148))

interpolate10 <- interpolate_centroids10 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts10$local_municipality_id
interpolate10$area
#so NW373 is still NW373


#### MUN 11
print(viol_2006_to_2011$local_municipality_id)
pnts11 <- mun2006 %>% 
  filter(local_municipality_id == "KZN284")

centroids11 <- as.data.frame(st_coordinates(st_centroid(pnts11$geometry)))

interpolate_centroids11 <- st_as_sf(centroids11, coords = c("X", "Y"), crs = st_crs(6148))

interpolate11 <- interpolate_centroids11 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts11$local_municipality_id
interpolate11$area
#so NKZN284 is still KZN284


#### MUN 12
print(viol_2006_to_2011$local_municipality_id)
pnts12 <- mun2006 %>% 
  filter(local_municipality_id == "MP322")

centroids12 <- as.data.frame(st_coordinates(st_centroid(pnts12$geometry)))

interpolate_centroids12 <- st_as_sf(centroids12, coords = c("X", "Y"), crs = st_crs(6148))

interpolate12 <- interpolate_centroids12 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts12$local_municipality_id
interpolate12$area
#so MP322 is now MP326


#### MUN 13
print(viol_2006_to_2011$local_municipality_id)
pnts13 <- mun2006 %>% 
  filter(local_municipality_id == "KZN263")

centroids13 <- as.data.frame(st_coordinates(st_centroid(pnts13$geometry)))

interpolate_centroids13 <- st_as_sf(centroids13, coords = c("X", "Y"), crs = st_crs(6148))

interpolate13 <- interpolate_centroids13 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts13$local_municipality_id
interpolate13$area
#so KZN263 is still KZN263

#### MUN 14
print(viol_2006_to_2011$local_municipality_id)
pnts14 <- mun2006 %>% 
  filter(local_municipality_id == "KZN241")

centroids14 <- as.data.frame(st_coordinates(st_centroid(pnts14$geometry)))

interpolate_centroids14 <- st_as_sf(centroids14, coords = c("X", "Y"), crs = st_crs(6148))

interpolate14 <- interpolate_centroids14 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts14$local_municipality_id
interpolate14$area
#so KZN241 is still KZN241

#### MUN 15
print(viol_2006_to_2011$local_municipality_id)
pnts15 <- mun2006 %>% 
  filter(local_municipality_id == "WC024")

centroids15 <- as.data.frame(st_coordinates(st_centroid(pnts15$geometry)))

interpolate_centroids15 <- st_as_sf(centroids15, coords = c("X", "Y"), crs = st_crs(6148))

interpolate15 <- interpolate_centroids15 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts15$local_municipality_id
interpolate15$area
#so WC024 is still WC024


#### MUN 16
print(viol_2006_to_2011$local_municipality_id)
pnts16 <- mun2006 %>% 
  filter(local_municipality_id == "FS204")

centroids16 <- as.data.frame(st_coordinates(st_centroid(pnts16$geometry)))

interpolate_centroids16 <- st_as_sf(centroids16, coords = c("X", "Y"), crs = st_crs(6148))

interpolate16 <- interpolate_centroids16 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts16$local_municipality_id
interpolate16$area
#so FS204 is still FS204


#### MUN 17
print(viol_2006_to_2011$local_municipality_id)
pnts17 <- mun2006 %>% 
  filter(local_municipality_id == "MP307")

centroids17 <- as.data.frame(st_coordinates(st_centroid(pnts17$geometry)))

interpolate_centroids17 <- st_as_sf(centroids17, coords = c("X", "Y"), crs = st_crs(6148))

interpolate17 <- interpolate_centroids17 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts17$local_municipality_id
interpolate17$area
#so MP307 is still MP307


#### MUN 18
#CPT is still CPT

#### MUN 19
#CPT is still CPT

#### MUN20
#ETH is still ETH

#### MUN 21
print(viol_2006_to_2011$local_municipality_id)
pnts21 <- mun2006 %>% 
  filter(local_municipality_id == "KZN242")

centroids21 <- as.data.frame(st_coordinates(st_centroid(pnts21$geometry)))

interpolate_centroids21 <- st_as_sf(centroids21, coords = c("X", "Y"), crs = st_crs(6148))

interpolate21 <- interpolate_centroids21 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts21$local_municipality_id
interpolate21$area
#so KZN242 is still KZN242

#### MUN 22
print(viol_2006_to_2011$local_municipality_id)
pnts22 <- mun2006 %>% 
  filter(local_municipality_id == "KZN292")

centroids22 <- as.data.frame(st_coordinates(st_centroid(pnts22$geometry)))

interpolate_centroids22 <- st_as_sf(centroids22, coords = c("X", "Y"), crs = st_crs(6148))

interpolate22 <- interpolate_centroids22 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts22$local_municipality_id
interpolate22$area
#so KZN292 is still KZN292


#### MUN 23
print(viol_2006_to_2011$local_municipality_id)
pnts23 <- mun2006 %>% 
  filter(local_municipality_id == "KZN234")

centroids23 <- as.data.frame(st_coordinates(st_centroid(pnts23$geometry)))

interpolate_centroids23 <- st_as_sf(centroids23, coords = c("X", "Y"), crs = st_crs(6148))

interpolate23 <- interpolate_centroids23 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts23$local_municipality_id
interpolate23$area
#so KZN234 is now KZN237

#### MUN 24
#KZN234 is KZN237

#### MUN 25
#KZN234 is KZN237

#### MUN 26
#NW405 is now GT484

#### MUN 27 
#NW405 is now GT484


#### MUN 28
print(viol_2006_to_2011$local_municipality_id)
pnts29 <- mun2006 %>% 
  filter(local_municipality_id == "KZN215")

centroids29 <- as.data.frame(st_coordinates(st_centroid(pnts29$geometry)))

interpolate_centroids29 <- st_as_sf(centroids29, coords = c("X", "Y"), crs = st_crs(6148))

interpolate29 <- interpolate_centroids29 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts29$local_municipality_id
interpolate29$area
#so KZN215 is now KZN216

#### MUN 29
print(viol_2006_to_2011$local_municipality_id)
pnts30 <- mun2006 %>% 
  filter(local_municipality_id == "KZN232")

centroids30 <- as.data.frame(st_coordinates(st_centroid(pnts30$geometry)))

interpolate_centroids30 <- st_as_sf(centroids30, coords = c("X", "Y"), crs = st_crs(6148))

interpolate30 <- interpolate_centroids30 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts30$local_municipality_id
interpolate30$area
#so KZN232 is now KZN238

#### MUN 30

#so BUF is still BUF

#### MUN 31
print(viol_2006_to_2011$local_municipality_id)
pnts31 <- mun2006 %>% 
  filter(local_municipality_id == "KZN245")

centroids31 <- as.data.frame(st_coordinates(st_centroid(pnts31$geometry)))

interpolate_centroids31 <- st_as_sf(centroids31, coords = c("X", "Y"), crs = st_crs(6148))

interpolate31 <- interpolate_centroids31 %>% mutate(
  intersection = as.integer(st_intersects(geometry, mun_2016)),
  area = if_else(is.na(intersection), '', mun_2016$local_municipality_id[intersection])
)

pnts31$local_municipality_id
interpolate31$area
#so KZN245 is still KZN245


#NOW READ IN THE CLEANED CANDIDATES DATA AND CREATE A VARIABLE FOR ASSASSINATION TREATMENT
data_2011 <- readRDS(here("data", "processed_data", "candidates_clean_2011_interpolated_to_2016_MUNICIPALITY.RDS"))


# CREATE DUMMY VARIABLE FOR ANY EXPOSED UNITS ------------------
data_2011 <- data_2011 %>% 
  mutate(treat = case_when(
    local_municipality_id == "KZN226" ~ 1,
    local_municipality_id == "ETH" ~ 1,
    local_municipality_id == "KZN291" ~ 1,
    local_municipality_id == "GT484" ~ 1,
    local_municipality_id == "MAN" ~ 1,
    local_municipality_id == "KZN245" ~ 1,
    local_municipality_id == "NW373" ~ 1,
    local_municipality_id == "KZN284" ~ 1,
    local_municipality_id == "MP326" ~ 1,
    local_municipality_id == "KZN263" ~ 1,
    local_municipality_id == "KZN241" ~ 1,
    local_municipality_id == "WC024" ~ 1,
    local_municipality_id == "FS204" ~ 1,
    local_municipality_id == "MP307" ~ 1,
    local_municipality_id == "CPT" ~ 1,
    local_municipality_id == "KZN242" ~ 1,
    local_municipality_id == "KZN292" ~ 1,
    local_municipality_id == "KZN237" ~ 1,
    local_municipality_id == "KZN216" ~ 1,
    local_municipality_id == "KZN238" ~ 1,
    local_municipality_id == "BUF" ~ 1
  )) 



# check to make sure this worked properly: 
length(which(data_2011$treat==1)) ###21


# WRITE OUT DATA TO PROCESSED_DATA FILE
saveRDS(data_2011, here("data", "processed_data", "data_2011_with_assassinations_merged_MUNICIPALITY.rds"))



