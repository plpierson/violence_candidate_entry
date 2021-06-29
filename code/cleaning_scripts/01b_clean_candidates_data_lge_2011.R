rm(list=ls())

library(tidyverse)
library(here) #workflow
library(readxl)
library(stringr) #strings and text data
library(lubridate) #dates
library(naniar) #useful for dealing with various forms of NA values
library(sf) #gis
library(areal) #gis

options(scipen = 999) # prevent defaulting to scientific notation for longer numeric variable values (such as ID numbers)


##########################
###### READ IN DATA ######
##########################

data <- read_excel(here("data", "raw_data", "lge2011_age_gender.xlsx"), sheet = "Ward")

# tidy data
data <- data %>% 
  separate(Municipality, c("local_municipality_id", "local_municipality_name"), sep = "-",
           extra = "merge", fill = "right") %>% 
  rename(province = Province,
         party_name = Party,
         ward_id = Ward,
         last_name = Surname,
         first_name = Fullname,
         gender = Gender,
         age = Age) %>% 
  unite(full_name, sep = " ", na.rm = TRUE, first_name:last_name, remove = FALSE) %>% 
  mutate(electoral_cycle = 2011) %>% 
  mutate(electoral_cycle = as.factor(electoral_cycle),
         province = as.factor(province),
         ward_id = as.factor(ward_id),
         gender = as.factor(gender)) %>% 
  mutate(across(where(is.character), str_trim)) 


# ok, what does the data look like?
glimpse(data)

table(data$province) # 9 provinces, so that's good
length(unique(data$local_municipality_id)) #234
length(unique(data$local_municipality_name)) #234...ok local_municipality_id and local_municipality_name are symmetric,
  # and that's what we would hope, so that's good

length(unique(data$party_name)) # 120 distinct parties fielded ward candidates in 2006 LGE
length(unique(data$ward_id)) # 4277 wards across the 234 municipalities
length(unique(data$full_name)) # 21179 distinct candidates out of 30501 total...so a number of individuals ran for 
  # office in more than one ward

summary(data$gender) # no NA...more than 2x as meny men running in ward elections

summary(data$age) # max is 999.00...so data entry issue somewhere
plot(data$age) # visually it looks like there are two of these values
length(which(data$age==999)) # yep...let's convert these to NA for consistency


data <- data %>% 
  replace_with_na(replace = list(age = 999))

summary(data$age) # verify conversion..now there are two NA values


######################################
###### AGGREGATE TO WARD LEVEL #######
######################################

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
  select(province, local_municipality_id, local_municipality_name, electoral_cycle, sum_candidates, num_male, mean_age, prop_female)


    # check out aggregate/summary data for ward-level variables
glimpse(data_summary)
length(unique(data_summary$ward_id)) # 4277 unique ward_ids, so this looks good
length(unique(data_summary$local_municipality_id)) # 234
length(unique(data_summary$local_municipality_name)) # 234

sum(data_summary$sum_candidates) # 30501...great, this comports with number of candidates in raw data
sum(data_summary$num_male) # 20509...great, this comports with number of male candidates in raw data


#########################################
######## READ IN WARD SHAPE FILE ########
#########################################

ward_shape_file <- st_read(here("data", "gis_data_raw", "wards2011.shp"))
  # there are duplicates of all the sahpe files for some reason...let's filter those to single observations
ward_shape_file <- ward_shape_file %>% 
  distinct(WardID, .keep_all=TRUE)

ward_shape_file <- st_transform(ward_shape_file, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)


ward_shape_file <- ward_shape_file %>% 
  rename(province = ProvinceNa,
         local_municipality_id = LocalMunic,
         ward_id = WardID) %>% 
  select(province, local_municipality_id, ward_id, geometry)


  # join candidates data to shape file
data_join <- left_join(data_summary, ward_shape_file, by = "ward_id") 


#####################################################
######## INTERPOLATE TO 2016 WARD BOUNDARIES ########
#####################################################

ward_2016_shape_file <- st_read(here("data", "gis_data_raw", "wards2016.shp"))

  # TRANSFORM SHAPE FILES TO SA'S OFFICIAL COORDINATE SYSTEM -----------------------
ward_2016_shape_file <- st_transform(ward_2016_shape_file, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)


ward_2016_shape_file <- ward_2016_shape_file %>% 
  rename(ward_id = WardID) %>% 
  select(ward_id, geometry)


  # CONVERT DATA TO SF OBJECTS IN ORDER TO PERFORM AREAL INTERPOLATION -----------------------
data_join <- st_as_sf(data_join)
ward_2016_shape_file <- st_as_sf(ward_2016_shape_file)


  # VALIDATE GEOM -----------------------
data_join <- st_make_valid(data_join)
st_is_valid(data_join, reason = TRUE)

ward_2016_shape_file <- st_make_valid(ward_2016_shape_file)
st_is_valid(ward_2016_shape_file, reason=TRUE)


  # interpolate to 2016 polygons
data_final <- aw_interpolate(ward_2016_shape_file,
                        tid = ward_id,
                        source = data_join,
                        sid = ward_id,
                        weight = "sum",
                        output = "tibble",
                        intensive = c("mean_age", "prop_female"),
                        extensive = c("sum_candidates", "num_male")
                        )


  # WRITE TO PROCESSED_DATA FOLDER -----------------------
saveRDS(data_final, here("data", "processed_data", "candidates_clean_2011_extrapolated_to_2016.RDS"))
