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
  mutate(province = as.factor(province),
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

age_outlier <- data %>% 
  filter(age>500)

data <- data %>% 
  replace_with_na(replace = list(age = 999))

  # the areal interpolation we will perform later on does not work well with NAs (let's replace these values with the median == 43.00)
data <- data %>% 
  mutate(age = replace_na(age, 43.00))

summary(data$age) # verify conversion..no NAs remaining


######################################
###### AGGREGATE TO WARD LEVEL #######
######################################

data_summary <- data %>% 
  group_by(ward_id) %>% 
  mutate(sum_candidates = n()) %>% 
  ungroup() %>% 
  group_by(ward_id) %>% 
  mutate(num_male = sum(length(which(gender=="M"))),
         num_female = sum(length(which(gender=="F")))) %>% 
  ungroup() %>% 
  group_by(ward_id) %>% 
  mutate(mean_age = mean(age, na.rm = TRUE)) %>% 
  mutate(prop_female = 1 - (num_male / sum_candidates)) %>% 
  distinct(ward_id, .keep_all = TRUE) %>% 
  select(province, local_municipality_id, local_municipality_name, sum_candidates, num_male, num_female, mean_age, prop_female)


    # check out aggregate/summary data for ward-level variables
glimpse(data_summary)
length(unique(data_summary$ward_id)) # 4277 unique ward_ids, so this looks good
length(unique(data_summary$local_municipality_id)) # 234
length(unique(data_summary$local_municipality_name)) # 234

sum(data_summary$sum_candidates) # 30501...great, this comports with number of candidates in raw data
sum(data_summary$num_male) # 20509...great, this comports with number of male candidates in raw data
sum(data_summary$num_female)
summary(data_summary$mean_age)


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
                        extensive = c("sum_candidates", "num_male", "num_female")
                        )

data_final <- data_final %>% 
  mutate(electoral_cycle = 2011) %>% 
  mutate(electoral_cycle = as.factor(electoral_cycle))


  # WRITE TO PROCESSED_DATA FOLDER -----------------------
saveRDS(data_final, here("data", "processed_data", "candidates_clean_2011_extrapolated_to_2016.RDS"))



##################################################################################
##################################################################################
######################### AGGREGATE TO MUNICIPAL LEVEL ###########################
##################################################################################
##################################################################################


data_summary <- data %>% 
  group_by(local_municipality_id) %>% 
  mutate(sum_candidates = n()) %>% 
  ungroup() %>% 
  group_by(local_municipality_id) %>% 
  mutate(num_male = sum(length(which(gender=="M"))),
         num_female = sum(length(which(gender=="F")))) %>% 
  ungroup() %>% 
  group_by(local_municipality_id) %>% 
  mutate(mean_age = mean(age, na.rm = TRUE)) %>% 
  mutate(prop_female = 1 - (num_male / sum_candidates)) %>% 
  mutate(num_independents = sum(length(which(party_name == "INDEPENDENT CANDIDATE")))) %>% 
  mutate(num_ind_female = sum(length(which(party_name == "INDEPENDENT CANDIDATE" & gender=="F")))) %>% 
  mutate(num_ind_male = sum(length(which(party_name == "INDEPENDENT CANDIDATE" & gender=="M")))) %>% 
  distinct(local_municipality_id, .keep_all = TRUE) %>% 
  select(province, local_municipality_id, local_municipality_name,
         sum_candidates, num_male, num_female, mean_age, prop_female, num_independents,
         num_ind_female, num_ind_male)


data_num_parties <- data %>% 
  filter(party_name != "INDEPENDENT CANDIDATE") %>% 
  group_by(local_municipality_id) %>% 
  mutate(num_parties_contest = n_distinct(party_name)) %>% 
  distinct(local_municipality_id, .keep_all = TRUE) %>% 
  select(local_municipality_id, num_parties_contest)


data_summary <- left_join(data_summary, data_num_parties, by = "local_municipality_id")



##############################################
######## READ IN MUNICIPAL SHAPE FILE ########
##############################################

mun_shape_file <- st_read(here("data", "gis_data_raw", "MDB_Local_Municipal_Boundary_2011.shp"))

mun_shape_file <- st_transform(mun_shape_file, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)


mun_shape_file <- mun_shape_file %>% 
  rename(province = ProvinceNa,
         local_municipality_id = LocalMunic) %>% 
  select(province, local_municipality_id, geometry)


# join candidates data to shape file
data_join <- left_join(data_summary, mun_shape_file, by = "local_municipality_id") 


#####################################################
######## INTERPOLATE TO 2016 MUNICIPAL BOUNDARIES ########
#####################################################

mun_2016_shape_file <- st_read(here("data", "gis_data_raw", "ecc03798-c4f1-42e2-a0e6-573c0c07c8542020410-1-1psvop3.0vyd.shp"))

# TRANSFORM SHAPE FILES TO SA'S OFFICIAL COORDINATE SYSTEM -----------------------
mun_2016_shape_file <- st_transform(mun_2016_shape_file, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)


mun_2016_shape_file <- mun_2016_shape_file %>% 
  rename(local_municipality_id = LocalMunic) %>% 
  select(local_municipality_id, geometry)


# CONVERT DATA TO SF OBJECTS IN ORDER TO PERFORM AREAL INTERPOLATION -----------------------
data_join <- st_as_sf(data_join)
mun_2016_shape_file <- st_as_sf(mun_2016_shape_file)


# VALIDATE GEOM -----------------------
data_join <- st_make_valid(data_join)
st_is_valid(data_join, reason = TRUE)

mun_2016_shape_file <- st_make_valid(mun_2016_shape_file)
st_is_valid(mun_2016_shape_file, reason=TRUE)


# interpolate to 2016 polygons
data_final <- aw_interpolate(mun_2016_shape_file,
                             tid = local_municipality_id,
                             source = data_join,
                             sid = local_municipality_id,
                             weight = "sum",
                             output = "tibble",
                             intensive = c("mean_age", "prop_female"),
                             extensive = c("sum_candidates", "num_male", "num_female", "num_independents", "num_ind_female", "num_ind_male", "num_parties_contest")
)

data_final <- data_final %>% 
  mutate(electoral_cycle = 2011) %>% 
  mutate(electoral_cycle = as.factor(electoral_cycle))


# WRITE TO PROCESSED_DATA FOLDER -----------------------
saveRDS(data_final, here("data", "processed_data", "candidates_clean_2011_interpolated_to_2016_MUNICIPALITY.RDS"))
