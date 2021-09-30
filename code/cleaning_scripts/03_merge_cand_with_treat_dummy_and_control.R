rm(list = ls())

library(tidyverse)
library(here)

options(scipen = 999)

############################
####### READ IN DATA #######
############################
data_2006 <- readRDS(here("data", "processed_data", "data_2006_with_assassinations_merged.RDS"))
data_2011 <- readRDS(here("data", "processed_data", "data_2011_with_assassinations_merged.RDS"))
data_2016 <- readRDS(here("data", "processed_data", "data_2016_with_assassinations_merged.RDS"))

data_2016 <- data_2016 %>% 
  select(-c(province_id))

  # do datasets look as expected?
glimpse(data_2006)
glimpse(data_2011)
glimpse(data_2016)
  # I need to update/change the variable type (class) for a few of the vars in data_2011 & data_2016 for a clean merger

data_2011 <- data_2011 %>% 
  mutate(ward_id = as.factor(ward_id))

data_2016 <- data_2016 %>% 
  mutate(ward_id = as.factor(ward_id),
         sum_candidates = as.double(sum_candidates),
         num_male = as.double(num_male)
         )
  


##############################################
######### READ IN CONTROL VARIABLES ##########
##############################################
controls_2006 <- readRDS(here("data", "processed_data", "data_2006_controls.RDS"))
controls_2011 <- readRDS(here("data", "processed_data", "data_2011_controls.RDS"))
controls_2016 <- readRDS(here("data", "processed_data", "data_2011_controls.RDS"))


  # merge with candidates data
data_full_2006 <- left_join(data_2006, controls_2006, by = "ward_id")
data_full_2011 <- left_join(data_2011, controls_2011, by = "ward_id")
data_full_2016 <- left_join(data_2016, controls_2016, by = "ward_id")

# bind datasets together
data_final <- rbind(data_full_2006, data_full_2011, data_full_2016)


# quick inspection to make sure things look ok
length(unique(data_final$ward_id)) # 4392 - yep...this means we have 3 observations for each ward_id
table(data_final$electoral_cycle) 

summary(data_final$sum_candidates)
summary(data_final$num_male)
summary(data_final$mean_age) 
summary(data_final$prop_female) 


length(which(data_final$treat==1)) #43



  # WRITE TO PROCESSED_DATA FOLDER -----------------------
saveRDS(data_final, here("data", "processed_data", "full_candidate_data_with_controls.RDS"))



#############################################################################################
#############################################################################################
############################ MERGE MUNICIPAL-LEVEL DATA ####################################
#############################################################################################
#############################################################################################

############################
####### READ IN DATA #######
############################
data_2006_mun <- readRDS(here("data", "processed_data", "data_2006_with_assassinations_merged_MUNICIPALITY.RDS"))
data_2011_mun <- readRDS(here("data", "processed_data", "data_2011_with_assassinations_merged_MUNICIPALITY.RDS"))
data_2016_mun <- readRDS(here("data", "processed_data", "data_2016_with_assassinations_merged_MUNICIPALITY.RDS"))


# do datasets look as expected?
glimpse(data_2006_mun)
glimpse(data_2011_mun)
glimpse(data_2016_mun)


# drop a few vars to allow clean merger
data_2006_mun <- data_2006_mun %>% 
  select(-c(num_parties_contest, num_independents, num_ind_male, num_ind_female))

data_2011_mun <- data_2011_mun %>% 
  select(-c(num_parties_contest, num_independents, num_ind_male, num_ind_female))

data_2016_mun <- data_2016_mun %>% 
  select(-c(local_municipality_name))



##############################################
######### READ IN CONTROL VARIABLES ##########
##############################################
controls_2006_mun <- readRDS(here("data", "processed_data", "data_2006_controls_MUN.RDS"))
controls_2011_mun <- readRDS(here("data", "processed_data", "data_2011_controls_MUN.RDS"))
controls_2016_mun <- readRDS(here("data", "processed_data", "data_2011_controls_MUN.RDS"))


# merge with candidates data
data_full_2006_mun <- left_join(data_2006_mun, controls_2006_mun, by = "local_municipality_id")
data_full_2011_mun <- left_join(data_2011_mun, controls_2011_mun, by = "local_municipality_id")
data_full_2016_mun <- left_join(data_2016_mun, controls_2016_mun, by = "local_municipality_id")

# bind datasets together
data_final_mun <- rbind(data_full_2006_mun, data_full_2011_mun, data_full_2016_mun)


# quick inspection to make sure things look ok
length(unique(data_final_mun$local_municipality_id)) # 213 
table(data_final_mun$electoral_cycle) 

summary(data_final_mun$sum_candidates)


length(which(data_final_mun$treat==1)) #51



# WRITE TO PROCESSED_DATA FOLDER -----------------------
saveRDS(data_final_mun, here("data", "processed_data", "full_candidate_data_with_controls_MUN.RDS"))

