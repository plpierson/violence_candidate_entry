rm(list=ls())

library(tidyverse)
library(stargazer)
library(lfe)
library(plm)
library(wfe)
library(PanelMatch)
library(MatchIt)
library(cobalt)
library(lmtest)
library(sandwich)
library(here)
library(sf) #gis
library(areal) #gis

options(scipen = 999)

# IMPORT TREAT DATA --------------------------
treat <- readRDS(here("data", "processed_data", "full_candidate_data_with_controls_MUN.rds"))
treat$local_municipality_id <- as.factor(treat$local_municipality_id)


# create dummy variable for metros
treat <- treat %>% 
  mutate(metro = if_else(local_municipality_id == "BUF" |
                           local_municipality_id == "CPT" |
                           local_municipality_id == "EKU" |
                           local_municipality_id == "ETH" |
                           local_municipality_id == "JHB" |
                           local_municipality_id == "MAN" |
                           local_municipality_id == "TSH" |
                           local_municipality_id == "NMA", 
                         1, 0))



# read in data to ANC vote share control var
anc_share_2006 <- readRDS(here("data", "processed_data", "anc_vote_share_2006_in_2016_boundaries_MUN.RDS"))
anc_share_2006 <- anc_share_2006 %>% 
  select(-c(for_AFRICAN_NATIONAL_CONGRESS)) %>% 
  mutate(electoral_cycle = 2006)

anc_share_2011 <- readRDS(here("data", "processed_data", "anc_vote_share_2011_in_2016_boundaries_MUN.RDS"))
anc_share_2011 <- anc_share_2011 %>% 
  select(-c(for_AFRICAN_NATIONAL_CONGRESS)) %>% 
  mutate(electoral_cycle = 2011)

anc_share_2016 <- readRDS(here("data", "processed_data", "data_2016_clean_MUN.RDS"))
anc_share_2016$local_municipality_id <- as.factor(anc_share_2016$local_municipality_id)
  
anc_share_2016 <- anc_share_2016 %>% 
  ungroup() %>% 
  select(-c(province)) %>% 
  mutate(electoral_cycle = 2016)

data_join <- rbind(anc_share_2006, anc_share_2011)
data_join2 <- rbind(data_join, anc_share_2016)

data_join2$electoral_cycle <- as.factor(data_join2$electoral_cycle)

data_join3 <- left_join(treat, data_join2, by = c("electoral_cycle", "local_municipality_id"))



##### merge in control data on gender demographics
gender <- readRDS(here("data", "processed_data", "data_controls_gender_MUN.RDS"))

gender$electoral_cycle <- as.factor(gender$electoral_cycle)

data_join4 <- left_join(data_join3, gender, by = c("local_municipality_id", "electoral_cycle"))

# replace NAs in treat variable
data_join4$treat[is.na(data_join4$treat)] <- 0


############################
###### BEGIN ANALYSIS ######
############################

mod1 <- plm(sum_candidates ~ treat,
            data = data_join4,
            index = c("local_municipality_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod2 <- plm(sum_candidates ~ treat + unemploy_rate +
              prop_informal + vote_share_for_AFRICAN_NATIONAL_CONGRESS + total_pop + prop_pop_female,
            data = data_join4,
            index = c("local_municipality_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod3 <- plm(prop_female ~ treat,
            data = data_join4,
            index = c("local_municipality_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod4 <- plm(prop_female ~ treat + unemploy_rate + 
              prop_informal + vote_share_for_AFRICAN_NATIONAL_CONGRESS + metro + total_pop + prop_pop_female,
            data = data_join4,
            index = c("local_municipality_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")



cov1 <- vcovHC(mod1, type = "HC1")
robust_se1 <- sqrt(diag(cov1))

cov2 <- vcovHC(mod2, type = "HC1")
robust_se2 <- sqrt(diag(cov2))

cov3 <- vcovHC(mod3, type = "HC1")
robust_se3 <- sqrt(diag(cov3))

cov4 <- vcovHC(mod4, type = "HC1")
robust_se4 <- sqrt(diag(cov4))


# output results
stargazer(mod1, mod2, mod3, mod4,
          #covariate.labels = c("Any Exposure", "90 Day Window", "30 Day Window"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Sum Candidates", "Proportion Female"),
          se = list(robust_se1, robust_se2, robust_se3, robust_se4),
          add.lines = list(c("Unit FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Electoral Cycle FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "No", "Yes", "No", "Yes"),
                           c("Cluster-Robust SE", "Yes", "Yes", "Yes", "Yes"))
)


##################################################################
##################################################################
################# RE-RUN WITH SECOND SET OF DV'S #################
##################################################################
##################################################################


mod5 <- plm(num_female ~ treat,
            data = data_join4,
            index = c("local_municipality_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod6 <- plm(num_female ~ treat + unemploy_rate +
              prop_informal + vote_share_for_AFRICAN_NATIONAL_CONGRESS + metro + total_pop + prop_pop_female,
            data = data_join4,
            index = c("local_municipality_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod7 <- plm(num_male ~ treat,
            data = data_join4,
            index = c("local_municipality_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod8 <- plm(num_male ~ treat + unemploy_rate + 
              prop_informal + vote_share_for_AFRICAN_NATIONAL_CONGRESS + metro + total_pop + prop_pop_female,
            data = data_join4,
            index = c("local_municipality_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")



cov5 <- vcovHC(mod5, type = "HC1")
robust_se5 <- sqrt(diag(cov5))

cov6 <- vcovHC(mod6, type = "HC1")
robust_se6 <- sqrt(diag(cov6))

cov7 <- vcovHC(mod7, type = "HC1")
robust_se7 <- sqrt(diag(cov7))

cov8 <- vcovHC(mod8, type = "HC1")
robust_se8 <- sqrt(diag(cov8))


# output results
stargazer(mod5, mod6, mod7, mod8,
          #covariate.labels = c("Any Exposure", "90 Day Window", "30 Day Window"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Num. Female", "Num Male"),
          se = list(robust_se5, robust_se6, robust_se7, robust_se8),
          add.lines = list(c("Unit FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Electoral Cycle FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "No", "Yes", "No", "Yes"),
                           c("Cluster-Robust SE", "Yes", "Yes", "Yes", "Yes"))
)




