rm(list=ls())

library(tidyverse)
library(here)
library(sf) #gis
library(areal) #gis
library(readxl)
library(stringr) #strings and text data
library(lubridate) #dates
library(naniar)
library(numform)

options(scipen = 999)

viol <- read_excel(here("data", "raw_data", "assassinations_data", "za_assassinations copy.xlsx"), sheet = 1)

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

dat <- viol_ward %>% 
  filter(sitting == 1)


### plot 1: attacks targeting LC Ward councillors over time
ggplot(data = dat, aes(x = year, y = ..count..)) +
  geom_bar(width = 0.7) +
  scale_x_continuous(name = "Year", breaks = c(seq(2000, 2016, 1))) +
  ylim(0, 10) +
  theme(axis.text.x = element_text(angle=65, hjust = 1)) +
  ggtitle("Political Assassinations of LC Ward Councillors by Year, 2000-2016") +
  ggsave("figures_and_tables/ward_councillors_by_year.png")




##### failed vs successful assassination attempts against LC ward councillors, by year

# create success var
dat <- dat %>% 
  mutate(success = case_when(failed==0 ~ 1,
                             failed==1 ~ 0))

# transform viol_sub$success into factor variable for plotting purposes
dat$success <- factor(dat$success, levels = c(1,0))

ggplot(dat, aes(x = year, fill = factor(success), group = factor(success))) +
  geom_bar(width = 0.7, position = position_stack(reverse=TRUE)) +
  scale_x_continuous(name = "Year", breaks = c(seq(2000, 2016, 1))) +
  scale_fill_manual(name = "Assassinations", values = c("grey22", "grey65"), 
                    labels = c("Lethal", "Failed"), guide = guide_legend(reverse = TRUE, title = "Assassination Attempts")) +
  ylim(0, 10) +
  theme(legend.position = c(0.2, 0.87)) +
  theme(axis.text.x = element_text(angle=50, hjust = 1)) +
  theme(axis.title.x = element_text(size=14, vjust = -2)) + #make `Year' on the x-axis slightly larger and shift it down just a touch
  theme(axis.title.y = element_text(size=14),
        plot.margin = margin(0,0,1,0, "cm")) + #add some extra padding so that `Year' doesn't get shifted out of the image
  ggtitle("Political Assassinations of LC Ward Councillors, Failed vs. Lethal") +
  ggsave(filename = "figures_and_tables/ward_councillors_failed_vs_successful.png")



##### successful vs. failed attempts
ggplot(data = dat, aes(x = success)) +
  geom_bar(width = 0.5) +
  geom_text(stat = "count", aes(label=..count..), vjust = -0.75) +
  scale_x_discrete(labels = c("Lethal", "Failed")) +
  ylim(0, 45) +
  xlab("Outcome of Assassination Attempt") +
  ggtitle("Lethal vs. Failed Assassination Attempts of LC Ward Councillors, 2000-2016") +
  ggsave(filename = "figures_and_tables/lethal_vs_failed.png")


##### gender
ggplot(data = dat, aes(x = gender)) +
  geom_bar(width = 0.5) +
  geom_text(stat = "count", aes(label=..count..), vjust = -0.75) +
  scale_x_discrete(labels = c("Female", "Male")) +
  ylim(0, 45) +
  xlab("Target Gender") +
  ggtitle("Assassination Targets by Gender, 2000-2016") +
  ggsave(filename = "figures_and_tables/assassination_attempts_by_gender.png")



##### target party
ggplot(data = dat, aes(x = target_party)) +
  geom_bar(width = 0.5) +
  geom_text(stat = "count", aes(label=..count..), vjust = -0.75) +
  ylim(0, 30) +
  xlab("Political Party Affiliation") +
  ggtitle("Assassination Targets by Political Party, 2000-2016") +
  ggsave(filename = "figures_and_tables/targets_by_political_party.png")



##### attacks by province
ggplot(data = dat, aes(x = province)) +
  geom_bar(width = 0.5) +
  geom_text(stat = "count", aes(label=..count..), vjust = -0.75) +
  ylim(0, 35) +
  xlab("Assassination Attempts by Province") +
  theme(axis.text.x = element_text(angle=40, hjust = 1)) +
  ggtitle("Assassination Attempts of LC Ward Councillors by Province, 2000-2016") +
  ggsave(filename = "figures_and_tables/targets_by_province.png")



##### attacks by weapon
dat <- dat %>% 
  mutate(weapon2 = case_when(weapon=="gun" ~ "gun",
                             weapon!="gun" ~ "other"))

ggplot(data = dat, aes(x = weapon2)) +
  geom_bar(width = 0.5) +
  geom_text(stat = "count", aes(label=..count..), vjust = -0.75) +
  ylim(0, 45) +
  scale_x_discrete(labels = c("Gun", "Other")) +
  xlab("Weapon Used") +
  ggtitle("Type of Weapon Used in Assassination Attempts of LC Ward Councillors, 2000-2016") +
  ggsave("figures_and_tables/weapon_used.png")


######################################################################################################################
######################################################################################################################
#### BASIC SUMMARY STATISTICS FOR THE BROADER DATASET OF ASSASSINATIONS (NOT JUST TARGETING LC WARD COUNCILLORS) #####
######################################################################################################################
######################################################################################################################

dat_full <- viol %>% 
  filter(date_of_attack>="2000-12-05" & date_of_attack<="2016-08-03") 

dat <- viol_ward %>% 
  filter(sitting == 1)


