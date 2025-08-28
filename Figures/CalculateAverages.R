###Settings and libraries####
rm(list=ls()) #removes all variables
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ##Set path to current dir
library(deSolve) #Lib for ODE solver
library(tidyverse) #Lib for data analysis
source("Functions_Model.R")


Flowers_wood_forward = readRDS("Data/Flowers_wood_fw.RDS")
Aphids_wood_spring_forward = readRDS("Data/Aphids_wood_spring_fw.RDS")
Aphids_wood_autumn_forward = readRDS("Data/Aphids_wood_autumn_fw.RDS")
T2s_annual_forward = readRDS("Data/T2s_annual_fw.RDS")
T2s_perennial_forward = readRDS("Data/T2s_perennial_fw.RDS")
T3s_annual_forward = readRDS("Data/T3s_annual_fw.RDS")
T3s_perennial_forward = readRDS("Data/T3s_perennial_fw.RDS")
B2m_annual_forward = readRDS("Data/B2m_annual_fw.RDS")
B2m_perennial_forward = readRDS("Data/B2m_perennial_fw.RDS")
B3m_annual_forward = readRDS("Data/B3m_annual_fw.RDS")
B3m_perennial_forward = readRDS("Data/B3m_perennial_fw.RDS")
Mowing_early_nomort_forward = readRDS("Data/Mowing_early_nomort_fw.RDS")
Mowing_early_mort_forward = readRDS("Data/Mowing_early_mort_fw.RDS")
Mowing_late_nomort_forward = readRDS("Data/Mowing_late_nomort_fw.RDS")
Mowing_late_mort_forward = readRDS("Data/Mowing_late_mort_fw.RDS")

#B1m
Flowers_wood_forward_avg <- Flowers_wood_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(B1m, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))

saveRDSAsk(Flowers_wood_forward_avg, "Data/Averages/Flowers_wood_fw_avg.RDS") #save way to safe data


#n1s
Aphids_wood_spring_forward_avg <- Aphids_wood_spring_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(n1s, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))

saveRDSAsk(Aphids_wood_spring_forward_avg, "Data/Averages/Aphids_wood_spring_fw_avg.RDS") #save way to safe data



#n1a
Aphids_wood_autumn_forward_avg <- Aphids_wood_autumn_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(n1a, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))

saveRDSAsk(Aphids_wood_autumn_forward_avg, "Data/Averages/Aphids_wood_autumn_fw_avg.RDS") #save way to safe data


#T2s annual
T2s_annual_forward_avg <- T2s_annual_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(T2s, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))

saveRDSAsk(T2s_annual_forward_avg, "Data/Averages/T2s_annual_fw_avg.RDS") #save way to safe data

#T2s perennial
T2s_perennial_forward_avg <- T2s_perennial_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(T2s, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))

saveRDSAsk(T2s_perennial_forward_avg, "Data/Averages/T2s_perennial_fw_avg.RDS") #save way to safe data

#T3s annual
T3s_annual_forward_avg <- T3s_annual_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(T3s, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))

saveRDSAsk(T3s_annual_forward_avg, "Data/Averages/T3s_annual_fw_avg.RDS") #save way to safe data


#T3s perennial
T3s_perennial_forward_avg <- T3s_perennial_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(T3s, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))

saveRDSAsk(T3s_perennial_forward_avg, "Data/Averages/T3s_perennial_fw_avg.RDS") #save way to safe data

#B2m annual
B2m_annual_forward_avg <- B2m_annual_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(B2m, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))

saveRDSAsk(B2m_annual_forward_avg, "Data/Averages/B2m_annual_fw_avg.RDS") #save way to safe data

#B2m perennial
B2m_perennial_forward_avg <- B2m_perennial_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(B2m, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))

saveRDSAsk(B2m_perennial_forward_avg, "Data/Averages/B2m_perennial_fw_avg.RDS") #save way to safe data

#B3m annual
B3m_annual_forward_avg <- B3m_annual_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(B3m, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))

saveRDSAsk(B3m_annual_forward_avg, "Data/Averages/B3m_annual_fw_avg.RDS") #save way to safe data

#B3m perennial
B3m_perennial_forward_avg <- B3m_perennial_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(B3m, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))

saveRDSAsk(B3m_perennial_forward_avg, "Data/Averages/B3m_perennial_fw_avg.RDS") #save way to safe data

#Mowing early crop flower margin WITHOUT mortality
Mowing_early_nomort_forward_avg <- Mowing_early_nomort_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(M2s, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))
saveRDSAsk(Mowing_early_nomort_forward_avg, "Data/Averages/Mowing_early_nomort_fw_avg.RDS") #save way to safe data

#Mowing early crop flower margin WITH mortality
Mowing_early_mort_forward_avg <- Mowing_early_mort_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(M2s, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))
saveRDSAsk(Mowing_early_mort_forward_avg, "Data/Averages/Mowing_early_mort_fw_avg.RDS") #save way to safe data

#Mowing late crop flower margin WITHOUT mortality
Mowing_late_nomort_forward_avg <- Mowing_late_nomort_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(M3s, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))
saveRDSAsk(Mowing_late_nomort_forward_avg, "Data/Averages/Mowing_late_nomort_fw_avg.RDS") #save way to safe data

#Mowing late crop flower margin WITH mortality
Mowing_late_mort_forward_avg <- Mowing_late_mort_forward$Timedynamics %>%
  filter(year > 12) %>% ##I do not want the transient part
  group_by(M3s, year) %>%
  summarize_all(mean) ##possible to calculate max/min as well (list(mean, min, max))
saveRDSAsk(Mowing_late_mort_forward_avg, "Data/Averages/Mowing_late_mort_fw_avg.RDS") #save way to safe data

