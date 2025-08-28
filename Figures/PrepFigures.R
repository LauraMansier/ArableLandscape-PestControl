###Settings and libraries####
rm(list=ls()) #removes all variables
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ##Set path to current dir
library(tidyverse) #Lib for data analysis
source("Functions_Model.R")


Flowers_wood_forward_avg = readRDS("Data/Averages/Flowers_wood_fw_avg.RDS")
Aphids_wood_spring_forward_avg = readRDS("Data/Averages/Aphids_wood_spring_fw_avg.RDS")
Aphids_wood_autumn_forward_avg = readRDS("Data/Averages/Aphids_wood_autumn_fw_avg.RDS")
B2m_annual_forward_avg = readRDS("Data/Averages/B2m_annual_fw_avg.RDS")
B2m_perennial_forward_avg = readRDS("Data/Averages/B2m_perennial_fw_avg.RDS")
B3m_annual_forward_avg = readRDS("Data/Averages/B3m_annual_fw_avg.RDS")
B3m_perennial_forward_avg = readRDS("Data/Averages/B3m_perennial_fw_avg.RDS")
T2s_annual_forward_avg = readRDS("Data/Averages/T2s_annual_fw_avg.RDS")
T2s_perennial_forward_avg = readRDS("Data/Averages/T2s_perennial_fw_avg.RDS")
T3s_annual_forward_avg = readRDS("Data/Averages/T3s_annual_fw_avg.RDS")
T3s_perennial_forward_avg = readRDS("Data/Averages/T3s_perennial_fw_avg.RDS")
Mowing_early_nomort_forward_avg = readRDS("Data/Averages/Mowing_early_nomort_fw_avg.RDS")
Mowing_early_mort_forward_avg = readRDS("Data/Averages/Mowing_early_mort_fw_avg.RDS")
Mowing_late_nomort_forward_avg = readRDS("Data/Averages/Mowing_late_nomort_fw_avg.RDS")
Mowing_late_mort_forward_avg = readRDS("Data/Averages/Mowing_late_mort_fw_avg.RDS")

#B1m
B1m_list <- list()

for (i in 1:7) {
  B1m_list[[i]] <- rbind(
    Flowers_wood_forward_avg %>%
      filter(B1m == 0 & year == 19 + i),
    Flowers_wood_forward_avg %>%
      filter(B1m > 0 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(B1m_list) <- paste0("B1m_year_", 1:7)


#n1s
n1s_list <- list()

for (i in 1:7) {
  n1s_list[[i]] <- rbind(
    Aphids_wood_spring_forward_avg %>%
      filter(n1s == 0 & year == 19 + i),
    Aphids_wood_spring_forward_avg %>%
      filter(n1s > 0 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(n1s_list) <- paste0("n1s_year_", 1:7)



#n1a
n1a_list <- list()
Aphids_wood_autumn_forward_avg = subset(Aphids_wood_autumn_forward_avg, n1a < 1.4)

for (i in 1:7) {
  n1a_list[[i]] <- rbind(
    Aphids_wood_autumn_forward_avg %>%
      filter(n1a == 0 & year == 19 + i),
    Aphids_wood_autumn_forward_avg %>%
      filter(n1a > 0 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(n1a_list) <- paste0("n1a_year_", 1:7)


#B2m annual
B2m_annual_list <- list()

for (i in 1:7) {
  B2m_annual_list[[i]] <- rbind(
    B2m_annual_forward_avg %>%
      filter(B2m == 0 & year == 19 + i),
    B2m_annual_forward_avg %>%
      filter(B2m > 0 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(B2m_annual_list) <- paste0("B2m_annual_year_", 1:7)


#B2m perennial
B2m_perennial_list <- list()

for (i in 1:7) {
  B2m_perennial_list[[i]] <- rbind(
    B2m_perennial_forward_avg %>%
      filter(B2m == 0 & year == 19 + i),
    B2m_perennial_forward_avg %>%
      filter(B2m > 0 && B2m<0.525 & year == 12 + i),
    B2m_perennial_forward_avg %>%
      filter(B2m == 0.525 & year == 19 + i),
    B2m_perennial_forward_avg %>%
      filter(B2m>0.525 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(B2m_perennial_list) <- paste0("B2m_perennial_year_", 1:7)



#B3m annual
B3m_annual_list <- list()

for (i in 1:7) {
  B3m_annual_list[[i]] <- rbind(
    B3m_annual_forward_avg %>%
      filter(B3m == 0 & year == 19 + i),
    B3m_annual_forward_avg %>%
      filter(B3m > 0 && B3m<0.525 & year == 12 + i),
    B3m_annual_forward_avg %>%
      filter(B3m == 0.525 & year == 19 + i),
    B3m_annual_forward_avg %>%
      filter(B3m>0.525 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(B3m_annual_list) <- paste0("B3m_annual_year_", 1:7)


#B3m perennial
B3m_perennial_list <- list()

for (i in 1:7) {
  B3m_perennial_list[[i]] <- rbind(
    B3m_perennial_forward_avg %>%
      filter(B3m == 0 & year == 19 + i),
    B3m_perennial_forward_avg %>%
      filter(B3m > 0 && B3m<0.525 & year == 12 + i),
    B3m_perennial_forward_avg %>%
      filter(B3m == 0.525 & year == 19 + i),
    B3m_perennial_forward_avg %>%
      filter(B3m>0.525 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(B3m_perennial_list) <- paste0("B3m_perennial_year_", 1:7)

#T2s annual
T2s_annual_list <- list()

for (i in 1:7) {
  T2s_annual_list[[i]] <- rbind(
    T2s_annual_forward_avg %>%
      filter(T2s == 30 & year == 19 + i),
    T2s_annual_forward_avg %>%
      filter(T2s > 30 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(T2s_annual_list) <- paste0("T2s_annual_year_", 1:7)


#T2s perennial
T2s_perennial_list <- list()

for (i in 1:7) {
  T2s_perennial_list[[i]] <- rbind(
    T2s_perennial_forward_avg %>%
      filter(T2s == 30 & year == 19 + i),
    T2s_perennial_forward_avg %>%
      filter(T2s > 30 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(T2s_perennial_list) <- paste0("T2s_perennial_year_", 1:7)


#T3s annual
T3s_annual_list <- list()

for (i in 1:7) {
  T3s_annual_list[[i]] <- rbind(
    T3s_annual_forward_avg %>%
      filter(T3s == 30 & year == 19 + i),
    T3s_annual_forward_avg %>%
      filter(T3s > 30 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(T3s_annual_list) <- paste0("T3s_annual_year_", 1:7)


#T3s perennial
T3s_perennial_list <- list()

for (i in 1:7) {
  T3s_perennial_list[[i]] <- rbind(
    T3s_perennial_forward_avg %>%
      filter(T3s == 30 & year == 19 + i),
    T3s_perennial_forward_avg %>%
      filter(T3s > 30 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(T3s_perennial_list) <- paste0("T3s_perennial_year_", 1:7)


#Mowing early nomort
Mowing_early_nomort_list <- list()

for (i in 1:7) {
  Mowing_early_nomort_list[[i]] <- rbind(
    Mowing_early_nomort_forward_avg %>%
      filter(M2s == 0 & year == 19 + i),
    Mowing_early_nomort_forward_avg %>%
      filter(M2s > 0 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(Mowing_early_nomort_list) <- paste0("Mowing_early_nomort_year_", 1:7)


#Mowing early mort
Mowing_early_mort_list <- list()

for (i in 1:7) {
  Mowing_early_mort_list[[i]] <- rbind(
    Mowing_early_mort_forward_avg %>%
      filter(M2s == 0 & year == 19 + i),
    Mowing_early_mort_forward_avg %>%
      filter(M2s > 0 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(Mowing_early_mort_list) <- paste0("Mowing_early_mort_year_", 1:7)


#Mowing late nomort
Mowing_late_nomort_list <- list()

for (i in 1:7) {
  Mowing_late_nomort_list[[i]] <- rbind(
    Mowing_late_nomort_forward_avg %>%
      filter(M3s == 0 & year == 19 + i),
    Mowing_late_nomort_forward_avg %>%
      filter(M3s > 0 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(Mowing_late_nomort_list) <- paste0("Mowing_late_nomort_year_", 1:7)


#Mowing late mort
Mowing_late_mort_list <- list()

for (i in 1:7) {
  Mowing_late_mort_list[[i]] <- rbind(
    Mowing_late_mort_forward_avg %>%
      filter(M3s == 0 & year == 19 + i),
    Mowing_late_mort_forward_avg %>%
      filter(M3s > 0 & year == 12 + i)
  )
}

# Optionally, name the list elements for easier reference
names(Mowing_late_mort_list) <- paste0("Mowing_late_mort_year_", 1:7)

