rm(list=ls()) #removes all variables

# The model simulates how different landscape compositions, 
# including early-season crops, late-season crops, flower-rich field margins, 
# and woody areas, affect the survival and effectiveness of hoverflies as natural 
# pest control agents of aphids. 
# The model contains 3 distinct habitats: (1) Woody habitat, (2) an early crop with a 
# flower margin and (3) a late crop with a flower margin.

# For an in-depth model description, please check out our publication and its supplementary material:
#Mansier, L., & van Rijn, P. C. (2024). 
#Modelling agricultural landscape complementation for natural pest control. 
#Journal of Applied Ecology, 61(11), 2701-2716. 
#and 
#Mansier, L., ten Brink J.A., Janssen A. & van Rijn, P.C.J. 
#How semi-natural habitats can maximally support nautral pest control. [in prep].

#These publications are part of doctoral dissertation of Laura Mansier: 
#The wonderful life of natural enemies – The role of landscape complementation in natural pest control (Chapter 3 and 4).

#Overview of script:
#1. All the supporting functions and ODEs/DDEs are given
#2. Parameters are stated in a different script (Pars_models)
#3. Simulation is run in a different script (Run_model_all)
#4. Data frame of output is generated and can be saved

###This script contains all functions and the different models###

###Functions of the model (for plotting/output, not for the model run)####
#functions to get data and day in the year##
GetDay <- function(Month, Day) {
  DaysPerMonth = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  MonthOrder = c("Jan", "Feb", "March", "April", "May", "June", "July", "Aug", "Sep", "Okt", "Nov", "Dec")
  index = which(MonthOrder == Month)
  if(Month == "Jan") {
    Result = Day
  } else {
    Result = sum(DaysPerMonth[1:(index - 1)]) + Day
  }
  Result = Result - 91
  if(Result < 0) {
    Result = "Winter"
  }
  if(Result > 209) {
    Result = "Winter"
  }
  return(Result)
}
GetDate2 <- function(Modelday) {
  Days = (Modelday %% 210) + 91
  DaysPerMonth = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  CumDays = cumsum(DaysPerMonth)
  MonthOrder = c("Jan", "Feb", "March", "April", "May", "June", "July", "Aug", "Sep", "Okt", "Nov", "Dec")
  if(Days > 31) {
    Month_index = which(CumDays >= Days)[1]
    Month = MonthOrder[Month_index]
    Day = Days - CumDays[Month_index - 1]
  } else {
    Month = "Jan"
    Day = Days
  }
  result = paste(Day, Month, sep = " ")
  return(result)
}
GetDate <- Vectorize(GetDate2)


Temp <- function(t, pars) {
  x <- t %% 210
  #Temperature change within a year
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  return(Temp)
}



##Temperature correction###
Temp_cor <- function(t, pars) {
  x <- t %% 210
  #Temperature change within a year
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"] 
  #Temperature correction
  tc <- (Temp - 4) / (22 - 4)
  return(as.numeric(tc))
}

##Flower density###
FlowersB1 <- function(t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  B1 <- pars["B1l"] +  pars["B1m"] * dweibull((x - pars["T1s"]) / pars["T1d"], pars["k1"], 1) * pars["F1"]
  return(B1)
}
FlowersB2 <- function(t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  #Mowing function
  m <- 1 - dweibull((x - pars["M2s"])/pars["Md"], pars["km"], 1) * pars["FM2"]
  B2z <- pars["B23l"] + pars["B2m"] * dweibull((x - pars["T2s"]) / pars["T2d"], pars["ks"], 1) * pars["F2"]
  B2 <- B2z * m
  return(as.numeric(B2))
}
FlowersB3 <- function(t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  B3 <- pars["B23l"] + pars["B3m"] * dweibull((x - pars["T3s"]) / pars["T3d"], pars["ks"], 1) * pars["F3"]
  return(B3)
}

##Aphids growth rate##
r1_aphids <- function(t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  n1 <- pars["n1s"] * dweibull((x - pars["A1s"]) / pars["A1d"], pars["ks"], 1) * pars["SA1"] + pars["n1a"] * dweibull((x - pars["A1as"]) / pars["A1ad"], pars["ks"], 1) * pars["AA1"]
  #Aphid population growth rate per habitat
  r1 <- n1 * pars["rN1"] * tc
  return(as.numeric(r1))
}
r2_aphids <- function(t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  K2 <- pars["Km"] * dweibull((pars["A2f"] - x) / pars["A2d"], pars["ks"], 1) * pars["A2"] + pars["phi"]
  n2 <- ifelse(K2 > 0.01 * pars["Km"], 1, 0)
  r2 <- n2 * pars["rN2"] * tc
  return(as.numeric(r2))
  }
r3_aphids <- function(t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  K3 <- pars["Km"] * dweibull((pars["A3f"] - x) / pars["A3d"], pars["ks"], 1) * pars["A3"] + pars["phi"]
  n3 <- ifelse(K3 > 0.01 * pars["Km"], 1, 0)
  r3 <- n3 * pars["rN3"] * tc
  return(as.numeric(r3))
}

##Juvenile hoverflies functions#
Juv_dev <- function(prey, t, pars){
  x <- t %% 210 
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  result = pars["em"] * (prey / (prey + pars["Ne"]) + phi) * tc
  as.numeric(result)
}
Juv_mort <- function(prey, t, pars){
  x <- t %% 210 
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  mort <- pars["mum"] * ((prey + pars["Ne"] * 0.5) / (prey + pars["epsilon"])) * tc
  return(as.numeric(mort))
}

#Repro rates adult hoverflies (including with multiplication of saf!)
R1fnc <- function(prey, t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  B1 <- FlowersB1(t = t, pars = pars)
  Q1 <- ((1 / pars["db"]) + (1 / pars["a"]) + (1 / (pars["b"] * B1)) + (1 / pars["da"]))^-1
  saf <- Q1 / pars["a"]
  G <- pars["G1m"]* tc * (prey / (prey + pars["Ng"]))
  return(as.numeric(saf * G))} 
R2fnc <- function(prey, t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  B2 <- FlowersB2(t = t, pars = pars)
  Q2 <- ((1 / pars["db"]) + (1 / pars["a"]) + (1 / (pars["b"] * B2)) + (1 / pars["da"]))^-1
  saf <- Q2 / pars["a"]
  G <- pars["G2m"] * tc * (prey / (prey + pars["Ng"]))
  return(saf * G)}
R3fnc <- function(prey, t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  B3 <- FlowersB3(t = t, pars = pars)
  Q3 <- ((1 / pars["db"]) + (1 / pars["a"]) + (1 / (pars["b"] * B3)) + (1 / pars["da"]))^-1
  saf <- Q3 / pars["a"]
  G <- pars["G2m"] * tc * (prey / (prey + pars["Ng"]))
  return(saf * G)}

#adult hoverflies mortality rates 
muA1fnc <- function(t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  B1 <- FlowersB1(t = t, pars = pars)
  Q1 <- ((1 / pars["db"]) + (1 / pars["a"]) + (1 / (pars["b"] * B1)) + (1 / pars["da"]))^-1
  sbi <- Q1 / (pars["b"] * B1)
  mu <- pars["muP"] * tc * sbi
  return(mu)
}
muA2fnc <- function(t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  B2 <- FlowersB2(t = t, pars = pars)
  Q2 <- ((1 / pars["db"]) + (1 / pars["a"]) + (1 / (pars["b"] * B2)) + (1 / pars["da"]))^-1
  sbi <- Q2 / (pars["b"] * B2)
  mu <- pars["muP"] * tc * sbi
  return(mu)}
muA3fnc<- function(t, pars) {
  x <- t %% 210
  Temp <- pars["Ta"] * cos((x - pars["td"]) * 2 * pi / 365) + pars["Tc"]
  tc <- (Temp - 4) / (22 - 4)
  B3 <- FlowersB3(t = t, pars = pars)
  Q3 <- ((1 / pars["db"]) + (1 / pars["a"]) + (1 / (pars["b"] * B3)) + (1 / pars["da"]))^-1
  sbi <- Q3 / (pars["b"] * B3)
  mu <- pars["muP"] * tc * sbi
  return(mu)}

#R0 functions#
R01fnc <- function(prey, t, pars) {
  GG <- R1fnc(prey = prey, t = t, pars = pars) ##Repro adult 
  muA <- muA1fnc(t = t, pars = pars) #mortality adults
  e2 <- Juv_dev(prey = prey, t = t, pars = pars) #development
  muj <- Juv_mort(prey = prey, t = t, pars = pars) #mort rate
  R0 <- GG * (muA)^-1 * exp(-muj / e2)
  return(as.numeric(R0))
}
R02fnc <- function(prey, t, pars) {
  GG <- R2fnc(prey = prey, t = t, pars = pars) ##Repro adult 
  muA <- muA2fnc(t = t, pars = pars) #mortality adults
  e2 <- Juv_dev(prey = prey, t = t, pars = pars) #development
  muj <- Juv_mort(prey = prey, t = t, pars = pars) #mort rate
  R0 <- GG * (muA)^-1 * exp(-muj / e2)
  return(as.numeric(R0))
}
R03fnc <- function(prey, t, pars) {
  GG <- R3fnc(prey = prey, t = t, pars = pars) ##Repro adult 
  muA <- muA3fnc(t = t, pars = pars) #mortality adults
  e2 <- Juv_dev(prey = prey, t = t, pars = pars) #development
  muj <- Juv_mort(prey = prey, t = t, pars = pars) #mort rate
  R0 <- GG * (muA)^-1 * exp(-muj / e2)
  return(as.numeric(R0))
}

#split per life-stage#
R0fnc_juv <- function(prey, t, pars) {
  e2 <- Juv_dev(prey = prey, t = t, pars = pars) #development
  muj <- Juv_mort(prey = prey, t = t, pars = pars) #mort rate
  R0 <- exp(-muj / e2)
  return(R0)
}
R01fnc_ad <- function(prey, t, pars) {
  GG <- R1fnc(prey = prey, t = t, pars = pars) ##Repro adult 
  muA <- muA1fnc(t = t, pars = pars) #mortality adults
  R0 <- GG * (muA)^-1 
  return(R0)
}
R02fnc_ad <- function(prey, t, pars) {
  GG <- R2fnc(prey = prey, t = t, pars = pars) ##Repro adult 
  muA <- muA2fnc(t = t, pars = pars) #mortality adults
  R0 <- GG * (muA)^-1 
  return(R0)
}
R03fnc_ad <- function(prey, t, pars) {
  GG <- R3fnc(prey = prey, t = t, pars = pars) ##Repro adult 
  muA <- muA3fnc(t = t, pars = pars) #mortality adults
  R0 <- GG * (muA)^-1 
  return(R0)
}


#Dispersal rates of adults##
D1fnc <- function(prey, t, pars) {
  R0 <- R01fnc(prey = prey, t = t, pars = pars)
  disp <- pars["D0"]/(R0 + 0.01 * prey + 1)
  return(as.numeric(disp))
}
D2fnc <- function(prey, t, pars) {
  R0 <- R02fnc(prey = prey, t = t, pars = pars)
  disp <- pars["D0"]/(R0 + 0.01 * prey + 1)
  return(as.numeric(disp))
}
D3fnc <- function(prey, t, pars) {
  R0 <- R03fnc(prey = prey, t = t, pars = pars)
  disp <- pars["D0"]/(R0 + 0.01 * prey + 1)
  return(as.numeric(disp))
}

###function to name output####
GiveNames <- function(out, pars, global = F) {
  data_frame_PopDyn <- data.frame(time=out[,1], Aphids_wood=out[,2], Aphids_early=out[,3], 
                                  Aphids_late=out[,4], Adult_hib=out[,5], 
                                  Larvae_wood=out[,6], Adult_wood=out[,7], 
                                  Larvae_early=out[,8], Adult_early=out[,9], 
                                  Larvae_late=out[,10], Adult_late=out[,11], Adult_disp=out[,12])
  data_frame_PopDyn$day = data_frame_PopDyn$time %% 210
  data_frame_PopDyn$year = data_frame_PopDyn$time %/% 210
  data_frame_PopDyn$Aphids <- data_frame_PopDyn$Aphids_wood + data_frame_PopDyn$Aphids_early +
    data_frame_PopDyn$Aphids_late
  data_frame_PopDyn$Larvae <- data_frame_PopDyn$Larvae_wood + data_frame_PopDyn$Larvae_early +
    data_frame_PopDyn$Larvae_late
  data_frame_PopDyn$Adults <- data_frame_PopDyn$Adult_wood + data_frame_PopDyn$Adult_early +
    data_frame_PopDyn$Adult_late + data_frame_PopDyn$Adult_disp +
    data_frame_PopDyn$Adult_hib
  data_frame_PopDyn$Aphids_wood_sus <- max(data_frame_PopDyn$Aphids_wood - pars["Nr"], 0)
  data_frame_PopDyn$B1 <- FlowersB1(t = data_frame_PopDyn$time, pars = pars)
  data_frame_PopDyn$B2 <- FlowersB2(t = data_frame_PopDyn$time, pars = pars)
  data_frame_PopDyn$B3 <- FlowersB3(t = data_frame_PopDyn$time, pars = pars)
  data_frame_PopDyn$Hoverflies = data_frame_PopDyn$Adults + data_frame_PopDyn$Larvae
  data_frame_PopDyn$devrate_wood <- Juv_dev(prey = data_frame_PopDyn$Aphids_wood_sus,
                                                t = data_frame_PopDyn$time,
                                                pars = pars)
  data_frame_PopDyn$devrate_early <- Juv_dev(prey = data_frame_PopDyn$Aphids_early,
                                                 t = data_frame_PopDyn$time,
                                                 pars = pars)
  data_frame_PopDyn$devrate_late <- Juv_dev(prey = data_frame_PopDyn$Aphids_late,
                                                t = data_frame_PopDyn$time,
                                                pars = pars)
  data_frame_PopDyn$mortrateL_wood <- Juv_mort(prey = data_frame_PopDyn$Aphids_wood_sus, 
                                               t = data_frame_PopDyn$time,
                                               pars = pars)
  data_frame_PopDyn$mortrateL_early <- Juv_mort(prey = data_frame_PopDyn$Aphids_early, 
                                                t = data_frame_PopDyn$time,
                                                pars = pars)
  data_frame_PopDyn$mortrateL_late <- Juv_mort(prey = data_frame_PopDyn$Aphids_late, 
                                               t = data_frame_PopDyn$time,
                                               pars = pars)
  data_frame_PopDyn$Reprorate_wood <- R1fnc(prey = data_frame_PopDyn$Aphids_wood_sus,
                                            t = data_frame_PopDyn$time,
                                            pars = pars)
  data_frame_PopDyn$Reprorate_early <- R2fnc(prey = data_frame_PopDyn$Aphids_early,
                                             t = data_frame_PopDyn$time,
                                             pars = pars)
  data_frame_PopDyn$Reprorate_late <- R3fnc(prey = data_frame_PopDyn$Aphids_late,
                                            t = data_frame_PopDyn$time,
                                            pars = pars)
  data_frame_PopDyn$mortrateA_wood <- muA1fnc(t = data_frame_PopDyn$time,
                                              pars = pars)
  data_frame_PopDyn$mortrateA_early <- muA2fnc(t = data_frame_PopDyn$time,
                                               pars = pars)
  data_frame_PopDyn$mortrateA_late <- muA3fnc(t = data_frame_PopDyn$time,
                                              pars = pars)
  data_frame_PopDyn$R0_wood <- R01fnc(prey = data_frame_PopDyn$Aphids_wood_sus, 
                                      t = data_frame_PopDyn$time,
                                      pars = pars)
  data_frame_PopDyn$R0_woodL <- R0fnc_juv(prey =  data_frame_PopDyn$Aphids_wood_sus,
                                          t = data_frame_PopDyn$time,
                                          pars = pars)
  data_frame_PopDyn$R0_woodA <- R01fnc_ad(prey = data_frame_PopDyn$Aphids_wood_sus, 
                                          t = data_frame_PopDyn$time,
                                          pars = pars)
  data_frame_PopDyn$R0_early <- R02fnc(prey = data_frame_PopDyn$Aphids_early, 
                                       t = data_frame_PopDyn$time,
                                       pars = pars)
  data_frame_PopDyn$R0_earlyL <- R0fnc_juv(prey =  data_frame_PopDyn$Aphids_early,
                                           t = data_frame_PopDyn$time,
                                           pars = pars)
  data_frame_PopDyn$R0_earlyA <- R02fnc_ad(prey = data_frame_PopDyn$Aphids_early,
                                           t = data_frame_PopDyn$time,
                                           pars = pars)
  data_frame_PopDyn$R0_late <- R03fnc(prey = data_frame_PopDyn$Aphids_late, 
                                      t = data_frame_PopDyn$time,
                                      pars = pars)
  data_frame_PopDyn$R0_lateL <- R0fnc_juv(prey =  data_frame_PopDyn$Aphids_late,
                                          t = data_frame_PopDyn$time,
                                          pars = pars)
  data_frame_PopDyn$R0_lateA <- R03fnc_ad(prey = data_frame_PopDyn$Aphids_late, 
                                          t = data_frame_PopDyn$time,
                                          pars = pars)
  data_frame_PopDyn$Disp_wood <- D1fnc(prey = data_frame_PopDyn$Aphids_wood_sus, 
                                        t = data_frame_PopDyn$time,
                                        pars = pars)
  data_frame_PopDyn$Disp_early <- D2fnc(prey = data_frame_PopDyn$Aphids_early, 
                                         t = data_frame_PopDyn$time,
                                         pars = pars)
  data_frame_PopDyn$Disp_late <- D3fnc(prey = data_frame_PopDyn$Aphids_late, 
                                        t = data_frame_PopDyn$time,
                                        pars = pars)
  data_frame_PopDyn$Immigration = pars["D0"] * data_frame_PopDyn$Adult_disp
  ##Note that these are NOT absolute numbers but in per m2
  data_frame_PopDyn$Emmigration_wood = data_frame_PopDyn$Disp_wood * data_frame_PopDyn$Adult_wood
  data_frame_PopDyn$Emmigration_early = data_frame_PopDyn$Disp_wood * data_frame_PopDyn$Adult_early
  data_frame_PopDyn$Emmigration_late = data_frame_PopDyn$Disp_wood * data_frame_PopDyn$Adult_late
  data_frame_PopDyn$Migration_wood = data_frame_PopDyn$Immigration - data_frame_PopDyn$Emmigration_wood
  data_frame_PopDyn$Migration_early = data_frame_PopDyn$Immigration - data_frame_PopDyn$Emmigration_early
  data_frame_PopDyn$Migration_late = data_frame_PopDyn$Immigration - data_frame_PopDyn$Emmigration_late
  name = deparse(substitute(out))
  if(global) {
  assign(name, data_frame_PopDyn, .GlobalEnv)
  } else {
    return(data_frame_PopDyn)
  }
}


###Model specification####

###Model with R0 and Refuge (hibernation from wood)
lvpred_ref_hibwood = function(t, n, parms){
  
  with(as.list(parms), {
    N1=n[1]; N2=n[2]; N3=n[3]; P0=n[4]; p1=n[5]; P1=n[6]; p2=n[7]; P2=n[8]; p3=n[9]; P3=n[10]; PD=n[11]
    
    ####SUPPORTING FUNCTIONS####
    
    #SEASONAL FORCING FUNCTIONS varying over days
    # x is day of the year used as input for seasonal forcing functions for multiple years
    x <- t %% 210
    #Temperature change within a year
    Temp <- Ta * cos((x - td) * 2 * pi / 365) + Tc 
    #Temperature correction
    tc <- (Temp - 4) / (22 - 4)
    
    
    #Floral F
    #Functions flower food per habitat
    B1 <- B1l +  B1m * dweibull((x - T1s) / T1d, k1, 1) * F1 
    B2z <- B23l + B2m * dweibull((x - T2s) / T2d, ks, 1) * F2
    B3z <- B23l + B3m * dweibull((x - T3s) / T3d, ks, 1) * F3
    
    #Mowing function
    m_2 <- 1 - dweibull((x - M2s)/Md, km, 1) * FM2
    m_3 <- 1 - dweibull((x - M3s)/Md, km, 1) * FM3
    B2 <- B2z * m_2
    B3 <- B3z * m_3
    
    #APHID GROWTH
    #Functions aphid resource availability and carrying capacity (Weibull, reverse Weibull) per habitat
    K2 <- Km * dweibull((A2f - x) / A2d, ks, 1) * A2 + phi
    K3 <- Km * dweibull((A3f - x) / A3d, ks, 1) * A3 + phi
    n1 <- n1s * dweibull((x - A1s) / A1d, ks, 1) * SA1 + n1a * dweibull((x - A1as) / A1ad, ks, 1) * AA1
    n2 <- ifelse(K2 > 0.01 * Km, 1, 0)
    n3 <- ifelse(K3 > 0.01 * Km, 1, 0)
    
    #Aphid population growth rate per habitat
    r1 <- n1 * rN1 * tc
    r2 <- n2 * rN2 * tc
    r3 <- n3 * rN3 * tc
    
    
    #HOVERFLY LIFE HISTORY
    #Type 2 functional response hoverflies to aphid density N
    f <- function(N) {fm * tc * (N / (N + Nf))}
    
    #Juvenile developmental rate hoverflies [with delay]
    e <- function(N, t){
      x2 <- t %% 210 
      Temp2 <- Ta * cos((x2 - td) * 2 * pi / 365) + Tc
      tc2 <- (Temp2 - 4) / (22 - 4)
      em * (N / (N + Ne) + phi) * tc2
    }
    #Juvenile developmental rate hoverflies [without delay[]
    e2 <- function(N){em * (N / (N + Ne) + phi) * tc}
    
    #Juvenile mortality rate hoverflies
    muj <- function(N){mum * ((N + Ne * 0.5) / (N + epsilon)) * tc}
    
    #Reproduction rate hoverflies (lower in H1)
    G1 <- function(N){G1m * tc * (N / (N + Ng))}
    G2 <- function(N){G2m * tc * (N / (N + Ng))}
    
    #HOVERFLY FORAGING BETWEEN SUB-HABITATS
    #Within-habitat distribution model (assuming equilibrium)
    Q1 <- ((1 / db) + (1 / a) + (1 / (b * B1)) + (1 / da))^-1
    Q2 <- ((1 / db) + (1 / a) + (1 / (b * B2)) + (1 / da))^-1
    Q3 <- ((1 / db) + (1 / a) + (1 / (b * B3)) + (1 / da))^-1
    
    #Proportion ill fed in flower habitat -> mortality
    sbi <- function(B, Q){Q / (b * B)}
    #proportion well fed in flower habitat
    sbf <- function(Q){Q / da}
    #proportion ill fed in aphid habitat
    sai <- function(Q){Q / db}
    #proportion well fed in aphid habitat -> reproduction
    saf <- function(Q){Q / a}
    
    
    # total proportion on flowers [NEVER USED?]
    sf <- function(B, Q){sbi(B, Q) + sbf(Q)}
    
    # REFUGE (Nr) for aphids in WOODY habitat 1, Nv= part vulnerable for predation #*
    Nv <-  max(N1 - Nr, 0)
    
    Nv_lag <- function(N){
      max(N - Nr,0)
    }
    
    #HOVERFLY DISPERSAL AMONG HABITATS
    #Dispersal from Ph into disperser pool (induced by lack of resources locally = inversely related to local fitness R0)
    
    # R0: daily reproduction x female longevity x juvenile survival
    R0_h1 <- function(N, B, Q) {
      G1(N) * saf(Q) * (tc * muP * sbi(B, Q))^-1 * exp(-muj(N) / e2(N))
    }
    R0_h2 <- function(N, B, Q) {
      G2(N) * saf(Q) * (tc * muP * sbi(B, Q))^-1 * exp(-muj(N) / e2(N)) 
    }
    
    #Emigration equal to immigration when no food present, otherwise inversely related to R0, but when R0=0 still affected by N:
    D_h1 <- function(N, B, Q){D0 / (R0_h1(N, B, Q) + 0.01 * N + 1)}
    D_h2 <- function(N, B, Q){D0 / (R0_h2(N, B, Q) + 0.01 * N + 1)}
    D1 <- D_h1(Nv, B1, Q1)
    D2 <- D_h2(N2, B2, Q2)
    D3 <- D_h2(N3, B3, Q3)
    
    #HOVERFLY HIBERNATION
    #Release from hibernation (around day H0=30) h0
    h0 <- sigma0 * dnorm(x, H0, sigma0)
    
    #Induction into hibernation (around day H1=183) h1  
    h1 <- sigma1 * dnorm(x, H1, sigma1)
    #HOVERFLY INTERMEDIATE PHASE (non-reproductive and non-predatory)
    # Delay representing hoverfly intermediate phase
    tau <-  tauh/tc
    
    #MORTALITY
    #Mortality events habitat 2 and 3 (e.g. spraying insecticide and mowing)
    etha2 = function(S){
      ifelse(Is2 < x && x < (Is2 + 3), -log(S) / 3,0) * I2
    }
    etha3 = function(S){
      ifelse(Is3 < x && x < (Is3 + 3), -log(S) / 3,0) * I3
    }
    
    
    #Seasonal mortality enforced over last day of year or after harvest in habitat 2 & 3
    #low mortality (some winter survival)
    ml <- function(WNP, Wth){
      ifelse(Wth < x && x < (Wth + 1), -log(WNP), 0)
    }
    
    #maximum mortality (no survival)
    mm <- function(W, Wth){
      ifelse(Wth < x &&  x < (Wth + 1), -log(W), 0)
    }
    mm1 <- mm(W, Wt1)
    
    
    
    #INFESTATION
    #Yearly infestation of annual habitat with aphids
    inN2 <- ifelse(T2i < x && x< (T2i + 1), N2i, 0)
    inN3 <- ifelse(T3i < x && x < (T3i + 1), N3i, 0)
    
    ####STATE VARIABLE DERIVATIVES (DIFFERENTIAL EQUATIONS)####
    
    #N1 Aphids woody habitat [growth - background mortality - juvenile hoverfly predation - winter mortality]
    dn1dt = r1 * (1 - pmin(N1 / K1, 1)) * N1 - mu1 * tc * Nv - p1 * f(Nv) - ml(WN, Wt1) * N1
    #N2 Aphids winter crop [invasion + growth - background mortality - juvenile hoverfly predation - winter mortality - spraying mortality]
    dn2dt = inN2 + r2 * (1 - pmin(N2 / K2, 1)) * N2 - mu2 * tc * N2 - p2 * f(N2) - mm(W, Wt2) * N2 - etha2(sN) * N2
    #N3 Aphids summer crop  [invasion + growth - background mortality - juvenile hoverfly predation - winter mortality - spraying mortality]
    dn3dt = inN3 + r3 * (1 - pmin(N3 / K3, 1)) * N3 - mu3 * tc * N3 - p3 * f(N3) - mm(W, Wt3) * N3 - etha3(sN) * N3
    #P0 hibernation pool from woody habitat only [Hibernation - endHib - mortality]
    dn4dt = h1 * P1 - h0 * P0 - mu0 * tc * P0 - ml(WP, Wt1) * P0 
    #p1 juvenile H in woody habitat [repro - mortality - development - winter mortality]
    dn5dt = G1(Nv) * saf(Q1) * P1 - muj(Nv) * p1 - e2(Nv) * p1 - mm1 * p1
    #delay for P1 [juvenile hoverfles in woody habitat]
    lag1 <- ifelse((x - tau) < 0, 0, e(Nv_lag(lagvalue(t - tau, 1)), t - tau) * lagvalue(t - tau, 5))
    #P1 adult H in woody habitat [development + dispersal? - starve mortality - dispersal - winter mortality]
    dn6dt = lag1 + D0 * PD - muP * tc * sbi(B1, Q1) * P1 - D1 * P1 - mm1 * P1 + h0 * P0 - h1 * P1
    #p2 juvenile H in crop 1
    dn7dt = G2(N2) * saf(Q2) * P2 - muj(N2) * p2 - e2(N2) * p2 - mm1 * p2 - etha2(sN) * p2
    #delay for P2
    lag2 <- ifelse((x - tau) < 0, 0, e(lagvalue(t - tau, 2), t - tau) * lagvalue(t - tau, 7))
    #P2 adult H in crop 1
    dn8dt = lag2 + D0*PD - muP * tc * sbi(B2, Q2) * P2 - D2 * P2 - mm1 * P2 - etha2(sP) * P2 
    #p3 juvenile H in crop 2
    dn9dt = G2(N3) * saf(Q3) * P3 - muj(N3) * p3 - e2(N3) * p3 - mm1 * p3 - etha3(sN) * p3
    #delay for P3
    lag3 = ifelse((x - tau) < 0, 0, e(lagvalue(t - tau, 3), t - tau) * lagvalue(t - tau, 9))
    #P3 adult H in crop 2
    dn10dt = lag3 + D0 * PD - muP * tc * sbi(B3, Q3) * P3 - D3 * P3 - mm1 * P3 - etha3(sP) * P3  
    #PD [something from P0, dispersal from three habitats] h1 = hibernation
    dn11dt = D1 * P1 * alpha1 + D2 * P2 * alpha2 + D3 * P3 * alpha3 - D0 * PD - muPD * tc * PD - mm1 * PD
    
    list(c(dn1dt, dn2dt, dn3dt, dn4dt, dn5dt, dn6dt, dn7dt, dn8dt, dn9dt, dn10dt, dn11dt))
  })
}

#model for the runs with additional mortality of adult hoverflies in the flower margins due to mowing
lvpred_ref_hibwood_mowingmort = function(t, n, parms){
  
  with(as.list(parms), {
    N1=n[1]; N2=n[2]; N3=n[3]; P0=n[4]; p1=n[5]; P1=n[6]; p2=n[7]; P2=n[8]; p3=n[9]; P3=n[10]; PD=n[11]
    
    ####SUPPORTING FUNCTIONS####
    
    #SEASONAL FORCING FUNCTIONS varying over days
    # x is day of the year used as input for seasonal forcing functions for multiple years
    x <- t %% 210
    #Temperature change within a year
    Temp <- Ta * cos((x - td) * 2 * pi / 365) + Tc 
    #Temperature correction
    tc <- (Temp - 4) / (22 - 4)
    
    
    #Floral F
    #Functions flower food per habitat
    B1 <- B1l +  B1m * dweibull((x - T1s) / T1d, k1, 1) * F1 
    B2z <- B23l + B2m * dweibull((x - T2s) / T2d, ks, 1) * F2
    B3z <- B23l + B3m * dweibull((x - T3s) / T3d, ks, 1) * F3
    
    #Mowing function
    m_2 <- 1 - dweibull((x - M2s)/Md, km, 1) * FM2
    m_3 <- 1 - dweibull((x - M3s)/Md, km, 1) * FM3
    B2 <- B2z * m_2
    B3 <- B3z * m_3
    
    #APHID GROWTH
    #Functions aphid resource availability and carrying capacity (Weibull, reverse Weibull) per habitat
    K2 <- Km * dweibull((A2f - x) / A2d, ks, 1) * A2 + phi
    K3 <- Km * dweibull((A3f - x) / A3d, ks, 1) * A3 + phi
    n1 <- n1s * dweibull((x - A1s) / A1d, ks, 1) * SA1 + n1a * dweibull((x - A1as) / A1ad, ks, 1) * AA1
    n2 <- ifelse(K2 > 0.01 * Km, 1, 0)
    n3 <- ifelse(K3 > 0.01 * Km, 1, 0)
    
    #Aphid population growth rate per habitat
    r1 <- n1 * rN1 * tc
    r2 <- n2 * rN2 * tc
    r3 <- n3 * rN3 * tc
    
    
    #HOVERFLY LIFE HISTORY
    #Type 2 functional response hoverflies to aphid density N
    f <- function(N) {fm * tc * (N / (N + Nf))}
    
    #Juvenile developmental rate hoverflies [with delay]
    e <- function(N, t){
      x2 <- t %% 210 
      Temp2 <- Ta * cos((x2 - td) * 2 * pi / 365) + Tc
      tc2 <- (Temp2 - 4) / (22 - 4)
      em * (N / (N + Ne) + phi) * tc2
    }
    #Juvenile developmental rate hoverflies [without delay[]
    e2 <- function(N){em * (N / (N + Ne) + phi) * tc}
    
    #Juvenile mortality rate hoverflies
    muj <- function(N){mum * ((N + Ne * 0.5) / (N + epsilon)) * tc}
    
    #Reproduction rate hoverflies (lower in H1)
    G1 <- function(N){G1m * tc * (N / (N + Ng))}
    G2 <- function(N){G2m * tc * (N / (N + Ng))}
    
    #HOVERFLY FORAGING BETWEEN SUB-HABITATS
    #Within-habitat distribution model (assuming equilibrium)
    Q1 <- ((1 / db) + (1 / a) + (1 / (b * B1)) + (1 / da))^-1
    Q2 <- ((1 / db) + (1 / a) + (1 / (b * B2)) + (1 / da))^-1
    Q3 <- ((1 / db) + (1 / a) + (1 / (b * B3)) + (1 / da))^-1
    
    #Proportion ill fed in flower habitat -> mortality
    sbi <- function(B, Q){Q / (b * B)}
    #proportion well fed in flower habitat
    sbf <- function(Q){Q / da}
    #proportion ill fed in aphid habitat
    sai <- function(Q){Q / db}
    #proportion well fed in aphid habitat -> reproduction
    saf <- function(Q){Q / a}
    
    
    # total proportion on flowers [NEVER USED?]
    sf <- function(B, Q){sbi(B, Q) + sbf(Q)}
    
    # REFUGE (Nr) for aphids in WOODY habitat 1, Nv= part vulnerable for predation #*
    Nv <-  max(N1 - Nr, 0)
    
    Nv_lag <- function(N){
      max(N - Nr,0)
    }
    
    #HOVERFLY DISPERSAL AMONG HABITATS
    #Dispersal from Ph into disperser pool (induced by lack of resources locally = inversely related to local fitness R0)
    
    # R0: daily reproduction x female longevity x juvenile survival
    R0_h1 <- function(N, B, Q) {
      G1(N) * saf(Q) * (tc * muP * sbi(B, Q))^-1 * exp(-muj(N) / e2(N))
    }
    R0_h2 <- function(N, B, Q) {
      G2(N) * saf(Q) * (tc * muP * sbi(B, Q))^-1 * exp(-muj(N) / e2(N)) 
    }
    
    #Emigration equal to immigration when no food present, otherwise inversely related to R0, but when R0=0 still affected by N:
    D_h1 <- function(N, B, Q){D0 / (R0_h1(N, B, Q) + 0.01 * N + 1)}
    D_h2 <- function(N, B, Q){D0 / (R0_h2(N, B, Q) + 0.01 * N + 1)}
    D1 <- D_h1(Nv, B1, Q1)
    D2 <- D_h2(N2, B2, Q2)
    D3 <- D_h2(N3, B3, Q3)
    
    #HOVERFLY HIBERNATION
    #Release from hibernation (around day H0=30) h0
    h0 <- sigma0 * dnorm(x, H0, sigma0)
    
    #Induction into hibernation (around day H1=183) h1  
    h1 <- sigma1 * dnorm(x, H1, sigma1)
    #HOVERFLY INTERMEDIATE PHASE (non-reproductive and non-predatory)
    # Delay representing hoverfly intermediate phase
    tau <-  tauh/tc
    
    #MORTALITY
    #Mortality events habitat 2 and 3 (e.g. spraying insecticide and mowing)
    #for mowing extra mortality ISh + 2, and for spraying insecticide ISh + 3
    etha2 = function(S){
      ifelse(Is2 < x && x < (Is2 + 2), -log(S) / 3,0) * I2
    }
    etha3 = function(S){
      ifelse(Is3 < x && x < (Is3 + 2), -log(S) / 3,0) * I3
    }
    
    
    #Seasonal mortality enforced over last day of year or after harvest in habitat 2 & 3
    #low mortality (some winter survival)
    ml <- function(WNP, Wth){
      ifelse(Wth < x && x < (Wth + 1), -log(WNP), 0)
    }
    
    #maximum mortality (no survival)
    mm <- function(W, Wth){
      ifelse(Wth < x &&  x < (Wth + 1), -log(W), 0)
    }
    mm1 <- mm(W, Wt1)
    
    
    
    #INFESTATION
    #Yearly infestation of annual habitat with aphids
    inN2 <- ifelse(T2i < x && x< (T2i + 1), N2i, 0)
    inN3 <- ifelse(T3i < x && x < (T3i + 1), N3i, 0)
    
    ####STATE VARIABLE DERIVATIVES (DIFFERENTIAL EQUATIONS)####
    
    #N1 Aphids woody habitat [growth - background mortality - juvenile hoverfly predation - winter mortality]
    dn1dt = r1 * (1 - pmin(N1 / K1, 1)) * N1 - mu1 * tc * Nv - p1 * f(Nv) - ml(WN, Wt1) * N1
    #N2 Aphids winter crop [invasion + growth - background mortality - juvenile hoverfly predation - winter mortality - spraying mortality]
    dn2dt = inN2 + r2 * (1 - pmin(N2 / K2, 1)) * N2 - mu2 * tc * N2 - p2 * f(N2) - mm(W, Wt2) * N2 - etha2(sN) * N2
    #N3 Aphids summer crop  [invasion + growth - background mortality - juvenile hoverfly predation - winter mortality - spraying mortality]
    dn3dt = inN3 + r3 * (1 - pmin(N3 / K3, 1)) * N3 - mu3 * tc * N3 - p3 * f(N3) - mm(W, Wt3) * N3 - etha3(sN) * N3
    #P0 hibernation pool from woody habitat only [Hibernation - endHib - mortality]
    dn4dt = h1 * P1 - h0 * P0 - mu0 * tc * P0 - ml(WP, Wt1) * P0 
    #p1 juvenile H in woody habitat [repro - mortality - development - winter mortality]
    dn5dt = G1(Nv) * saf(Q1) * P1 - muj(Nv) * p1 - e2(Nv) * p1 - mm1 * p1
    #delay for P1 [juvenile hoverfles in woody habitat]
    lag1 <- ifelse((x - tau) < 0, 0, e(Nv_lag(lagvalue(t - tau, 1)), t - tau) * lagvalue(t - tau, 5))
    #P1 adult H in woody habitat [development + dispersal? - starve mortality - dispersal - winter mortality]
    dn6dt = lag1 + D0 * PD - muP * tc * sbi(B1, Q1) * P1 - D1 * P1 - mm1 * P1 + h0 * P0 - h1 * P1
    #p2 juvenile H in crop 1
    dn7dt = G2(N2) * saf(Q2) * P2 - muj(N2) * p2 - e2(N2) * p2 - mm1 * p2 - etha2(sN) * p2
    #delay for P2
    lag2 <- ifelse((x - tau) < 0, 0, e(lagvalue(t - tau, 2), t - tau) * lagvalue(t - tau, 7))
    #P2 adult H in crop 1
    dn8dt = lag2 + D0*PD - muP * tc * sbi(B2, Q2) * P2 - D2 * P2 - mm1 * P2 - etha2(sP) * P2 * (sbi(B2, Q2) +sbf(Q2)) 
    #p3 juvenile H in crop 2
    dn9dt = G2(N3) * saf(Q3) * P3 - muj(N3) * p3 - e2(N3) * p3 - mm1 * p3 - etha3(sN) * p3
    #delay for P3
    lag3 = ifelse((x - tau) < 0, 0, e(lagvalue(t - tau, 3), t - tau) * lagvalue(t - tau, 9))
    #P3 adult H in crop 2
    dn10dt = lag3 + D0 * PD - muP * tc * sbi(B3, Q3) * P3 - D3 * P3 - mm1 * P3 - etha3(sP) * P3 * (sbi(B3, Q3) +sbf(Q3))  
    #PD [something from P0, dispersal from three habitats] h1 = hibernation
    dn11dt = D1 * P1 * alpha1 + D2 * P2 * alpha2 + D3 * P3 * alpha3 - D0 * PD - muPD * tc * PD - mm1 * PD
    
    list(c(dn1dt, dn2dt, dn3dt, dn4dt, dn5dt, dn6dt, dn7dt, dn8dt, dn9dt, dn10dt, dn11dt))
  })
}
###Function to save data
###Bifurcation function####
BifRun <- function(modelname, ##name of ode model
                   pars,  ##name of parameter vector
                   dataname,##name to save the model to
                   bifname,  ##name of bifparameter
                   minbif,  #minimum value of the bifpar
                   maxbif,  #maximum value of the bifpar
                   stepsize, #stepsize of the bif
                   direction = "for", #direction of the bif
                   RunYears = 10 #number of years to run the simulation (first run is twice as long!)
                   ) {
  #Initialize#
  start_time_bif <- Sys.time()
  Timedata <- data.frame()
  default_par <- pars[bifname] ##Remember the default value of the bifpar
  ParVal <- seq(minbif, maxbif, stepsize) 
  if(direction != 'for') {
    ParVal <- ParVal[(length(ParVal):1)]
  }
  i = 0
  times <- seq(0, 210 * RunYears * 2)
  initialN <- c(N1i, 0, 0, Pi, 0, 0, 0, 0, 0, 0, 0)
  ##loop##
  for(i in 1:length(ParVal)){
    pars[bifname] <- ParVal[i]
    start_time <- Sys.time()
    out <- dede(y = initialN, times = times, 
                func = modelname, 
                parms = pars, 
                control = list(mxhist = 1e6))
    end_time <- Sys.time()
    initialN <- out[nrow(out), c(2:12)] ##Get the new initial pop
    time.taken <- end_time - start_time
    out <- GiveNames(out, pars = pars) #give names 
    subdata <- subset(out, year < max(out$year)) #remove the last entry [one day only]
    if(i == 1) {
      subdata <- subset(subdata, year >= RunYears) #Remove the transient dyn
    }
    subdata[[bifname]] <- ParVal[i]
    Timedata <- rbind(Timedata, subdata)
    cat("Value of bifpar = ", ParVal[i], ". Duration:", time.taken, units(time.taken), "\n")
    times <- seq(0, 210 * RunYears)
  }
  AllData <- list()
  AllData$Timedynamics <- Timedata
  pars[bifname] <- NaN
  AllData$Parameters <- pars #assign parameters
  AllData$BifPar <- bifname
  AllData$ModelName <- as.character(substitute(modelname))
  assign(dataname, AllData, envir = .GlobalEnv) ##save data globally
  end_time_bif <- Sys.time()
  time.taken <- end_time_bif - start_time_bif
  pars[bifname] <- default_par #but in function so not necessary!
  cat("Total bif duration was ", time.taken, units(time.taken), "\n")
}
###Save RDS with asking####
saveRDSAsk <- function(object, file = ""){
  write = 'y'
  if (file.exists(file)){
    message = paste('The file', file, 'already exist. Do you want to overwrite [y/n]? ')
    write <- readline(prompt = message)
  }
  if(write=='y'){
    saveRDS(object,file)
  } else{
    print("The object has not been saved!")
  }
}

