#Run model#
###Settings and libraries####
rm(list=ls()) #removes all variables
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ##Set path to current dir
library(deSolve) #Lib for ODE solver
library(tidyverse) #Lib for data analysis
source("Pars_model.R")
source("Functions_Model.R")


###Run a bif over woody habitat flower ####
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "Flowers_wood_forward",
       bifname = "B1m",
       pars = parms,
       minbif = 0, maxbif = 0.6, stepsize = 0.025,
       RunYears = 20)


saveRDSAsk(Flowers_wood_hibwood_forward, "Data/Flowers_wood_fw.RDS") #save way to safe data


###Run a bif over woody habitat aphid spring ####
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "Aphids_wood_spring_forward",
       bifname = "n1s",
       pars = parms,
       minbif = 0, maxbif = 2, stepsize = 0.115, 
       RunYears = 20)

saveRDSAsk(Aphids_wood_spring_hibwood_forward, "Data/Aphids_wood_spring_fw.RDS") #save way to safe data





###Run a bif over woody habitat aphid autumn####
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "Aphids_wood_autumn_forward",
       bifname = "n1a",
       pars = parms,
       minbif = 0, maxbif = 1.5, stepsize = 0.08625, 
       RunYears = 20)

saveRDSAsk(Aphids_wood_autumn_hibwood_forward, "Data/Aphids_wood_autumn_fw.RDS") #save way to safe data



###Run a bif over T2s annual####
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "T2s_annual_forward",
       bifname = "T2s",
       pars = parms,
       minbif = 30, maxbif = 100, stepsize = 5,
       RunYears = 20)

saveRDSAsk(T2s_annual_forward, "Data/T2s_annual_fw.RDS") #save way to safe data




###Run a bif over T2s perennial####

parms["B2m"] = 0.26 #parameters different from default, important to reset at end!
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "T2s_perennial_forward",
       bifname = "T2s",
       pars = parms,
       minbif = 30, maxbif = 100, stepsize = 5,
       RunYears = 20)

saveRDSAsk(T2s_perennial_forward, "Data/T2s_perennial_fw.RDS") #save way to safe data
parms["B2m"] = B2m



###Run a bif over T3s annual####
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "T3s_annual_forward",
       bifname = "T3s",
       pars = parms,
       minbif = 30, maxbif = 100, stepsize = 5,
       RunYears = 20)

saveRDSAsk(T3s_annual_forward, "Data/T3s_annual_fw.RDS") #save way to safe data




###Run a bif over T3s perennial####

parms["B3m"] = 0.26 #parameters different from default, important to reset at end!
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "T3s_perennial_forward",
       bifname = "T3s",
       pars = parms,
       minbif = 30, maxbif = 100, stepsize = 5,
       RunYears = 20)

saveRDSAsk(T3s_perennial_forward, "Data/T3s_perennial_fw.RDS") #save way to safe data
parms["B3m"] = B3m




###Run a bif over B2m annual####
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "B2m_annual_forward",
       bifname = "B2m",
       pars = parms,
       minbif = 0, maxbif = 0.6, stepsize = 0.025,
       RunYears = 20)

saveRDSAsk(B2m_annual_forward, "Data/B2m_annual_fw_extra.RDS") #save way to safe data



###Run a bif over B2m perennial####

parms["T2s"] = 40 #parameters different from default, important to reset at end!
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "B2m_perennial_forward",
       bifname = "B2m",
       pars = parms,
       minbif = 0, maxbif = 0.6, stepsize = 0.025,
       RunYears = 20)

saveRDSAsk(B2m_perennial_forward, "Data/B2m_perennial_fw_extra.RDS") #save way to safe data
parms["T2s"] = T2s



###Run a bif over B3m annual####
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "B3m_annual_forward",
       bifname = "B3m",
       pars = parms,
       minbif = 0, maxbif = 0.6, stepsize = 0.025,
       RunYears = 20)

saveRDSAsk(B3m_annual_forward, "Data/B3m_annual_fw_extra.RDS") #save way to safe data



###Run a bif over B3m perennial####

parms["T2s"] = 40 #parameters different from default, important to reset at end!
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "B3m_perennial_forward",
       bifname = "B3m",
       pars = parms,
       minbif = 0, maxbif = 0.6, stepsize = 0.025,
       RunYears = 20)

saveRDSAsk(B3m_perennial_forward, "Data/B3m_perennial_fw_extra.RDS") #save way to safe data
parms["T3s"] = T3s





###Do a bif over mowing early crop fm [without mortality due to mowing]#####
parms["FM2"] = 1 #Turn mowing flowers early ON, important to reset at end!
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "Mowing_early_nomort_forward",
       bifname = "M2s",
       pars = parms,
       minbif = 0, maxbif = 210, stepsize = 10,
       RunYears = 20)
parms["FM2"] = 0 #reset

saveRDSAsk(Mowing_early_nomort_forward, file = "Data/Mowing_early_nomort_fw.RDS")




###Do a bif over mowing early crop fm [WITH mortality due to mowing!]#####
parms["FM2"] = 1 #Turn mowing flowers early ON, important to reset at end!
parms["I2"] = 1
parms["sN"] = 1
parms["sP"] = 0.05
parms["Is2"] = M2s
BifRun(modelname = lvpred_ref_hibwood_mowingmort,
       dataname = "Mowing_early_mort_forward",
       bifname = "M2s",
       pars = parms,
       minbif = 0, maxbif = 210, stepsize = 10,
       RunYears = 20)
parms["FM2"] = 0 #reset
parms["I2"] = 0
parms["sN"] = sN
parms["sP"] = sP
parms["Is2"] = Is2

saveRDSAsk(Mowing_early_mort_forward, file = "Data/Mowing_early_mort_fw.RDS")




###Do a bif over mowing late crop fm [without mortality due to mowing] using improved R0 model#####
parms["FM3"] = 1 #Turn mowing flowers early ON, important to reset at end!
BifRun(modelname = lvpred_ref_hibwood,
       dataname = "Mowing_late_nomort_forward",
       bifname = "M3s",
       pars = parms,
       minbif = 0, maxbif = 210, stepsize = 10,
       RunYears = 20)
parms["FM3"] = 0 #reset

saveRDSAsk(Mowing_late_nomort_forward, file = "Data/Mowing_late_nomort_fw.RDS")






###Do a bif over mowing late crop fm [WITH mortality due to mowing!] using improved R0 model#####
parms["FM3"] = 1 #Turn mowing flowers early ON, important to reset at end!
parms["I3"] = 1
parms["sN"] = 1
parms["sP"] = 0.05
parms["Is3"] = M3s
BifRun(modelname = lvpred_ref_hibwood_mowingmort,
       dataname = "Mowing_late_mort_forward",
       bifname = "M3s",
       pars = parms,
       minbif = 0, maxbif = 210, stepsize = 10,
       RunYears = 20)
parms["FM3"] = 0 #reset
parms["I3"] = 0
parms["sN"] = sN
parms["sP"] = sP
parms["Is3"] = Is3

saveRDSAsk(Mowing_late_mort_forward, file = "Data/Mowing_late_mort_fw.RDS")



