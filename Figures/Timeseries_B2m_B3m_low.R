###Settings and libraries####
rm(list=ls()) #removes all variables
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ##Set path to current dir
library(tidyverse) #Lib for data analysis

B2m_annual_forward = readRDS("Data/B2m_annual_fw.RDS")
B3m_annual_forward = readRDS("Data/B3m_annual_fw.RDS")


# to save the figures in a specific map
setwd("~/GitHub/Hover_aphid_model/Analysis_basic_model/Analysis/SNH_management/Figures")


B2m_15=subset(B2m_annual_forward$Timedynamics, round(B2m,2)==0.15)
B2m_15_year1=subset(B2m_15, year==18)

B2m_50=subset(B2m_annual_forward$Timedynamics, round(B2m,2)==0.5)
B2m_50_year1=subset(B2m_50, year==18)

B3m_15=subset(B3m_annual_forward$Timedynamics, round(B3m,2)==0.15)
B3m_15_year1=subset(B3m_15, year==18)

B3m_50=subset(B3m_annual_forward$Timedynamics, round(B3m,2)==0.5)
B3m_50_year1=subset(B3m_50, year==18)


pdf("Timeseries_Shrubs_B2m_low.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(B2m_15_year1$day, B2m_15_year1$Aphids_wood,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab=expression(paste("Density " (number/m^{2}))), log="y", ylim=c(0.01,10000))
lines(B2m_50_year1$day, B2m_50_year1$Aphids_wood, lwd=10, col="#33a02c", lty="21")

lines(B2m_15_year1$day, B2m_15_year1$Adult_wood, lwd=10, col="#e31a1c")
lines(B2m_50_year1$day, B2m_50_year1$Adult_wood, lwd=10, col="#e31a1c", lty="21")

lines(B2m_15_year1$day, B2m_15_year1$Adult_hib, lwd=10, col="#6a3d9a")
lines(B2m_50_year1$day, B2m_50_year1$Adult_hib, lwd=10, col="#6a3d9a", lty="21")

axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")
legend("topright",legend=c(expression("Aphids"), 
                           expression("Adult hoverflies"),
                           expression("Hibernating hoverflies")), cex=1.75, lty=1, lwd=10, 
       col=c('#33a02c','#e31a1c','#6a3d9a'), bg="white")

dev.off()

pdf("Timeseries_Early_B2m_low.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(B2m_15_year1$day, B2m_15_year1$Aphids_early,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(B2m_50_year1$day, B2m_50_year1$Aphids_early, lwd=10, col="#33a02c", lty="21")

lines(B2m_15_year1$day, B2m_15_year1$Adult_early, lwd=10, col="#e31a1c")
lines(B2m_50_year1$day, B2m_50_year1$Adult_early, lwd=10, col="#e31a1c", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()


pdf("Timeseries_late_B2m_low.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(B2m_15_year1$day, B2m_15_year1$Aphids_late,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(B2m_50_year1$day, B2m_50_year1$Aphids_late, lwd=10, col="#33a02c", lty="21")

lines(B2m_15_year1$day, B2m_15_year1$Adult_late, lwd=10, col="#e31a1c")
lines(B2m_50_year1$day, B2m_50_year1$Adult_late, lwd=10, col="#e31a1c", lty="21")

axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()








pdf("Timeseries_Shrubs_B3m_low.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(B3m_15_year1$day, B3m_15_year1$Aphids_wood,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab=expression(paste("Density " (number/m^{2}))), log="y", ylim=c(0.01,10000))
lines(B3m_50_year1$day, B3m_50_year1$Aphids_wood, lwd=10, col="#33a02c", lty="21")

lines(B3m_15_year1$day, B3m_15_year1$Adult_wood, lwd=10, col="#e31a1c")
lines(B3m_50_year1$day, B3m_50_year1$Adult_wood, lwd=10, col="#e31a1c", lty="21")

lines(B3m_15_year1$day, B3m_15_year1$Adult_hib, lwd=10, col="#6a3d9a")
lines(B3m_50_year1$day, B3m_50_year1$Adult_hib, lwd=10, col="#6a3d9a", lty="21")

axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")
# legend("topright",legend=c(expression("Aphids"), 
#                            expression("Adult hoverflies"),
#                            expression("Hibernating hoverflies")), cex=1.75, lty=1, lwd=10, 
#        col=c('#33a02c','#e31a1c','#6a3d9a'), bg="white")

dev.off()

pdf("Timeseries_Early_B3m_low.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(B3m_15_year1$day, B3m_15_year1$Aphids_early,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(B3m_50_year1$day, B3m_50_year1$Aphids_early, lwd=10, col="#33a02c", lty="21")

lines(B3m_15_year1$day, B3m_15_year1$Adult_early, lwd=10, col="#e31a1c")
lines(B3m_50_year1$day, B3m_50_year1$Adult_early, lwd=10, col="#e31a1c", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()


pdf("Timeseries_late_B3m_low.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(B3m_15_year1$day, B3m_15_year1$Aphids_late,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(B3m_50_year1$day, B3m_50_year1$Aphids_late, lwd=10, col="#33a02c", lty="21")

lines(B3m_15_year1$day, B3m_15_year1$Adult_late, lwd=10, col="#e31a1c")
lines(B3m_50_year1$day, B3m_50_year1$Adult_late, lwd=10, col="#e31a1c", lty="21")

axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()