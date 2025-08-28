###Settings and libraries####
rm(list=ls()) #removes all variables
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ##Set path to current dir
library(tidyverse) #Lib for data analysis


n1s_forward=readRDS("Data/Aphids_wood_spring_fw.RDS")
n1a_forward=readRDS("Data/Aphids_wood_autumn_fw.RDS")


n1s_0230=subset(n1s_forward$Timedynamics, n1s==0.230)
n1s_0230_year1=subset(n1s_0230, year==18)
n1s_0230_year2=subset(n1s_0230, year==19)


n1s_115=subset(n1s_forward$Timedynamics, round(n1s,2)==1.15) #round is used to get the correct decimals for n1s
n1s_115_year1=subset(n1s_115, year==18)
n1s_115_year2=subset(n1s_115, year==19)

n1s_0805=subset(n1s_forward$Timedynamics, round(n1s,5)==0.805) #round is used to get the correct decimals for n1s
n1s_0805_year1=subset(n1s_0805, year==18)
n1s_0805_year2=subset(n1s_0805, year==19)

n1a_0=subset(n1a_forward$Timedynamics, round(n1a,5)==0)
n1a_0_year1=subset(n1a_0, year==20)
n1a_0_year2=subset(n1a_0, year==21)

# to save the figures in a specific map
setwd("~/GitHub/Hover_aphid_model/Analysis_basic_model/Analysis/SNH_management/Figures")


points_x=seq(1,420,1)
points_labels=seq(0,420,30)
labels=c(seq(0, 210, 30), seq(30, 210, 30))


#n1a_0
# n1a_0$time=n1a_0$time-3780


pdf("Timeseries_Shrubs_n1a_0.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1a_0_year1$day, n1a_0_year1$Aphids_wood,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab=expression(paste("Density " (number/m^{2}))), log="y", ylim=c(0.01,10000))
lines(n1a_0_year2$day, n1a_0_year2$Aphids_wood, lwd=10, col="#33a02c", lty="21")

lines(n1a_0_year1$day, n1a_0_year1$Adult_wood, lwd=10, col="#e31a1c")
lines(n1a_0_year2$day, n1a_0_year2$Adult_wood, lwd=10, col="#e31a1c", lty="21")

lines(n1a_0_year1$day, n1a_0_year1$Adult_hib, lwd=10, col="#6a3d9a")
lines(n1a_0_year2$day, n1a_0_year2$Adult_hib, lwd=10, col="#6a3d9a", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")
# legend("topright",legend=c(expression("Aphids"), 
#                            expression("Adult hoverflies"),
#                            expression("Hibernating hoverflies")), cex=1.75, lty=1, lwd=10, 
#        col=c('#33a02c','#e31a1c','#6a3d9a'), bg="white")

dev.off()


pdf("Timeseries_Early_n1a_0.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1a_0_year1$day, n1a_0_year1$Aphids_early,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(n1a_0_year2$day, n1a_0_year2$Aphids_early, lwd=10, col="#33a02c", lty="21")

lines(n1a_0_year1$day, n1a_0_year1$Adult_early, lwd=10, col="#e31a1c")
lines(n1a_0_year2$day, n1a_0_year2$Adult_early, lwd=10, col="#e31a1c", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()


pdf("Timeseries_late_n1a_0.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1a_0_year1$day, n1a_0_year1$Aphids_late,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(n1a_0_year2$day, n1a_0_year2$Aphids_late, lwd=10, col="#33a02c", lty="21")

lines(n1a_0_year1$day, n1a_0_year1$Adult_late, lwd=10, col="#e31a1c")
lines(n1a_0_year2$day, n1a_0_year2$Adult_late, lwd=10, col="#e31a1c", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()


#n1s_0230
# n1s_0230$time=n1s_0230$time-3780


pdf("Timeseries_Shrubs_n1s_0230.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_0230_year1$day, n1s_0230_year1$Aphids_wood,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab=expression(paste("Density " (number/m^{2}))), log="y", ylim=c(0.01,10000))
lines(n1s_0230_year2$day, n1s_0230_year2$Aphids_wood, lwd=10, col="#33a02c", lty="21")

lines(n1s_0230_year1$day, n1s_0230_year1$Adult_wood, lwd=10, col="#e31a1c")
lines(n1s_0230_year2$day, n1s_0230_year2$Adult_wood, lwd=10, col="#e31a1c", lty="21")

lines(n1s_0230_year1$day, n1s_0230_year1$Adult_hib, lwd=10, col="#6a3d9a")
lines(n1s_0230_year2$day, n1s_0230_year2$Adult_hib, lwd=10, col="#6a3d9a", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")
legend("topright",legend=c(expression("Aphids"), 
                           expression("Adult hoverflies"),
                           expression("Hibernating hoverflies")), cex=1.75, lty=1, lwd=10, 
       col=c('#33a02c','#e31a1c','#6a3d9a'), bg="white")

dev.off()


pdf("Timeseries_Early_n1s_0230.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_0230_year1$day, n1s_0230_year1$Aphids_early,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(n1s_0230_year2$day, n1s_0230_year2$Aphids_early, lwd=10, col="#33a02c", lty="21")

lines(n1s_0230_year1$day, n1s_0230_year1$Adult_early, lwd=10, col="#e31a1c")
lines(n1s_0230_year2$day, n1s_0230_year2$Adult_early, lwd=10, col="#e31a1c", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()


pdf("Timeseries_late_n1s_0230.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_0230_year1$day, n1s_0230_year1$Aphids_late,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(n1s_0230_year2$day, n1s_0230_year2$Aphids_late, lwd=10, col="#33a02c", lty="21")

lines(n1s_0230_year1$day, n1s_0230_year1$Adult_late, lwd=10, col="#e31a1c")
lines(n1s_0230_year2$day, n1s_0230_year2$Adult_late, lwd=10, col="#e31a1c", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()


#0805
pdf("Timeseries_Shrubs_n1s_0805.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_0805_year1$day, n1s_0805_year1$Aphids_wood,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab=expression(paste("Density " (number/m^{2}))), log="y", ylim=c(0.01,10000))
lines(n1s_0805_year2$day, n1s_0805_year2$Aphids_wood, lwd=10, col="#33a02c", lty="21")

lines(n1s_0805_year1$day, n1s_0805_year1$Adult_wood, lwd=10, col="#e31a1c")
lines(n1s_0805_year2$day, n1s_0805_year2$Adult_wood, lwd=10, col="#e31a1c", lty="21")

lines(n1s_0805_year1$day, n1s_0805_year1$Adult_hib, lwd=10, col="#6a3d9a")
lines(n1s_0805_year2$day, n1s_0805_year2$Adult_hib, lwd=10, col="#6a3d9a", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")
legend("topright",legend=c(expression("Aphids"), 
                           expression("Adult hoverflies"),
                           expression("Hibernating hoverflies")), cex=1.75, lty=1, lwd=10, 
       col=c('#33a02c','#e31a1c','#6a3d9a'), bg="white")

dev.off()


pdf("Timeseries_Early_n1s_0805.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_0805_year1$day, n1s_0805_year1$Aphids_early,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(n1s_0805_year2$day, n1s_0805_year2$Aphids_early, lwd=10, col="#33a02c", lty="21")

lines(n1s_0805_year1$day, n1s_0805_year1$Adult_early, lwd=10, col="#e31a1c")
lines(n1s_0805_year2$day, n1s_0805_year2$Adult_early, lwd=10, col="#e31a1c", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()


pdf("Timeseries_late_n1s_0805.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_0805_year1$day, n1s_0805_year1$Aphids_late,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(n1s_0805_year2$day, n1s_0805_year2$Aphids_late, lwd=10, col="#33a02c", lty="21")

lines(n1s_0805_year1$day, n1s_0805_year1$Adult_late, lwd=10, col="#e31a1c")
lines(n1s_0805_year2$day, n1s_0805_year2$Adult_late, lwd=10, col="#e31a1c", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()

#115

pdf("Timeseries_Shrubs_n1s_115.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_115_year1$day, n1s_115_year1$Aphids_wood,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='Time (days)', axes=FALSE,
     ylab=expression(paste("Density " (number/m^{2}))), log="y", ylim=c(0.01,10000))
lines(n1s_115_year2$day, n1s_115_year2$Aphids_wood, lwd=10, col="#33a02c", lty="dotdash")

lines(n1s_115_year1$day, n1s_115_year1$Adult_wood, lwd=10, col="#e31a1c")
lines(n1s_115_year2$day, n1s_115_year2$Adult_wood, lwd=10, col="#e31a1c", lty="21")

lines(n1s_115_year1$day, n1s_115_year1$Adult_hib, lwd=10, col="#6a3d9a")
lines(n1s_115_year2$day, n1s_115_year2$Adult_hib, lwd=10, col="#6a3d9a", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")
# legend("topright",legend=c(expression("Aphids"), 
#                            expression("Adult hoverflies"),
#                            expression("Hibernating hoverflies")), cex=1.75, lty=1, lwd=10, 
#        col=c('#33a02c','#e31a1c','#6a3d9a'), bg="white")

dev.off()


pdf("Timeseries_Early_n1s_115.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_115_year1$day, n1s_115_year1$Aphids_early,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='Time (days)', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(n1s_115_year2$day, n1s_115_year2$Aphids_early, lwd=10, col="#33a02c", lty="21")

lines(n1s_115_year1$day, n1s_115_year1$Adult_early, lwd=10, col="#e31a1c")
lines(n1s_115_year2$day, n1s_115_year2$Adult_early, lwd=10, col="#e31a1c", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()


pdf("Timeseries_late_n1s_115.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_115_year1$day, n1s_115_year1$Aphids_late,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='Time (days)', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(n1s_115_year2$day, n1s_115_year2$Aphids_late, lwd=10, col="#33a02c", lty="21")

lines(n1s_115_year1$day, n1s_115_year1$Adult_late, lwd=10, col="#e31a1c")
lines(n1s_115_year2$day, n1s_115_year2$Adult_late, lwd=10, col="#e31a1c", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()







pdf("Pop_dyn_shrubs.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_115_year1$day, n1s_115_year1$Aphids_wood,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='Time (days)', axes=FALSE,
     ylab=expression(paste("Density " (number/m^{2}))), log="y", ylim=c(0.01,10000))
lines(n1s_115_year2$day, n1s_115_year2$Aphids_wood, lwd=10, col="#33a02c", lty="dotdash")

lines(n1s_115_year1$day, n1s_115_year1$Adult_wood, lwd=10, col="#e31a1c")
lines(n1s_115_year2$day, n1s_115_year2$Adult_wood, lwd=10, col="#e31a1c", lty="21")

lines(n1s_115_year1$day, n1s_115_year1$Adult_hib, lwd=10, col="#6a3d9a")
lines(n1s_115_year2$day, n1s_115_year2$Adult_hib, lwd=10, col="#6a3d9a", lty="21")


axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

abline(v=0)
abline(v=30)
abline(v=60)
abline(v=90)
abline(v=120)
abline(v=150)
abline(v=180)
abline(v=210)

text(15, 1e-02, "April", cex=2)
text(45, 1e-02, "May", cex=2)
text(75, 1e-02, "June", cex=2)
text(105, 1e-02, "July", cex=2)
text(135, 1e-02, "Aug", cex=2)
text(165, 1e-02, "Sep", cex=2)
text(195, 1e-02, "Oct", cex=2)


box(col="black")
legend("topright",legend=c(expression("Aphids"), 
                           expression("Adult hoverflies"),
                           expression("Hibernating hoverflies")), cex=1.75, lty=1, lwd=10, 
       col=c('#33a02c','#e31a1c','#6a3d9a'), bg="white")

dev.off()


pdf("Pop_dyn_early.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_115_year1$day, n1s_115_year1$Aphids_early,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='Time (days)', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(n1s_115_year2$day, n1s_115_year2$Aphids_early, lwd=10, col="#33a02c", lty="21")

lines(n1s_115_year1$day, n1s_115_year1$Adult_early, lwd=10, col="#e31a1c")
lines(n1s_115_year2$day, n1s_115_year2$Adult_early, lwd=10, col="#e31a1c", lty="21")

abline(v=0)
abline(v=30)
abline(v=60)
abline(v=90)
abline(v=120)
abline(v=150)
abline(v=180)
abline(v=210)

text(15, 1e-02, "April", cex=2)
text(45, 1e-02, "May", cex=2)
text(75, 1e-02, "June", cex=2)
text(105, 1e-02, "July", cex=2)
text(135, 1e-02, "Aug", cex=2)
text(165, 1e-02, "Sep", cex=2)
text(195, 1e-02, "Oct", cex=2)

axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()


pdf("Pop_dyn_late.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(n1s_115_year1$day, n1s_115_year1$Aphids_late,type="l", lwd=10, cex.axis = 2.5, cex.lab = 2.5, cex.main=2, 
     col="#33a02c", xlab='Time (days)', axes=FALSE,
     ylab="", log="y", ylim=c(0.01,10000))
lines(n1s_115_year2$day, n1s_115_year2$Aphids_late, lwd=10, col="#33a02c", lty="21")

lines(n1s_115_year1$day, n1s_115_year1$Adult_late, lwd=10, col="#e31a1c")
lines(n1s_115_year2$day, n1s_115_year2$Adult_late, lwd=10, col="#e31a1c", lty="21")

abline(v=0)
abline(v=30)
abline(v=60)
abline(v=90)
abline(v=120)
abline(v=150)
abline(v=180)
abline(v=210)

text(15, 1e-02, "April", cex=2)
text(45, 1e-02, "May", cex=2)
text(75, 1e-02, "June", cex=2)
text(105, 1e-02, "July", cex=2)
text(135, 1e-02, "Aug", cex=2)
text(165, 1e-02, "Sep", cex=2)
text(195, 1e-02, "Oct", cex=2)

axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000, 10000), labels=c(0.01,0.1, 1,10, 100, 1000, 10000), cex.axis=2)

box(col="black")

dev.off()

