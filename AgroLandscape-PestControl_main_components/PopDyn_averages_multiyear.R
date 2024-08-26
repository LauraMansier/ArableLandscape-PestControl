#clear global environment
#rm(list=ls()) #removes all variables

#Set working directory to the directory where you saved the model output and where the graphs will be saved
setwd("")

####Load data frame####
data_frames=readRDS("AgroLandscape-PestControl.RData")
#Take a subset of mutliple years
data_frame_PopDyn_subset=subset(data_frames, timestep>4200)

#Define years
year21=seq(210*20+1, 210*21, 1)
year22=seq(210*21+1, 210*22, 1)
year23=seq(210*22+1, 210*23, 1)
year24=seq(210*23+1, 210*24, 1)

#Make a subset for every population for every year
#year21
data_frame_year21=subset(data_frame_PopDyn_subset, timestep==year21)

mean_data_frame_N1_year21=mean(data_frame_all_on_year21$N1)
mean_data_frame_N2_year21=mean(data_frame_all_on_year21$N2)
mean_data_frame_N3_year21=mean(data_frame_all_on_year21$N3)

mean_data_frame_P_year21=mean(data_frame_year21$P1)+mean(data_frame_year21$P2)+
  mean(data_frame_year21$P3)+mean(data_frame_year21$PD)+mean(data_frame_year21$P0)

#year22
data_frame_year22=subset(data_frame_PopDyn_subset, timestep==year22)

mean_data_frame_N1_year22=mean(data_frame_year22$N1)
mean_data_frame_N2_year22=mean(data_frame_year22$N2)
mean_data_frame_N3_year22=mean(data_frame_year22$N3)

mean_data_frame_P_year22=mean(data_frame_year22$P1)+mean(data_frame_year22$P2)+
  mean(data_frame_year22$P3)+mean(data_frame_year22$PD)+mean(data_frame_year22$P0)

#year23
data_frame_year23=subset(data_frame_PopDyn_subset, timestep==year23)

mean_data_frame_N1_year23=mean(data_frame_year23$N1)
mean_data_frame_N2_year23=mean(data_frame_year23$N2)
mean_data_frame_N3_year23=mean(data_frame_year23$N3)

mean_data_frame_P_year23=mean(data_frame_year23$P1)+mean(data_frame_year23$P2)+
  mean(data_frame_year23$P3)+mean(data_frame_year23$PD)+mean(data_frame_year23$P0)

#year24
data_frame_year24=subset(data_frame_PopDyn_subset, timestep==year24)

mean_data_frame_N1_year24=mean(data_frame_year24$N1)
mean_data_frame_N2_year24=mean(data_frame_year24$N2)
mean_data_frame_N3_year24=mean(data_frame_year24$N3)

mean_data_frame_P_year24=mean(data_frame_year24$P1)+mean(data_frame_year24$P2)+
  mean(data_frame_year24$P3)+mean(data_frame_year24$PD)+mean(data_frame_year24$P0)




#points per year
N1=c(mean_data_frame_N1_year21,mean_data_frame_N1_year22,mean_data_frame_N1_year23,mean_data_frame_N1_year24)

N2=c(mean_data_frame_N2_year21,mean_data_frame_N2_year22,mean_data_frame_N2_year23,mean_data_frame_N2_year24)

N3=c(mean_data_frame_N3_year21,mean_data_frame_N3_year22,mean_data_frame_N3_year23,mean_data_frame_N3_year24)

P=c(mean_data_frame_P_year21, mean_data_frame_P_year22,mean_data_frame_P_year23,mean_data_frame_P_year24)

N=N1+N2+N3

points_x=c(105, 315, 525, 735) #x-axis coordinates of the middle of each year

#Finding the maximum densities for aphids (combined) and adult hoverflies (repetition to draw a line)
points_x_max=c(-10,105, 315, 525, 735,840)
max_N=c(max(N), max(N), max(N), max(N), max(N), max(N))
max_P=c(max(P), max(P), max(P), max(P), max(P), max(P))

#Combine populations per habitat into one group for aphids and hoverflies
data_frame_N=data_frames$N1+data_frames$N2+data_frames$N3
data_frame_P=data_frames$P1+data_frames$P2+data_frames$P3+data_frames$PD

timeperiod=seq(1,840, 1) # 4 year time period

#N AND P FROM ALL HABITATS TOGETHER
pdf("BasicModel_PopDyn_allhabitats_averages.pdf", width = 12, height = 8)
par(mgp=c(4,1.5, 0), mar=c(7, 12, 4.3, 4.3))
plot(timeperiod, data_frame_N, type="l", lwd=10, cex.axis = 2.5, cex.lab = 4, cex.main=2, 
     col="#33a02c", family="Times", xlab="Time (days)", yaxt='n',
     ylab="", log="y", ylim=c(0.01,1000))
lines(timeperiod, data_frame_P, type="l", lwd=10, cex.axis = 2, cex.lab = 2, col="#e31a1c")
lines(timeperiod, data_frames$P0,type="l", lwd=10, cex.axis = 2, cex.lab = 2, col="#6a3d9a")
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000), labels=c(0.01,'' , 1, '', '', 1000), cex.axis=2.5)
abline(v=c(210, 420, 630, 840), col="black", lwd=2)


lines(points_x_max, max_N, lwd=5, col="black")
lines(points_x_max, max_P, lwd=5, col="black")
points(points_x, N, cex=3, col="black", pch=16)
points(points_x, P, cex=3, col="black", pch=16)
points(points_x, N, cex=2, col="#33a02c", pch=16)
points(points_x, P, cex=2, col="#e31a1c", pch=16)

par(family="Times")

mtext(expression(paste("Population density ")), side=2, line=8, cex=4)
mtext(expression(paste((number/m^{2}))), side=2, line=4, cex=4)

legend("topright",legend=c("Aphids", 
                           "Adult hoverflies", 
                           "Hibernating hoverflies"), cex=1.5, lty=1, lwd=10, 
       col=c('#33a02c','#e31a1c','#6a3d9a'), bg="white")
dev.off()













