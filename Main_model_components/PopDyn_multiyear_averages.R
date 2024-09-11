


#clear global environment
rm(list=ls()) #removes all variables

setwd("") #set the working directory to where you have saved the model output

#Load your data frame and taking the last 4 years of that data frame
data_frame=subset(readRDS("ArableLandscape-PestControl.RData"), timestep>4200)

#Setting up years (make sure you have run the model for 24 years, if you have run the model for a shorter period you will need to adapt here)
year21=seq(210*20+1, 210*21, 1)
year22=seq(210*21+1, 210*22, 1)
year23=seq(210*22+1, 210*23, 1)
year24=seq(210*23+1, 210*24, 1)

#Year21
data_frame_year21=subset(data_frame, timestep==year21)
#Calculating the mean for every population in year 21
mean_data_frame_N1_year21=mean(data_frame_year21$N1)
mean_data_frame_N2_year21=mean(data_frame_year21$N2)
mean_data_frame_N3_year21=mean(data_frame_year21$N3)

mean_data_frame_P_year21=mean(data_frame_year21$P1)+mean(data_frame_year21$P2)+
  mean(data_frame_year21$P3)+mean(data_frame_year21$PD)+mean(data_frame_year21$P0)

#Year22
data_frame_year22=subset(data_frame, timestep==year22)
#Calculating the mean for every population in year 22
mean_data_frame_N1_year22=mean(data_frame_year22$N1)
mean_data_frame_N2_year22=mean(data_frame_year22$N2)
mean_data_frame_N3_year22=mean(data_frame_year22$N3)

mean_data_frame_P_year22=mean(data_frame_year22$P1)+mean(data_frame_year22$P2)+
  mean(data_frame_year22$P3)+mean(data_frame_year22$PD)+mean(data_frame_year22$P0)

#Year23
data_frame_year23=subset(data_frame, timestep==year23)
#Calculating the mean for every population in year 23
mean_data_frame_N1_year23=mean(data_frame_year23$N1)
mean_data_frame_N2_year23=mean(data_frame_year23$N2)
mean_data_frame_N3_year23=mean(data_frame_year23$N3)

mean_data_frame_P_year23=mean(data_frame_year23$P1)+mean(data_frame_year23$P2)+
  mean(data_frame_year23$P3)+mean(data_frame_year23$PD)+mean(data_frame_year23$P0)

#Year24
data_frame_year24=subset(data_frame, timestep==year24)
#Calculating the mean for every population in year 24
mean_data_frame_N1_year24=mean(data_frame_year24$N1)
mean_data_frame_N2_year24=mean(data_frame_year24$N2)
mean_data_frame_N3_year24=mean(data_frame_year24$N3)

mean_data_frame_P_year24=mean(data_frame_year24$P1)+mean(data_frame_year24$P2)+
  mean(data_frame_year24$P3)+mean(data_frame_year24$PD)+mean(data_frame_year24$P0)




#Combine all means per population per year in an array

N1=c(mean_data_frame_N1_year21, mean_data_frame_N1_year22,mean_data_frame_N1_year23,mean_data_frame_N1_year24)

N2=c(mean_data_frame_N2_year21, mean_data_frame_N2_year22,mean_data_frame_N2_year23,mean_data_frame_N2_year24)

N3=c(mean_data_frame_N3_year21, mean_data_frame_N3_year22,mean_data_frame_N3_year23,mean_data_frame_N3_year24)

P=c(mean_data_frame_P_year21, mean_data_frame_P_year22,mean_data_frame_P_year23,mean_data_frame_P_year24)

#Combine aphid populations
N=N1+N2+N3

#Create points in the middle of a year (210 days) for plotting
points_x=c(105, 315, 525, 735)

#Calculate the maximum densities of the populations
points_x_max=c(-10, 105, 315, 525, 735,840)
max_N=c(max(N), max(N), max(N), max(N), max(N), max(N))
max_P=c(max(P), max(P), max(P), max(P), max(P), max(P))

data_frame_N=data_frame$N1+data_frame$N2+data_frame$N3
data_frame_P=data_frame$P1+data_frame$P2+data_frame$P3+data_frame$PD

#Set the time
timeperiod=seq(1,840, 1)

#PLOTTING
#N AND P FROM ALL HABITATS TOGETHER
pdf("PopDyn_allhabitats_averages.pdf", width = 12, height = 8)
par(mgp=c(4,1.5, 0), mar=c(7, 12, 4.3, 4.3))
plot(timeperiod, data_frame_N, type="l", lwd=10, cex.axis = 2.5, cex.lab = 4, cex.main=2, 
     col="#33a02c", family="Times", xlab="Time (days)", yaxt='n',
     ylab="", log="y", ylim=c(0.01,1000))
lines(timeperiod, data_frame_P, type="l", lwd=10, cex.axis = 2, cex.lab = 2, col="#e31a1c")
lines(timeperiod, data_frame$P0,type="l", lwd=10, cex.axis = 2, cex.lab = 2, col="#6a3d9a")

axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000), labels=c(0.01,'' , 1, '', '', 1000), cex.axis=2.5)
abline(v=c(210, 420, 630, 840), col="black", lwd=2)


lines(points_x_max, max_N, lwd=5, col="black")
lines(points_x_max, max_P, lwd=5, col="black")
#Create points of averages first creatig large black dot and then smaller coloured dot (aestatic)
points(points_x, N, cex=3, col="black", pch=16)
points(points_x, P, cex=3, col="black", pch=16)
points(points_x, N, cex=2, col="#33a02c", pch=16)
points(points_x, P, cex=2, col="#e31a1c", pch=16)

par(family="Times")
#Give labels to the y-axis
mtext(expression(paste("Population density ")), side=2, line=8, cex=4)
mtext(expression(paste((number/m^{2}))), side=2, line=4, cex=4)

#Create the legend
legend("topright",legend=c("Aphids", 
                           "Adult hoverflies", 
                           "Hibernating hoverflies"), cex=1.5, lty=1, lwd=10, 
       col=c('#33a02c','#e31a1c','#6a3d9a'), bg="white")
dev.off()













