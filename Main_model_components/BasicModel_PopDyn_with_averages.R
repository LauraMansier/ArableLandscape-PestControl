


#clear global environment
rm(list=ls()) #removes all variables


#set working directory
setwd("~/GitHub/Hover_aphid_model/Analysis_basic_model/Analysis/Switches_on")

####load all data frames####
data_frames_all_on=subset(readRDS("data_frame_all_on_all_years.RData"), timestep>4200)


year21=seq(210*20+1, 210*21, 1)
year22=seq(210*21+1, 210*22, 1)
year23=seq(210*22+1, 210*23, 1)
year24=seq(210*23+1, 210*24, 1)

#####all on#####



#year21
data_frame_all_on_year21=subset(data_frames_all_on, timestep==year21)

mean_data_frame_all_on_N1_year21=mean(data_frame_all_on_year21$N1)
mean_data_frame_all_on_N2_year21=mean(data_frame_all_on_year21$N2)
mean_data_frame_all_on_N3_year21=mean(data_frame_all_on_year21$N3)

mean_data_frame_all_on_P_year21=mean(data_frame_all_on_year21$P1)+mean(data_frame_all_on_year21$P2)+
  mean(data_frame_all_on_year21$P3)+mean(data_frame_all_on_year21$PD)+mean(data_frame_all_on_year21$P0)

#year22
data_frame_all_on_year22=subset(data_frames_all_on, timestep==year22)

mean_data_frame_all_on_N1_year22=mean(data_frame_all_on_year22$N1)
mean_data_frame_all_on_N2_year22=mean(data_frame_all_on_year22$N2)
mean_data_frame_all_on_N3_year22=mean(data_frame_all_on_year22$N3)

mean_data_frame_all_on_P_year22=mean(data_frame_all_on_year22$P1)+mean(data_frame_all_on_year22$P2)+
  mean(data_frame_all_on_year22$P3)+mean(data_frame_all_on_year22$PD)+mean(data_frame_all_on_year22$P0)

#year23
data_frame_all_on_year23=subset(data_frames_all_on, timestep==year23)

mean_data_frame_all_on_N1_year23=mean(data_frame_all_on_year23$N1)
mean_data_frame_all_on_N2_year23=mean(data_frame_all_on_year23$N2)
mean_data_frame_all_on_N3_year23=mean(data_frame_all_on_year23$N3)

mean_data_frame_all_on_P_year23=mean(data_frame_all_on_year23$P1)+mean(data_frame_all_on_year23$P2)+
  mean(data_frame_all_on_year23$P3)+mean(data_frame_all_on_year23$PD)+mean(data_frame_all_on_year23$P0)

#year24
data_frame_all_on_year24=subset(data_frames_all_on, timestep==year24)

mean_data_frame_all_on_N1_year24=mean(data_frame_all_on_year24$N1)
mean_data_frame_all_on_N2_year24=mean(data_frame_all_on_year24$N2)
mean_data_frame_all_on_N3_year24=mean(data_frame_all_on_year24$N3)

mean_data_frame_all_on_P_year24=mean(data_frame_all_on_year24$P1)+mean(data_frame_all_on_year24$P2)+
  mean(data_frame_all_on_year24$P3)+mean(data_frame_all_on_year24$PD)+mean(data_frame_all_on_year24$P0)




#points per year

all_on_N2=c(
  # mean_data_frame_all_on_N2_year15,
  # mean_data_frame_all_on_N2_year16,mean_data_frame_all_on_N2_year17,mean_data_frame_all_on_N2_year18,
  # mean_data_frame_all_on_N2_year19,mean_data_frame_all_on_N2_year20,
  mean_data_frame_all_on_N2_year21,
  mean_data_frame_all_on_N2_year22,mean_data_frame_all_on_N2_year23,mean_data_frame_all_on_N2_year24)



all_on_N1=c(
  # mean_data_frame_all_on_N1_year15,
  #           mean_data_frame_all_on_N1_year16,mean_data_frame_all_on_N1_year17,mean_data_frame_all_on_N1_year18,
  #           mean_data_frame_all_on_N1_year19,mean_data_frame_all_on_N1_year20,
  mean_data_frame_all_on_N1_year21,
            mean_data_frame_all_on_N1_year22,mean_data_frame_all_on_N1_year23,mean_data_frame_all_on_N1_year24)

all_on_N3=c(
  # mean_data_frame_all_on_N3_year15,
  #           mean_data_frame_all_on_N3_year16,mean_data_frame_all_on_N3_year17,mean_data_frame_all_on_N3_year18,
  #           mean_data_frame_all_on_N3_year19,mean_data_frame_all_on_N3_year20,
  mean_data_frame_all_on_N3_year21,
            mean_data_frame_all_on_N3_year22,mean_data_frame_all_on_N3_year23,mean_data_frame_all_on_N3_year24)

all_on_P=c(
  # mean_data_frame_all_on_P_year15,
  #          mean_data_frame_all_on_P_year16,mean_data_frame_all_on_P_year17,mean_data_frame_all_on_P_year18,
  #          mean_data_frame_all_on_P_year19,mean_data_frame_all_on_P_year20,
  mean_data_frame_all_on_P_year21,
           mean_data_frame_all_on_P_year22,mean_data_frame_all_on_P_year23,mean_data_frame_all_on_P_year24)


all_on_N=all_on_N1+all_on_N2+all_on_N3

points_x=c(105, 315, 525, 735)

points_x_max=c(-10,105, 315, 525, 735,840)
max_N=c(max(all_on_N), max(all_on_N), max(all_on_N), max(all_on_N), max(all_on_N), max(all_on_N))
max_P=c(max(all_on_P), max(all_on_P), max(all_on_P), max(all_on_P), max(all_on_P), max(all_on_P))

setwd("~/GitHub/Hover_aphid_model/Figures")



data_frame_N=data_frames_all_on$N1+data_frames_all_on$N2+data_frames_all_on$N3
data_frame_P=data_frames_all_on$P1+data_frames_all_on$P2+data_frames_all_on$P3+data_frames_all_on$PD

timeperiod=seq(1,840, 1)

#N AND P FROM ALL HABITATS TOGETHER
pdf("BasicModel_PopDyn_allhabitats_averages.pdf", width = 12, height = 8)
par(mgp=c(4,1.5, 0), mar=c(7, 12, 4.3, 4.3))
plot(timeperiod, data_frame_N, type="l", lwd=10, cex.axis = 2.5, cex.lab = 4, cex.main=2, 
     col="#33a02c", family="Times", xlab="Time (days)", yaxt='n',
     ylab="", log="y", ylim=c(0.01,1000))
lines(timeperiod, data_frame_P, type="l", lwd=10, cex.axis = 2, cex.lab = 2, col="#e31a1c")
lines(timeperiod, data_frames_all_on$P0,type="l", lwd=10, cex.axis = 2, cex.lab = 2, col="#6a3d9a")
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000), labels=c(0.01,'' , 1, '', '', 1000), cex.axis=2.5)
abline(v=c(210, 420, 630, 840), col="black", lwd=2)


lines(points_x_max, max_N, lwd=5, col="black")
lines(points_x_max, max_P, lwd=5, col="black")
#Create points of averages first creatig large black dot and then smaller coloured dot
points(points_x, all_on_N, cex=3, col="black", pch=16)
points(points_x, all_on_P, cex=3, col="black", pch=16)
points(points_x, all_on_N, cex=2, col="#33a02c", pch=16)
points(points_x, all_on_P, cex=2, col="#e31a1c", pch=16)

par(family="Times")

mtext(expression(paste("Population density ")), side=2, line=8, cex=4)
mtext(expression(paste((number/m^{2}))), side=2, line=4, cex=4)

legend("topright",legend=c("Aphids", 
                           "Adult hoverflies", 
                           "Hibernating hoverflies"), cex=1.5, lty=1, lwd=10, 
       col=c('#33a02c','#e31a1c','#6a3d9a'), bg="white")
dev.off()













