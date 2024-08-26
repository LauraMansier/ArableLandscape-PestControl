#Set working directory to the directory where you saved the model output and where the graphs will be saved
setwd("")

#Load the data frame created with the script AgroLandscape-PestControl script
data_frames=readRDS("AgroLandscape-PestControl.RData")
#Take a subset of one year
data_frame_PopDyn_subset=subset(data_frames, timestep<210)

####Population dynamics in Shrubs over 1 year####
pdf("PopDyn_Shrubs_1year.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #shrubs
plot(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$N1,type="l", lwd=10, cex.axis = 2.5, cex.lab = 3, cex.main=2, 
     col="#33a02c", family="Times", xlab='Time (days)', axes=FALSE,
     ylab=expression(paste("Population density " (number/m^{2}))), log="y", ylim=c(0.01,1000))
par(family="Times")
axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000), labels=c(0.01,0.1, 1,10, 100, 1000), cex.axis=2.3)

box(col="black")
lines(data_frame_PopDyn_subset$timestep,data_frame_PopDyn_subset$p1,type="l", lwd=10, col="#fb9a99")
lines(data_frame_PopDyn_subset$timestep,data_frame_PopDyn_subset$P1,type="l", lwd=10, col="#e31a1c")
lines(data_frame_PopDyn_subset$timestep,data_frame_PopDyn_subset$P0,type="l", lwd=10, col="#6a3d9a")
par(family="Times")
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
legend("topright",legend=c(expression("Aphids"), 
                           expression("Adult hoverflies"), 
                           expression("Juvenile hoverflies"),
                           expression("Hibernating hoverflies")), cex=1.75, lty=1, lwd=10, 
       col=c('#33a02c','#e31a1c','#fb9a99','#6a3d9a'), bg="white")
dev.off()

####Population dynamics in of the dispersing adult hoverfly population over 1 year####
#  pdf("PopDyn_Dispersing.pdf", width = 15)
# par(mar=c(5.1,5.5,4.1,2.1))
# #Dispersing
# plot(data_frame_PopDyn_subset$timestep,data_frame_PopDyn_subset$PD,type="l", lwd=2, cex.axis = 2, cex.lab = 2, cex.main=2, 
#      color="#1f78b4", family="Times", col="#1f78b4", main="Dispersing predators", xlab="Time (days)", 
#      ylab=expression(paste("Population density " (number/m^{2}))), log="y", ylim=c(0.0001,1000))
# abline(v=c(210, 420, 630, 840), col="black", lwd=2)
#  dev.off()
# 

####Population dynamics in the early crop habitat over 1 year####
pdf("PopDyn_EarlyCrop_1year.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #early crop
plot(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$N2,type="l", lwd=10, cex.axis = 2.5, cex.lab = 3, cex.main=2,
     col='#33a02c', family="Times", axes=FALSE,
     ylab=expression(paste("Population density " (number/m^{2}))),xlab='Time (days)',
     log="y", ylim=c(0.01,1000))
lines(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$p2, type="l", lwd=10, cex.axis = 2, cex.lab = 2, col='#fb9a99')
lines(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$P2, type="l", lwd=10, cex.axis = 2, cex.lab = 2, col='#e31a1c')
par(family="Times")
axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000), labels=c(0.01,0.1, 1,10, 100, 1000), cex.axis=2.3)

box(col="black")

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

# legend("topright",legend=c(expression("N"[2]),
#                            expression("P"[2]),
#                            expression("p"[2])),
#        cex=2, lty=1, lwd=10, col=c('#33a02c','#e31a1c','#fb9a99'), bg="white")
dev.off()

####Population dynamics in the late crop habitat over 1 year####
pdf("PopDyn_LateCrop_1year.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
# #late crop
plot(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$N3,type="l", lwd=10, cex.axis = 2.5, cex.lab = 3, cex.main=2, 
     col='#33a02c',family="Times", axes=FALSE,
     ylab=expression(paste("Population density " (number/m^{2}))), xlab='Time (days)',
     log="y", ylim=c(0.01,1000))
lines(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$p3, type="l", lwd=10, cex.axis = 2, cex.lab = 2, col='#fb9a99')
lines(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$P3, type="l", lwd=10, col='#e31a1c')
par(family="Times")

axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000), labels=c(0.01,0.1, 1,10, 100, 1000), cex.axis=2.3)

box(col="black")

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

# legend("topleft",legend=c(expression("N"[3]), 
#                            expression("P"[3]), 
#                            expression("p"[3])),
#        cex=2, lty=1, lwd=10, col=c('#33a02c','#e31a1c','#fb9a99'), bg="white")
# # 
dev.off()


