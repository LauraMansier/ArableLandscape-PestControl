setwd("") #set the working directory to where you have saved the model output

data_frames=readRDS("ArableLandscape-PestControl.RData")
data_frame_PopDyn_subset=subset(data_frames, timestep<210)

####SHRUBS####
#Figure showing the population dynamics of aphids and hoverflies in the woody habitat (H1) in 1 year
pdf("PopDyn_Shrubs_1year.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))

#Creating the plot with timestep and population dynamics of aphids in the woody habitat (H1)
plot(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$N1,type="l", lwd=10, cex.axis = 2.5, cex.lab = 3, cex.main=2, 
     col="#33a02c", family="Times", xlab='Time (days)', axes=FALSE,
     ylab=expression(paste("Population density " (number/m^{2}))), log="y", ylim=c(0.01,1000))
par(family="Times")
#Creating the axes
axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000), labels=c(0.01,0.1, 1,10, 100, 1000), cex.axis=2.3)

box(col="black")

#Population dynamics for the juvenile hoverflies in the woody habitat (H1)
lines(data_frame_PopDyn_subset$timestep,data_frame_PopDyn_subset$p1,type="l", lwd=10, col="#fb9a99")

#Population dynamics for the adult hoverflies in the woody habitat (H1)
lines(data_frame_PopDyn_subset$timestep,data_frame_PopDyn_subset$P1,type="l", lwd=10, col="#e31a1c")

#Population dynamics for the adult hoverflies in hibernation
lines(data_frame_PopDyn_subset$timestep,data_frame_PopDyn_subset$P0,type="l", lwd=10, col="#6a3d9a")
par(family="Times")

#Drawing vertical lines showing the months in the year
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

#Creating the legend
legend("topright",legend=c(expression("Aphids"), 
                           expression("Adult hoverflies"), 
                           expression("Juvenile hoverflies"),
                           expression("Hibernating hoverflies")), cex=1.75, lty=1, lwd=10, 
       col=c('#33a02c','#e31a1c','#fb9a99','#6a3d9a'), bg="white")
dev.off()

####EARLY CROP####
#Figure showing the population dynamics of aphids and hoverflies in the early crop (H2) in 1 year
pdf("PopDyn_EarlyCrop_1year.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
#Creating the plot with timestep and population dynamics of aphids in the early crop (H2)
plot(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$N2,type="l", lwd=10, cex.axis = 2.5, cex.lab = 3, cex.main=2,
     col='#33a02c', family="Times", axes=FALSE,
     ylab=expression(paste("Population density " (number/m^{2}))),xlab='Time (days)',
     log="y", ylim=c(0.01,1000))
#Creating the axes
axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000), labels=c(0.01,0.1, 1,10, 100, 1000), cex.axis=2.3)
#Population dynamics for the juvenile hoverflies in the early crop (H2)
lines(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$p2, type="l", lwd=10, cex.axis = 2, cex.lab = 2, col='#fb9a99')
#Population dynamics for the adult hoverflies in the late crop (H2)
lines(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$P2, type="l", lwd=10, cex.axis = 2, cex.lab = 2, col='#e31a1c')

par(family="Times")
box(col="black")

#Drawing vertical lines showing the months in the year
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

## Creating the legend, already created this for the plot of the woody habitat so, therefore, this one is commented out
# legend("topright",legend=c(expression("Aphids"), 
#                           expression("Adult hoverflies"), 
#                           expression("Juvenile hoverflies"),
#                           expression("Hibernating hoverflies")), cex=1.75, lty=1, lwd=10, 
#       col=c('#33a02c','#e31a1c','#fb9a99','#6a3d9a'), bg="white")
dev.off()

####LATE CROP####
#Figure showing the population dynamics of aphids and hoverflies in the late crop (H3) in 1 year
pdf("PopDyn_LateCrop_1year.pdf", width = 8)
par(mar=c(5.1,6.5,3,1.5))
#Creating the plot with timestep and population dynamics of aphids in the late crop (H3)
plot(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$N3,type="l", lwd=10, cex.axis = 2.5, cex.lab = 3, cex.main=2, 
     col='#33a02c',family="Times", axes=FALSE,
     ylab=expression(paste("Population density " (number/m^{2}))), xlab='Time (days)',
     log="y", ylim=c(0.01,1000))
#Creating the axes
axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)
axis(side=2, at=c(0.01, 0.1, 1,10, 100, 1000), labels=c(0.01,0.1, 1,10, 100, 1000), cex.axis=2.3)
#Population dynamics for the juvenile hoverflies in the late crop (H3)
lines(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$p3, type="l", lwd=10, cex.axis = 2, cex.lab = 2, col='#fb9a99')
#Population dynamics for the adult hoverflies in the late crop (H3)
lines(data_frame_PopDyn_subset$timestep, data_frame_PopDyn_subset$P3, type="l", lwd=10, col='#e31a1c')

par(family="Times")
box(col="black")

#Drawing vertical lines showing the months in the year
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

## Creating the legend, already created this for the plot of the woody habitat so, therefore, this one is commented out
# legend("topright",legend=c(expression("Aphids"), 
#                           expression("Adult hoverflies"), 
#                           expression("Juvenile hoverflies"),
#                           expression("Hibernating hoverflies")), cex=1.75, lty=1, lwd=10, 
#       col=c('#33a02c','#e31a1c','#fb9a99','#6a3d9a'), bg="white")
dev.off()


