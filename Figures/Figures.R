setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ##Set path to current dir
source("Pars_model.R")
source("Functions_Model.R")




B1l =0.05*100
points_x = seq(B1l+0, B1l+60, 2.5)

setwd("~/GitHub/Hover_aphid_model/Analysis_basic_model/Analysis/SNH_management/Figures")

####B1m####
#Save to pdf
pdf("Effect_B1m_meanpopsize_N1_N2.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))
plot(points_x,B1m_list[[1]]$Aphids_early ,type="l", lwd=10, col="white",  
     xlab=expression(""),ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     ylim=c(0,600))
points(points_x, B1m_list[[1]]$Aphids_early, col="#a1d99b", lwd=2,cex=3, pch=16)
points(points_x, B1m_list[[2]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, B1m_list[[3]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, B1m_list[[4]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)

points(points_x, B1m_list[[1]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)
points(points_x, B1m_list[[2]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)
points(points_x, B1m_list[[3]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)
points(points_x, B1m_list[[4]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)

abline(h=501.5, lwd=6, col="#377eb8") #absence of predators mean is 501.5

points(25+B1l, B1m_list[[1]]$Aphids_early[11], col="#a1d99b", cex=8, pch=4, lwd=4)
points(25+B1l, B1m_list[[1]]$Aphids_wood[11], col="#00441b", cex=8, pch=4, lwd=4)

mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)

axis(side=1,  at=c(0, 10,20,30,40,50, 60), cex.axis=2.5)

legend("topright", legend=c('Aphids in woody habitat', 'Aphids in early crop'), cex=3, pch=16, 
       col=c('#00441b', '#a1d99b'), bg="white")

dev.off()


#Save to pdf
pdf("Effect_B1m_meanpopsize_N3.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))


plot(points_x,B1m_list[[1]]$Aphids_late ,type="l", lwd=10, col="white",  
     xlab="",ylab=" ",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     # family="Times",
     ylim=c(0,10), xaxt="n")
points(points_x, B1m_list[[1]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, B1m_list[[2]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, B1m_list[[3]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, B1m_list[[4]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)

abline(h=1693, lwd=6, col="#377eb8") #absence of predators mean is 1693

points(25+B1l, B1m_list[[1]]$Aphids_late[11], col="#238b45", cex=8, pch=4, lwd=4)


mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)

mtext(expression("Maximum floral resource levels in the woody habitat (%)"), side=1, line=4, cex=2.5)

axis(side=1,  at=c(0, 10,20,30,40,50,60), cex.axis=2.5)

legend("topright", legend=c('Aphids in late crop'), cex=3, pch=16,
       col=c('#238b45'))

dev.off()


####n1s#####
points_x=c(0, 0.01579258, 0.03158517, 0.04737775, 0.06317033, 0.07896291, 0.0947555, 0.1105481,
             0.1263407, 0.1421332, 0.1579258, 0.1737184, 0.189511, 0.2053036, 0.2210962, 0.2368887,
             0.2526813, 0.273967) #max aphid growth rate per bif


#Save to pdf
pdf("Effect_n1s_meanpopsize_N1_N2.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 5.5, 4.3, 5.5))
plot(points_x,n1s_list[[1]]$Aphids_early ,type="l", lwd=10, col="white",  
     xlab=expression(""),ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     ylim=c(0,600))
points(points_x, n1s_list[[1]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, n1s_list[[2]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, n1s_list[[3]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, n1s_list[[4]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)

points(points_x, n1s_list[[1]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)
points(points_x, n1s_list[[2]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)
points(points_x, n1s_list[[3]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)
points(points_x, n1s_list[[4]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)

abline(h=501.5, lwd=6, col="#377eb8") #absence of predators mean is 501.5

points(0.1579258, n1s_list[[1]]$Aphids_early[11], col="#a1d99b", cex=8, pch=4, lwd=4)
points(0.1579258, n1s_list[[1]]$Aphids_wood[11], col="#00441b", cex=8, pch=4, lwd=4)


dev.off()


#Save to pdf
pdf("Effect_n1s_meanpopsize_N3.pdf", width=12, height=8)


#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 5.5, 4.3, 5.5))

plot(points_x,n1s_list[[1]]$Aphids_late ,type="l", lwd=10, col="white",  
     xlab="",ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     # family="Times",
     ylim=c(0,400))
points(points_x, n1s_list[[1]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, n1s_list[[2]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, n1s_list[[3]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, n1s_list[[4]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)

abline(h=1693, lwd=6, col="#377eb8") #absence of predators mean is 1693

points(0.1579258, n1s_list[[1]]$Aphids_late[11], col="#238b45", cex=8, pch=4, lwd=4)


mtext(expression(paste("Max aphid growth rate in spring" (day^{-1}))), side=1, line=5, cex=2.5)

dev.off()


####n1a####

points_x=c(0, 0.01622525, 0.0324505, 0.04867575, 0.064901, 0.08112625, 0.0973515, 0.1135767,
           0.129802, 0.1460272, 0.1622525, 0.1784777, 0.194703, 0.2109282, 0.2271535,
           0.2433787, 0.259604) #max aphid growth rate per bif
#Save to pdf
pdf("Effect_n1a_meanpopsize_N1_N2.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 4.3, 4.3, 8))
plot(points_x,n1a_list[[1]]$Aphids_early ,type="l", lwd=10, col="white",  
     xlab=expression(""),ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     ylim=c(0,600))
points(points_x, n1a_list[[1]]$Aphids_early, col="#a1d99b", lwd=2,cex=3, pch=16)
points(points_x, n1a_list[[2]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, n1a_list[[3]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, n1a_list[[4]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)

points(points_x, n1a_list[[1]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)
points(points_x, n1a_list[[2]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)
points(points_x, n1a_list[[3]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)
points(points_x, n1a_list[[4]]$Aphids_wood, col="#00441b", lwd=2, cex=3, pch=16)

abline(h=501.5, lwd=6, col="#377eb8") #absence of predators mean is 501.5

points(0.1622525, n1a_list[[1]]$Aphids_early[11], col="#a1d99b", cex=8, pch=4, lwd=4)
points(0.1622525, n1a_list[[1]]$Aphids_wood[11], col="#00441b", cex=8, pch=4, lwd=4)

dev.off()

#Save to pdf
pdf("Effect_n1a_meanpopsize_N3.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 4.3, 4.3, 8))

plot(points_x,n1a_list[[1]]$Aphids_late ,type="l", lwd=10, col="white",  
     xlab="",ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     # family="Times",
     ylim=c(0,35))
points(points_x, n1a_list[[1]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, n1a_list[[2]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, n1a_list[[3]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, n1a_list[[4]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)

abline(h=1693, lwd=6, col="#377eb8") #absence of predators mean is 1693

points(0.1622525, n1a_list[[1]]$Aphids_late[11], col="#238b45", cex=8, pch=4, lwd=4) 

mtext(expression(paste("Max aphid growth rate in autumn" (day^{-1}))), side=1, line=5, cex=2.5)


dev.off()

####T2s####
points_x = seq(30, 100, 5)

pdf("Effect_T2s_meanpopsize_N2.pdf", width=12, height=8)
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))
plot(points_x, T2s_annual_list[[1]]$Aphids_early ,type="l", lwd=10, col="white",  
     xlab="", ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5,
     # family="Times",
     ylim=c(0,600))
points(points_x, T2s_annual_list[[1]]$Aphids_early, col="#f768a1", lwd=2,cex=3, pch=16)
points(points_x, T2s_annual_list[[2]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, T2s_annual_list[[3]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, T2s_annual_list[[4]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)

points(points_x, T2s_perennial_list[[1]]$Aphids_early, col="#a1d99b", lwd=2,cex=3, pch=16)
points(points_x, T2s_perennial_list[[2]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, T2s_perennial_list[[3]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, T2s_perennial_list[[4]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)

abline(h=501.5, lwd=6, col="#377eb8") #absence of predators mean is 501.5

points(59, T2s_annual_list[[1]]$Aphids_early[6], col="#f768a1", cex=8, pch=4, lwd=4)
points(40, T2s_perennial_list[[1]]$Aphids_early[3], col="#a1d99b", cex=8, pch=4, lwd=4) 

mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)

legend("topleft", legend=c('Low floral abundance (cf. perennial)', 'High floral abundance (cf. annual)'), cex=2.5, pch=16, 
       col=c('#a1d99b', '#f768a1'), bg="white")

dev.off()


pdf("Effect_T2s_meanpopsize_N3.pdf", width=12, height=8)
#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))

plot(points_x, T2s_annual_list[[1]]$Aphids_late,type="l", lwd=10, col="white",  
     xlab="",ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     # family="Times",
     ylim=c(0,10))
points(points_x, T2s_annual_list[[1]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, T2s_annual_list[[2]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, T2s_annual_list[[3]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, T2s_annual_list[[4]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)


#perennial
points(points_x, T2s_perennial_list[[1]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, T2s_perennial_list[[2]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, T2s_perennial_list[[3]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, T2s_perennial_list[[4]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)


abline(h=1693, lwd=6, col="#377eb8") #absence of predators mean is 1693

points(59, T2s_annual_list[[1]]$Aphids_late[6], col="#984ea3", cex=8, pch=4, lwd=4)
points(40, T2s_perennial_list[[1]]$Aphids_late[3], col="#238b45", cex=8, pch=4, lwd=4) 


mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)

mtext(expression("Start of flowering early crop field margin"), side=1, line=4, cex=2.5)
mtext(expression("(days since April 1)"), side=1, line=7, cex=2.5)

legend("topleft", legend=c('Low floral abundance (cf. perennial)', 'High floral abundance (cf. annual)'), cex=2.5, pch=16,
       col=c('#238b45', '#984ea3'))

dev.off()

####T3s####
points_x = seq(30, 100, 5)

pdf("Effect_T3s_meanpopsize_N2.pdf", width=12, height=8)
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))
plot(points_x, T3s_annual_list[[1]]$Aphids_early ,type="l", lwd=10, col="white",  
     xlab="", ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5,
     # family="Times",
     ylim=c(0,200))
points(points_x, T3s_annual_list[[1]]$Aphids_early, col="#f768a1", lwd=2,cex=3, pch=16)
points(points_x, T3s_annual_list[[2]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, T3s_annual_list[[3]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, T3s_annual_list[[4]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)

points(points_x, T3s_perennial_list[[1]]$Aphids_early, col="#a1d99b", lwd=2,cex=3, pch=16)
points(points_x, T3s_perennial_list[[2]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, T3s_perennial_list[[3]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, T3s_perennial_list[[4]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)

abline(h=501.5, lwd=6, col="#377eb8") #absence of predators mean is 501.5

points(59, T3s_annual_list[[1]]$Aphids_early[6], col="#f768a1", cex=8, pch=4, lwd=4)
points(40, T3s_perennial_list[[1]]$Aphids_early[3], col="#a1d99b", cex=8, pch=4, lwd=4) 

# mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)

dev.off()


pdf("Effect_T3s_meanpopsize_N3.pdf", width=12, height=8)
#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))

plot(points_x, T3s_annual_list[[1]]$Aphids_late,type="l", lwd=10, col="white",  
     xlab="",ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     # family="Times",
     ylim=c(0,5))
points(points_x, T3s_annual_list[[1]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, T3s_annual_list[[2]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, T3s_annual_list[[3]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, T3s_annual_list[[4]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)


#perennial
points(points_x, T3s_perennial_list[[1]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, T3s_perennial_list[[2]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, T3s_perennial_list[[3]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, T3s_perennial_list[[4]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)


abline(h=1693, lwd=6, col="#377eb8") #absence of predators mean is 1693

points(59, T3s_annual_list[[1]]$Aphids_late[6], col="#984ea3", cex=8, pch=4, lwd=4)
points(40, T3s_perennial_list[[1]]$Aphids_late[3], col="#238b45", cex=8, pch=4, lwd=4) 


# mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)

mtext(expression("Start of flowering late crop field margin"), side=1, line=4, cex=2.5)
mtext(expression("(days since April 1)"), side=1, line=7, cex=2.5)

dev.off()
####B2m#####

B2l =0.02*100
points_x = seq(B2l+0, B2l+60, 2.5)

#Save to pdf
pdf("Effect_B2m_meanpopsize_N2_perennial.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))
plot(points_x, B2m_annual_list[[1]]$Aphids_early ,type="l", lwd=10, col="white",  
     xlab="", ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5,
     # family="Times",
     ylim=c(0,600))
points(points_x, B2m_annual_list[[1]]$Aphids_early, col="#f768a1", lwd=2,cex=3, pch=16)
points(points_x, B2m_annual_list[[2]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, B2m_annual_list[[3]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, B2m_annual_list[[4]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)

points(points_x, B2m_perennial_list[[1]]$Aphids_early, col="#a1d99b", lwd=2,cex=3, pch=16)
points(points_x, B2m_perennial_list[[2]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, B2m_perennial_list[[3]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, B2m_perennial_list[[4]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)

abline(h=501.5, lwd=6, col="#377eb8") #absence of predators mean is 501.5

points(B2l+49, B2m_annual_list[[1]]$Aphids_early[21], col="#f768a1", cex=8, pch=4, lwd=4) #annaul flowering coverage
points(B2l+26, B2m_perennial_list[[1]]$Aphids_early[11], col="#a1d99b", cex=8, pch=4, lwd=4) #perennial flower coverage


mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)


legend("topright", legend=c('Early flowering (cf. perennial)', 'Late flowering (cf. annual)'), cex=2.5, pch=16,
       col=c('#a1d99b', '#f768a1'), bg="white")

dev.off()



#Save to pdf
pdf("Effect_B2m_meanpopsize_N3_perennial.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))

plot(points_x, B2m_annual_list[[1]]$Aphids_late,type="l", lwd=10, col="white",  
     xlab="",ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     # family="Times",
     ylim=c(0,10))
points(points_x, B2m_annual_list[[1]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, B2m_annual_list[[2]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, B2m_annual_list[[3]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, B2m_annual_list[[4]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)

#perennial
points(points_x, B2m_perennial_list[[1]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, B2m_perennial_list[[2]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, B2m_perennial_list[[3]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, B2m_perennial_list[[4]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)


abline(h=1693, lwd=6, col="#377eb8") #absence of predators mean is 1693

points(B2l+49, B2m_annual_list[[1]]$Aphids_late[21], col="#984ea3", cex=8, pch=4, lwd=4) #annaul flowering coverage
points(B2l+26, B2m_perennial_list[[1]]$Aphids_late[11], col="#238b45", cex=8, pch=4, lwd=4) #perennial flower coverage

axis(side=1,  at=c(0, 10, 20, 30,40,50), cex.axis=2.5)

mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)

mtext(expression("Maximum floral resource levels next to early crop (%)"), side=1, line=4, cex=2.5)



legend("topright", legend=c('Early flowering (cf. perennial)', 'Late flowering (cf. annual)'), cex=2.5, pch=16,
       col=c('#238b45', '#984ea3'))

dev.off()

####B3m####
#Save to pdf
pdf("Effect_B3m_meanpopsize_N2_perennial.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))
plot(points_x, B3m_annual_list[[1]]$Aphids_early ,type="l", lwd=10, col="white",  
     xlab="", ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5,
     # family="Times",
     ylim=c(0,600))
points(points_x, B3m_annual_list[[1]]$Aphids_early, col="#f768a1", lwd=2,cex=3, pch=16)
points(points_x, B3m_annual_list[[2]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, B3m_annual_list[[3]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, B3m_annual_list[[4]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)

points(points_x, B3m_perennial_list[[1]]$Aphids_early, col="#a1d99b", lwd=2,cex=3, pch=16)
points(points_x, B3m_perennial_list[[2]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, B3m_perennial_list[[3]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, B3m_perennial_list[[4]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)

abline(h=501.5, lwd=6, col="#377eb8") #absence of predators mean is 501.5

points(B2l+49, B3m_annual_list[[1]]$Aphids_early[21], col="#f768a1", cex=8, pch=4, lwd=4) #annaul flowering coverage
points(B2l+26, B3m_perennial_list[[1]]$Aphids_early[11], col="#a1d99b", cex=8, pch=4, lwd=4) #perennial flower coverage


# mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)



dev.off()



#Save to pdf
pdf("Effect_B3m_meanpopsize_N3_perennial.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))

plot(points_x, B3m_annual_list[[1]]$Aphids_late,type="l", lwd=10, col="white",  
     xlab="",ylab="",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     # family="Times",
     ylim=c(0,10))
points(points_x, B3m_annual_list[[1]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, B3m_annual_list[[2]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, B3m_annual_list[[3]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, B3m_annual_list[[4]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)

#perennial
points(points_x, B3m_perennial_list[[1]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, B3m_perennial_list[[2]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, B3m_perennial_list[[3]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, B3m_perennial_list[[4]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)


abline(h=1693, lwd=6, col="#377eb8") #absence of predators mean is 1693

points(B2l+49, B3m_annual_list[[1]]$Aphids_late[21], col="#984ea3", cex=8, pch=4, lwd=4) #annaul flowering coverage
points(B2l+26, B3m_perennial_list[[1]]$Aphids_late[11], col="#238b45", cex=8, pch=4, lwd=4) #perennial flower coverage

axis(side=1,  at=c(0, 10, 20, 30,40,50), cex.axis=2.5)

# mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)

mtext(expression("Maximum floral resource levels next to late crop (%)"), side=1, line=4, cex=2.5)



dev.off()


####Mowing early M2s####
points_x=seq(0,210,10)

pdf("Effect_M2s_meanpopsize_N2.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))
plot(points_x, Mowing_early_mort_list[[1]]$Aphids_early ,type="l", lwd=10, col="white",  
     xlab="", ylab="", xaxt = "n",
     cex.main=3, cex.lab=2.5, cex.axis=2.5,
     # family="Times",
     ylim=c(0,600))
points(points_x, Mowing_early_mort_list[[1]]$Aphids_early, col="#f768a1", lwd=2,cex=3, pch=16)
points(points_x, Mowing_early_mort_list[[2]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, Mowing_early_mort_list[[3]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, Mowing_early_mort_list[[4]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)

points(points_x, Mowing_early_nomort_list[[1]]$Aphids_early, col="#a1d99b", lwd=2,cex=3, pch=16)
points(points_x, Mowing_early_nomort_list[[2]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, Mowing_early_nomort_list[[3]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, Mowing_early_nomort_list[[4]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)

abline(h=501.5, lwd=6, col="#377eb8") #absence of predators mean is 501.5

axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)

mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)


legend("topleft", legend=c('Mowing has no effect on hoverfly mortality', "Mowing causes 100% mortality in hoverflies"), cex=2, pch=16,
       col=c('#a1d99b', '#f768a1'), bg="white")

dev.off()



pdf("Effect_M2s_meanpopsize_N3.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))

plot(points_x, Mowing_early_mort_list[[1]]$Aphids_late,type="l", lwd=10, col="white",  
     xlab="",ylab="", xaxt = "n",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     # family="Times",
     ylim=c(0,10))
points(points_x, Mowing_early_mort_list[[1]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, Mowing_early_mort_list[[2]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, Mowing_early_mort_list[[3]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, Mowing_early_mort_list[[4]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)

points(points_x, Mowing_early_nomort_list[[1]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, Mowing_early_nomort_list[[2]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, Mowing_early_nomort_list[[3]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, Mowing_early_nomort_list[[4]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)

axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)


abline(h=1693, lwd=6, col="#377eb8") #absence of predators mean is 1693


mtext(expression(paste("Aphid density " (m^{-2}))), side=2, line=4, cex=2.5)
mtext(expression("Mowing date next to the early crop"), side=1, line=4, cex=2.5)
mtext(expression("(days since April 1)"), side=1, line=7, cex=2.5)


legend("topleft", legend=c('Mowing has no effect on hoverfly mortality', "Mowing causes 100% mortality in hoverflies"), cex=2, pch=16,
       col=c('#238b45','#984ea3'))

dev.off()



#Save to pdf
pdf("Effect_M3s_meanpopsize_N2.pdf", width=12, height=8)

#Plotting
par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))
plot(points_x, Mowing_late_mort_list[[1]]$Aphids_early ,type="l", lwd=10, col="white",  
     xlab="", ylab="",xaxt = "n",
     cex.main=3, cex.lab=2.5, cex.axis=2.5,
     # family="Times",
     ylim=c(0,600))
points(points_x, Mowing_late_mort_list[[1]]$Aphids_early, col="#f768a1", lwd=2,cex=3, pch=16)
points(points_x, Mowing_late_mort_list[[2]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, Mowing_late_mort_list[[3]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)
points(points_x, Mowing_late_mort_list[[4]]$Aphids_early, col="#f768a1", lwd=2, cex=3, pch=16)

points(points_x, Mowing_late_nomort_list[[1]]$Aphids_early, col="#a1d99b", lwd=2,cex=3, pch=16)
points(points_x, Mowing_late_nomort_list[[2]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, Mowing_late_nomort_list[[3]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)
points(points_x, Mowing_late_nomort_list[[4]]$Aphids_early, col="#a1d99b", lwd=2, cex=3, pch=16)

axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)

abline(h=501.5, lwd=6, col="#377eb8") #absence of predators mean is 501.5


dev.off()



#Save to pdf
pdf("Effect_M3s_meanpopsize_N3.pdf", width=12, height=8)

par(mgp=c(4,1.5, 0), mar=c(8, 8, 4.3, 3))
#Plotting
plot(points_x, Mowing_late_mort_list[[1]]$Aphids_late,type="l", lwd=10, col="white",  
     xlab="",ylab="",xaxt = "n",
     cex.main=3, cex.lab=2.5, cex.axis=2.5, 
     # family="Times",
     ylim=c(0,10))
points(points_x, Mowing_late_mort_list[[1]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, Mowing_late_mort_list[[2]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, Mowing_late_mort_list[[3]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)
points(points_x, Mowing_late_mort_list[[4]]$Aphids_late, col="#984ea3", lwd=2, cex=3, pch=16)

points(points_x, Mowing_late_nomort_list[[1]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, Mowing_late_nomort_list[[2]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, Mowing_late_nomort_list[[3]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)
points(points_x, Mowing_late_nomort_list[[4]]$Aphids_late, col="#238b45", lwd=2, cex=3, pch=16)

axis(side=1, at=c(0,30, 60, 90, 120, 150, 180,210), labels=c(0,30, 60, 90, 120, 150, 180,210),cex.axis=2)

abline(h=1693, lwd=6, col="#377eb8") #absence of predators mean is 1693

mtext(expression("Mowing date next to the late crop"), side=1, line=4, cex=2.5)
mtext(expression("(days since April 1)"), side=1, line=7, cex=2.5)


dev.off()
