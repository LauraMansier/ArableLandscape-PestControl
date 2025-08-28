###Parameters of the model####
#PARAMETER, value, description, units#####
#Switches (1=present, 0=absent) 
SA1 <- 1         #Spring aphids habitat 1 
AA1 <- 1         #Autumn aphids habitat 1
A2 <- 1          #Aphids habitat 2
A3 <- 1          #Aphids habitat 3

F1 <- 1         #Spring flowers habitat 1
F2 <- 1         #Flowers habitat 2 
F3 <- 1          #Flowers habitat 3 

I2 <- 0          #Mortality event habitat 2
I3 <- 0          #Mortality event habitat 3

#Temperature parameters
Ta <- 7.7        #Amplitude of the temperature cosine function, degrees Celsius
td <- 202-90     #Day at which the temperature is at its highest, day
Tc <- 10.7       #Average temperature over season, degrees Celsius

##Habitat and seasonal resource parameters
c <- 0.5                   #Constant to vary habitat fractions of habitat 2 & 3 relative to habitat 1
alpha1 <- 0.05             #Proportion habitat 1 (woody habitat) in landscape, proportion
alpha2 <- (1-alpha1) * c     #Proportion habitat 2 (early winter wheat and flower margin) in landscape, proportion
alpha3 <- (1-alpha1) * (1-c) #Proportion habitat 3 (late potato and flower margin) in landscape, proportion

# Floral resources
B23l <- 0.02       #Background level floral resources in habitat 2 and 3 (crop fields), proportion
B1l <- 0.05        #Background level floral resources in habitat 1 (woody habitat), proportion
ks <- 2            #Shape Weibull distribution of flowering and aphid growth, -
k1 <- 2.8          #Shape Weibull distribution of flowering of woody habitat, -
B1m <- 0.25        #Maximum level floral resources habitat 1 in spring, proportion
T1s <- -7          #Start day flowering habitat 1 in spring, day
T1d <- 53          #Duration flowering habitat 1 in spring, days
B2m <- 0.49        #Maximum level floral resources habitat 2, proportion
T2s <- 59          #Start day flowering habitat 2, day
T2d <- 73          #Duration flowering habitat 2, days
B3m <- 0.49        #Maximum level floral resources habitat 3, proportion
T3s <- 59          #Start day flowering habitat 3, day
T3d <- 73          #Duration flowering habitat 3, days   

# Aphid resources
n1s <- 1.15        #Maximum level aphid resource supply in spring in habitat 1 
n1a <- 0.8625      #Maximum level aphid resource supply in autumn in habitat 1
A1s <- -19         #Start day aphid growth habitat 1 in spring, day
A1d <- 56          #Duration aphid growth in habitat 1 in spring, days
A1as <- 150        #Start day aphid growth in habitat 1 in autumn, day
A1ad <- 36         #Duration aphid growth in habitat 1 in autumn, days
A2f <- 120         #Final day aphid growth habitat 2, days
A2d <- 65          #Duration aphid growth habitat 2, days
A3f <- 180         #Final day aphid growth habitat 3, days
A3d <- 53          #Duration aphid growth habitat 3, days

#Aphid parameters
phi <- 1e-5        #Small corrective value for numerical stability (to prevent K2 and K3 to reach 0 and thus preventing division by 0 in logistic growth of aphids)
K1 <- 10000        #Maximum aphid density (carrying capacity) in habitat 1, aphids/m2
Km <- 20000        #Maximum aphid density (carrying capacity) in habitat 2 & 3, aphids/m2
rN1 <- 0.38        #Aphid maximum intrinsic rate of increase in habitat 1 at 22C, day^-1
rN2 <- 0.35        #Aphid maximum intrinsic rate of increase in habitat 2 at 22C, day^-1
rN3 <- 0.32        #Aphid maximum intrinsic rate of increase in habitat 3 at 22C, day^-1
mu1 <- 0.12        #Aphid death rate due to biotic and abiotic factors in habitat 1, day^-1
mu2 <- 0.08        #Aphid death rate due to biotic and abiotic factors in habitat 2, day^-1
mu3 <- 0.08        #Aphid death rate due to biotic and abiotic factors in habitat 3, day^-1


#Hoverfly parameters
em <- 1/13         #Maximum development rate hoverfly, 1/developmental period in days
G1m <- 25          #Maximum reproduction rate hoverfly in habitat 1, eggs female^-1 day^-1
G2m <- 30          #Maximum reproduction rate hoverfly in habitat 2 & 3, eggs female^-1 day^-1
fm <- 40           #Maximum per capita predation rate, aphids*hoverfly^-1*day^-1   
Nf <- 200          #Half saturation density functional response, aphids/m2
Ne <- Nf/2         #Half saturation density hoverfly juvenile development, aphids/m2
Ng <- Nf/2         #Half saturation density ovipositional/numerical response, aphids/m2
mu0 <- 0.03        #Mortality rate adult hoverflies in winter habitat, day^-1
muPD <- 0.12       #Mortality rate dispersing adult hoverflies, day^-1
muP <- 0.48        #Mortality rate for ill-fed adult hoverflies in all habitats, day^-1  
mum <- 0.01        #Mortality rate juvenile hoverflies at abundant aphids, day^-1
tauh <- 11         #Time delay due to hoverfly pupation and maturation, days

#Hoverfly distribution between habitats/states
H0 <- 30           #Mean time of hoverfly emergence from hibernation, day
H1 <- 183          #Mean time of hoverfly going into hibernation, day
sigma0 <- 8        #Standard deviation of hoverfly emergence from hibernation, days
sigma1 <- 9        #Standard deviation of hoverfly going into hibernation, days
D0 <- 50           #Maximum dispersal rate (in absence of food)
epsilon <- 0.25    #Shape parameter of simple dispersal function
#Transition rates between subhabitats due to:
b <- 8             #Feeding when in flower habitat, depends on flower density, day^-1
a <- 0.6           #Energy use when in aphid habitat, day^-1
da <- 2            #Dispersal from flower to aphid habitat, day^-1
db <- 0.5          #Dispersal from aphid to flower habitat, day^-1

#Seasonal mortality
WN <- 0.25         #Winter survival aphids in habitat 1, proportion
WP <- 0.3          #Winter survival adult hoverflies in hibernation, proportion
W <- 1e-10         #Survival seasonal events, proportion
Wt1 <- 209         #Time of winter mortality event, day
Wt2 <- A2f         #Time of harvest winter wheat habitat 2, day
Wt3 <- A3f         #Time of haulm killing potato habitat 3, day

#Mortality event parameters
Is2 <- 120         #Start day mortality event habitat 2, day
Is3 <- 170         #Start day mortality event habitat 3, day
sN <- 0.05         #Survival aphids and hoverfly larvae after mortality event, proportion
sP <- 0.5          #Survival adult hoverflies after mortality event, proportion

#Initial population densities
Pi <- 0.02         #Hoverflies (into hibernating population, P0)
N1i <- 0.8         #Aphids habitat 1, aphids/m2
N2i <- 0.0015      #Yearly infestation aphids habitat 2, aphids/m2  
N3i <- 0.0015      #Yearly infestation aphids habitat 3, aphids/m2 
T2i <- 1           #Time of infestation aphids habitat 2, day 
T3i <- 55          #Time of infestation aphids habitat 3, day 

#mowing parameters#
M2s <- 90 #start of mowing in early crop flower margin 
M3s <- 90 #start of mowing in late crop flower margin
Md <- 45 #time before regrowing
km <- 1.05 #Shape Weibull distribution of mowing function, - NEW
FM2 <- 0 #mowing early crop flower margin on/off
FM3 <- 0 #mowing late flower margin on/off


Nr <- 0.5           #REFUGE in habitat 1: part of aphids not effected by predation, aphids/m2 #*


parms <- c(SA1  =  SA1, AA1 = AA1, A2 = A2, A3 = A3, F1 = F1, F2 = F2, F3 = F3, 
           I2 = I2, I3 = I3, Ta = Ta, td = td, Tc = Tc, c = c, alpha1 = alpha1, 
           alpha2 = alpha2, alpha3 = alpha3, B23l = B23l, ks = ks, k1 = k1, 
           A1s = A1s, A1as = A1as, A1d = A1d, A1ad = A1ad, B1l = B1l, B1m = B1m, T1s = T1s, 
           T1d = T1d, B2m = B2m, T2s = T2s, T2d = T2d, B3m = B3m, T3s = T3s, T3d = T3d, n1s = n1s, 
           n1a = n1a, A2f = A2f, A2d = A2d, A3f = A3f, A3d = A3d, phi = phi, K1 = K1, Km = Km, 
           T2i = T2i, T3i = T3i, Nf = Nf, Ne = Ne, Ng = Ng, em  =  em, G1m = G1m,G2m = G2m, 
           fm = fm, mu0 = mu0, muPD = muPD, muP = muP, mum = mum, rN1 = rN1,
           rN2 = rN2, rN3 = rN3, mu1 = mu1, mu2 = mu2, mu3 = mu3, WN = WN, WP = WP, W = W, 
           Wt1 = Wt1, Wt2 = Wt2, Wt3 = Wt3, tauh = tauh, H0 = H0, H1 = H1, 
           sigma0 = sigma0, sigma1 = sigma1, epsilon = epsilon,
           b = b, a = a, da = da, db = db, Is2 = Is2, Is3 = Is3, sN = sN, sP = sP, Pi = Pi, 
           N1i = N1i, N2i = N2i, N3i = N3i, D0 = D0,
           M2s = M2s, M3s = M3s, Md = Md, km = km, FM2 = FM2, FM3 = FM3,
           Nr = Nr)

