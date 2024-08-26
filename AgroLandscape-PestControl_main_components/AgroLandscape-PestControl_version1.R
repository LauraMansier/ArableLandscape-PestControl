rm(list=ls()) #removes all variables

# The model simulates how different landscape compositions, 
# including early-season crops, late-season crops, flower-rich field margins, 
# and woody areas, affect the survival and effectiveness of hoverflies as natural 
# pest control agents of aphids. 
# The model contains 3 distinct habitats: (1) Woody habitat, (2) an early crop with a neighboring 
#flower margin and (3) a late crop with a neighboring flower margin.
# For an in-depth model description, please check out our publication:
# Mansier, L & van Rijn, P.C.J. (In press). Agricultural landscape management for natural pest control. 
# Journal of Applied Ecology

#Overview of script:
#1. All the supporting functions and ODEs/DDEs are given
#2. Parameters are stated
#3. The simulation time in years is stated (default=20 years)
#4. Initial values of state variables are stated
#5. Simulation
#6. Data frame of output is generated and can be saved


library(deSolve)


start_time=Sys.time()

#THE MODEL
lvpred=function(t,n,parms){
  with(as.list(parms), {
    N1=n[1]; N2=n[2]; N3=n[3]; P0=n[4]; p1=n[5]; P1=n[6]; p2=n[7]; P2=n[8]; p3=n[9]; P3=n[10]; PD=n[11]
    
    ####SUPPORTING FUNCTIONS####
    
    #SEASONAL FORCING FUNCTIONS varying over days
    
    # x is day of the year used as input for seasonal forcing functions for multiple years
    x = function(t) {
      year= floor(t/210)
      return(t - 210*year)
    }   
    
    #Temperature change within a year
    Temp=function(t){
      Ta*cos((x(t)-td)*2*pi/365)+Tc
    }
    
    #Temperature correction
    tc=function(t){
      (Temp(t)-4)/(22-4)
    }
    
    #Floral F
    #Functions flower food per habitat
    B1=function(t){
      B1l + B1m*dweibull((x(t)-T1s)/T1d,k1,1)*F1
    }
    
    B2=function(t){
      B23l + B2m*dweibull((x(t)-T2s)/T2d,ks,1)*F2
    }
    B3=function(t){
      B23l + B3m*dweibull((x(t)-T3s)/T3d,ks,1)*F3
    }
    
    
    #APHID GROWTH
    #Functions aphid resource availability and carrying capacity (Weibull, reverse Weibull) per habitat
    n1=function(t){
      n1s*dweibull((x(t)-A1s)/A1d,ks,1)*SA1 + n1a*dweibull((x(t)-A1as)/A1ad,ks,1)*AA1
    }
    
    K2=function(t){
      Km*dweibull((A2f-x(t))/A2d,ks,1)*A2+phi
    }
    
    K3=function(t){
      Km*dweibull((A3f-x(t))/A3d,ks,1)*A3+phi
    }
    
    n2=function(t){
      ifelse(K2(t)>0.01*Km, 1, 0)
    }
    
    n3=function(t){
      ifelse(K3(t)>0.01*Km, 1, 0)
    }
    
    
    #Aphid population growth rate per habitat
    r1=function(t){
      n1(t)*rN1*tc(t)
    }
    
    r2=function(t){
      n2(t)*rN2*tc(t)
    }
    
    r3=function(t){
      n3(t)*rN3*tc(t)
    }
    
    
    #HOVERFLy LIFE HISTORY
    #Type 2 functional response hoverflies to aphids
    f=function(N, t){
      fm*tc(t)*(N/(N+Nf))
    }

    #Juvenile developmental rate hoverflies
    e=function(N, t){
      em*(N/(N+Ne)+phi)*tc(t)
    }
    
    #Juvenile mortality rate hoverflies
    muj=function(N, t){
      mum*((N+Ne*0.5)/(N+epsilon))*tc(t)
    }
    
    #Reproduction rate hoverflies (lower in H1)
    G1=function(N, t){
      G1m*tc(t)*(N/(N+Ng))
    }
    G2=function(N, t){
      G2m*tc(t)*(N/(N+Ng))
    }
    
    #HOVERFLY FORAGING BETWEEN SUB-HABITATS
    #Within-habitat distribution model
    Q=function(B){
      ((1/db)+(1/a)+(1/(b*B))+(1/da))^-1
    }
    
    #Proportion ill fed in flower habitat -> mortality
    sbi=function(B){
      Q(B)/(b*B)
    }
    #proportion well fed in flower habitat
    sbf=function(B){
      Q(B)/da
    }
    #proportion ill fed in aphid habitat
    sai=function(B){
      Q(B)/db
    }
    #proportion well fed in aphid habitat -> reproduction
    saf=function(B){
      Q(B)/a
    }
    # total proportion on flowers
    sf=function(B){
      sbi(B)+sbf(B)
    }
    
    #HOVERFLY DISPERSAL AMONG HABITATS
    #Dispersal from Ph into disperser pool (induced by lack of resources locally = inversely related to local fitness R0)
    # R0: daily reproduction x female longevity x juvenile survival
    R0=function(N,B,t) {
      G2(N,t)*saf(B)*(muP*sbi(B))^-1 *exp(-muj(N,t)/e(N,t))
    }
    
    #Emigration equal to immigration when no food present, otherwise inversely related to R0, but when R0=0 still affected by N:
    D=function(N, B,t){
      D0/(R0(N,B,t)+0.01*N+1)
    }
    
    #HOVERFLY HIBERNATION
    #Release from hibernation (around day 30) h0
 
    h0=function(t){
      sigma0*dnorm(x(t),H0,sigma0)
    }
    
    #Induction into hibernation (around day 183) h1  
    h1=function(t){
      sigma1*dnorm(x(t),H1,sigma1)
    }
    
    #HOVERFLY INTERMEDIATE PHASE (non-reproductive and non-predatory)
    # Delay representing hoverfly intermediate phase
    tau=function(t){
      tauh/tc(t)
    }  
    
    #MORTALITY
    #Mortality events habitat 2 and 3 
    etha2=function(t,S){
      ifelse(Is2<x(t) && x(t)<(Is2+3),-log(S)/3,0)*I2
    }
    etha3=function(t,S){
      ifelse(Is3<x(t) && x(t)<(Is3+3),-log(S)/3,0)*I3
    }
    
    #Seasonal mortality (periodic) enforced over last day of year or after harvest in habitat 2 & 3
    #low mortality
    ml=function(t,W,Wth){
      ifelse(Wth<x(t) && x(t)<(Wth+1),-log(W),0)
    }
    
    #maximum mortality
    mm=function(t,W,Wth){
      ifelse(Wth<x(t) && x(t)<(Wth+1), -log(W),0)
    }
    
    #INFESTATION
    #Yearly infestation of annual habitat with aphids
    inN2=function(t){
      ifelse(T2i<x(t) && x(t)<(T2i+1), N2i, 0)
    }
    inN3=function(t){
      ifelse(T3i<x(t) && x(t)<(T3i+1), N3i, 0)
    }
    
    ####STATE VARIABLE DERIVATIVES (DIFFERENTIAL EQUATIONS)####
    
    #N1
    dn1dt= r1(t)*(1-pmin(N1/K1,1))*N1 - mu1*tc(t)*N1 - p1*f(N1,t) - ml(t,WN,Wt1)*N1
    #N2
    dn2dt= inN2(t) + r2(t)*(1-pmin((N2/K2(t)),1))*N2 - mu2*tc(t)*N2 - p2*f(N2, t) - mm(t,W,Wt2)*N2 - etha2(t,sN)*N2
    #N3
    dn3dt= inN3(t) + r3(t)*(1-pmin((N3/K3(t)),1))*N3 - mu3*tc(t)*N3 - p3*f(N3, t) - mm(t,W,Wt3)*N3 - etha3(t,sN)*N3
    #P0
    dn4dt= h1(t)*PD - h0(t)*P0 - mu0*tc(t)*P0 - ml(t,WP,Wt1)*P0
    #p1
    dn5dt= G1(N1, t)*saf(B1(t))*P1 - muj(N1, t)*p1 - e(N1, t)*p1 - mm(t,W,Wt1)*p1
    #delay for P1
    lag1= ifelse((x(t)-tau(t))<0, 0, e(lagvalue(t-tau(t),1), t-tau(t))*lagvalue(t-tau(t),5))
    #P1
    dn6dt= lag1 + D0*PD - muP*tc(t)*sbi(B1(t))*P1 - D(N1,B1(t),t)*P1 - mm(t,W,Wt1)*P1
    #p2
    dn7dt= G2(N2, t)*saf(B2(t))*P2-muj(N2, t)*p2 - e(N2, t)*p2 - mm(t,W,Wt1)*p2 - etha2(t,sN)*p2
    #delay for P2
    lag2= ifelse((x(t)-tau(t))<0, 0, e(lagvalue(t-tau(t),2), t-tau(t))*lagvalue(t-tau(t),7))
    #P2
    dn8dt= lag2 + D0*PD - muP*tc(t)*sbi(B2(t))*P2 - D(N2,B2(t),t)*P2 - mm(t,W,Wt1)*P2 - etha2(t,sP)*P2
    #p3
    dn9dt= G2(N3, t)*saf(B3(t))*P3 - muj(N3, t)*p3 - e(N3, t)*p3 - mm(t,W,Wt1)*p3 - etha3(t,sN)*p3
    #delay for P3
    lag3= ifelse((x(t)-tau(t))<0, 0, e(lagvalue(t-tau(t),3), t-tau(t))*lagvalue(t-tau(t),9))
    #P3
    dn10dt= lag3 + D0*PD - muP*tc(t)*sbi(B3(t))*P3 - D(N3, B3(t),t)*P3 - mm(t,W,Wt1)*P3 - etha3(t,sP)*P3
    #PD
    dn11dt= h0(t)*P0 + D(N1,B1(t),t)*P1*alpha1 + D(N2,B2(t),t)*P2*alpha2 + D(N3,B3(t),t)*P3*alpha3 - h1(t)*PD - D0*PD - muPD*tc(t)*PD - mm(t,W,Wt1)*PD
    
    list(c(dn1dt, dn2dt, dn3dt, dn4dt, dn5dt, dn6dt, dn7dt, dn8dt, dn9dt, dn10dt, dn11dt))
  })
}


#PARAMETER, value, description, units
#Switches (1=present, 0=absent) 
SA1=1         #Spring aphids habitat 1 
AA1=1         #Autumn aphids habitat 1
A2=1          #Aphids habitat 2
A3=1          #Aphids habitat 3

F1=1         #Spring flowers habitat 1
F2=1          #Flowers habitat 2 
F3=1          #Flowers habitat 3 

I2=0          #Mortality event habitat 2
I3=0          #Mortality event habitat 3

#Temperature parameters
Ta=7.7        #Amplitude of the temperature cosine function, degrees Celsius
td=202-90     #Day at which the temperature is at its highest, day
Tc=10.7       #Average temperature over season, degrees Celsius

##Habitat and seasonal resource parameters
c=0.5                   #Constant to vary habitat fractions of habitat 2 & 3 relative to habitat 1
alpha1=0.05             #Proportion habitat 1 (woody habitat) in landscape, proportion
alpha2=(1-alpha1)*c     #Proportion habitat 2 (winter wheat and flower margin) in landscape, proportion
alpha3=(1-alpha1)*(1-c) #Proportion habitat 3 (potato and flower margin) in landscape, proportion

# Floral resources
B23l=0.02       #Background level floral resources in habitat 2 and 3 (crop fields), proportion
B1l=0.05        #Background level floral resources in habitat 1 (woody habitat), proportion
ks=2            #Shape Weibull distribution of flowering and aphid growth, -
k1=2.8          #Shape Weibull distribution of flowering of woody habitat, -
B1m=0.25        #Maximum level floral resources habitat 1 in spring, proportion
T1s=-7          #Start day flowering habitat 1 in spring, day
T1d=53          #Duration flowering habitat 1 in spring, days
B2m=0.49        #Maximum level floral resources habitat 2, proportion
T2s=59          #Start day flowering habitat 2, day
T2d=73          #Duration flowering habitat 2, days
B3m=0.49        #Maximum level floral resources habitat 3, proportion
T3s=59          #Start day flowering habitat 3, day
T3d=73          #Duration flowering habitat 3, days   

# Aphid resources
n1s=1.15        #Maximum level aphid resource supply in spring in habitat 1 
n1a=0.8625      #Maximum level aphid resource supply in autumn in habitat 1
A1s=-19         #Start day aphid growth habitat 1 in spring, day
A1d=56          #Duration aphid growth in habitat 1 in spring, days
A1as=150        #Start day aphid growth in habitat 1 in autumn, day
A1ad=36         #Duration aphid growth in habitat 1 in autumn, days
A2f=120         #Final day aphid growth habitat 2, days
A2d=65          #Duration aphid growth habitat 2, days
A3f=180         #Final day aphid growth habitat 3, days
A3d=53          #Duration aphid growth habitat 3, days

#Aphid parameters
phi=1e-5        #Small corrective value for numerical stability (to prevent K2 and K3 to reach 0 and thus preventing division by 0 in logistic growth of aphids)
K1=10000        #Maximum aphid density (carrying capacity) in habitat 1, aphids/m2
Km=20000        #Maximum aphid density (carrying capacity) in habitat 2 & 3, aphids/m2
rN1=0.38        #Aphid maximum intrinsic rate of increase in habitat 1 at 22C, day^-1
rN2=0.35        #Aphid maximum intrinsic rate of increase in habitat 2 at 22C, day^-1
rN3=0.32        #Aphid maximum intrinsic rate of increase in habitat 3 at 22C, day^-1
mu1=0.08        #Aphid death rate due to biotic and abiotic factors in habitat 1, day^-1
mu2=0.08        #Aphid death rate due to biotic and abiotic factors in habitat 2, day^-1
mu3=0.08        #Aphid death rate due to biotic and abiotic factors in habitat 3, day^-1


#Hoverfly parameters
em=1/13         #Maximum development rate hoverfly, 1/developmental period in days
G1m=25          #Maximum reproduction rate hoverfly in habitat 1, eggs female^-1 day^-1
G2m=30          #Maximum reproduction rate hoverfly in habitat 2 & 3, eggs female^-1 day^-1
fm=40           #Maximum per capita predation rate, aphids*hoverfly^-1*day^-1   
Nf=200          #Half saturation density functional response, aphids/m2
Ne=Nf/2         #Half saturation density hoverfly juvenile development, aphids/m2
Ng=Nf/2         #Half saturation density ovipositional/numerical response, aphids/m2
mu0=0.03        #Mortality rate adult hoverflies in winter habitat, day^-1
muPD=0.12       #Mortality rate dispersing adult hoverflies, day^-1
muP=0.48        #Mortality rate for ill-fed adult hoverflies in all habitats, day^-1  
mum=0.01        #Mortality rate juvenile hoverflies at abundant aphids, day^-1
tauh=11         #Time delay due to hoverfly pupation and maturation, days

#Hoverfly distribution between habitats/states
H0=30           #Mean time of hoverfly emergence from hibernation, day
H1=183          #Mean time of hoverfly going into hibernation, day
sigma0=8        #Standard deviation of hoverfly emergence from hibernation, days
sigma1=9        #Standard deviation of hoverfly going into hibernation, days
D0=50           #Maximum dispersal rate (in absence of food)
epsilon=0.25    #Shape parameter of simple dispersal function
#Transition rates between subhabitats due to:
b=8             #Feeding when in flower habitat, depends on flower density, day^-1
a=0.6           #Energy use when in aphid habitat, day^-1
da=2            #Dispersal from flower to aphid habitat, day^-1
db=0.5          #Dispersal from aphid to flower habitat, day^-1

#Seasonal mortality
WN=0.5          #Winter survival aphids in habitat 1, proportion
WP=0.3          #Winter survival adult hoverflies in hibernation, proportion
W=1e-10         #Survival seasonal events, proportion
Wt1=209         #Time of winter mortality event, day
Wt2=A2f         #Time of harvest winter wheat habitat 2, day
Wt3=A3f         #Time of haulm killing potato habitat 3, day

#Mortality event parameters
Is2=120         #Start day mortality event habitat 2, day
Is3=170         #Start day mortality event habitat 3, day
sN=0.05         #Survival aphids and hoverfly larvae after mortality event, proportion
sP=0.5          #Survival adult hoverflies after mortality event, proportion

#Initial population densities
Pi=0.02         #Hoverflies (into hibernating population, P0)
N1i=0.8         #Aphids habitat 1, aphids/m2
N2i=0.0015      #Yearly infestation aphids habitat 2, aphids/m2  
N3i=0.0015      #Yearly infestation aphids habitat 3, aphids/m2 
T2i=1           #Time of infestation aphids habitat 2, day 
T3i=55          #Time of infestation aphids habitat 3, day 


parms=c(SA1=SA1, AA1=AA1, A2=A2, A3=A3, F1=F1, F2=F2, F3=F3, I2=I2, I3=I3, Ta=Ta, td=td, Tc=Tc, c=c, alpha1=alpha1, 
        alpha2=alpha2, alpha3=alpha3, B23l=B23l, ks=ks, k1=k1, A1s=A1s, A1as=A1as, A1d=A1d, A1ad=A1ad, B1l=B1l, B1m=B1m, T1s=T1s, T1d=T1d, 
        B2m=B2m, T2s=T2s, T2d=T2d, B3m=B3m, T3s=T3s, T3d=T3d, n1s=n1s, n1a=n1a, A2f=A2f, A2d=A2d, A3f=A3f, A3d=A3d, phi=phi, K1=K1, Km=Km, 
        T2i=T2i, T3i=T3i, Nf=Nf, Ne=Ne, Ng=Ng, G1m=G1m,G2m=G2m, fm=fm, mu0=mu0, muPD=muPD, muP=muP, mum=mum, rN1=rN1,
        rN2=rN2, rN3=rN3, mu1=mu1, mu2=mu2, mu3=mu3, WN=WN, WP=WP, W=W, Wt1=Wt1, Wt2=Wt2, Wt3=Wt3, tauh=tauh, H0=H0, H1=H1, sigma0=sigma0, sigma1=sigma1, epsilon=epsilon,
        b=b, a=a, da=da, db=db, Is2=Is2, Is3=Is3, sN=sN, sP=sP, Pi=Pi, N1i=N1i, N2i=N2i, N3i=N3i, T2i=T2i, T3i=T3i)

#Years
#Run with 20 years takes approximately 5 minutes to run
years=20
times=seq(0,210*years)

#INITIAL CONDITIONS
initialN=c(N1i, 0, 0, Pi, 0, 0, 0, 0, 0, 0, 0)

#SIMULATION
out=dede(y=initialN, times=times, func=lvpred, parms=parms, control=list(mxhist = 1e6))

end_time=Sys.time()
end_time-start_time


#Creating data frame of output
data_frame_PopDyn = data.frame(timestep=out[,1], N1=out[,2], N2=out[,3], N3=out[,4], P0=out[,5], p1=out[,6], P1=out[,7], p2=out[,8], P2=out[,9], p3=out[,10], P3=out[,11], PD=out[,12])

#Set working directory 
#setwd("")
#Save data frame
#saveRDS(data_frame_PopDyn, file="AgroLandscape-PestControl.RData")
