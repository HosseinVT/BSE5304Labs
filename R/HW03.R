rm(list=objects())
# 
# Since everything depends on the libraries you install
# it is worthwhile loading them at the beginning
#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,parallel, htsr,hrbrthemes,ggeasy,scales,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
LabNo="/Lab03"
#
# Getting our organization on for where we want to put
# Data, external programs, and our project files.
# Things are going to get messy if we don't start issolating
# our data files by Lab
#
myhomedir=Sys.getenv("HOME")
datadir=paste0(myhomedir,"/data",LabNo)
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)

# Setting the directory for where the GitHub project exists. 
# This depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in... or if later in the project:
# WOOO HOOO... took me a few hours to find this function!
# 
mygitdir=rstudioapi::getActiveProject()
mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
dir.create(mypdfdir)
# 
setwd(mygitdir)
system("git config --global user.email 'h.ahmadi@vt.edu' ") 
system("git config --global user.name 'HosseinVT' ")
system("git config pull.rebase false")
#
# This was already done before, and doesn't need to be repeated unless there
# is an update to R or the EcoHydRology Package... but 
#
setwd(srcdir)
system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)

setwd(datadir)

#myflowgage_id="0205551460"
# My hometown flowgage id = 01658500

myflowgage_id="14216500"


myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2018-01-01",
                         end_date = "2023-01-01")
#
# This is where some folks had issues... they forgot to check their 
# watershed areas per the homework... though there were ways to fix
# it later with lower resolution DEM pull
#
print(myflowgage$area)
# For most watershed modelling purposes we normalize Q in mm/day for basins
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3

# In the Lab02, we introduced you to a way to quickly get your WX Data 
# for any location in the world way easier than traditional download and
# parsing methods most old people use.
#
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=150,minstns=2,date_min="2018-01-01",
                  date_max="2023-01-01",targElev=1,
                  method = "closest",alfa=2)

BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")
#
# Setting the projection information for the specific location
#

TMWB=BasinData

#######################################  Tsno ,SNO  ,SNOmlt  ###################################
SFTmp = 3.  # referred to as SFTMP in SWAT input (Table 1)
bmlt6 = 2.  # referred to as SMFMX in SWAT input (Table 1)
bmlt12 = 0.0  # referred to as SMFMN in SWAT input adjusted for season
Tmlt = SFTmp  # Assumed to be same as SnowFall Temperature
Tlag = 1  # referred to as TIMP in SWAT input (Table 1)
TMWB$AvgTemp=(TMWB$MaxTemp-TMWB$MinTemp)/2
TMWB$bmlt = (bmlt6 + bmlt12)/2 + (bmlt6 - bmlt12)/2 *  sin(2*pi/365*(julian(TMWB$date,origin = as.Date("2000-01-01"))-81))
# Initialize SNO, Tsno as well as the first values of each
TMWB$SNO = 0  # Snow Depth (mm)
TMWB$Tsno = 0  # Snow Temp (C)
TMWB$SNOmlt = 0  # Snow Melt (mm)

attach(TMWB)

for (t in 2:length(date)){
  Tsno[t]= Tsno[t-1] * (1.0-Tlag) +  AvgTemp[t] * Tlag
  if(AvgTemp[t] < SFTmp){
    SNO[t]= SNO[t-1] + P[t]
  }  else {
    SNOmlt[t]= bmlt[t] * SNO[t-1] * ((Tsno[t]+MaxTemp[t])/2 - Tmlt) 
    SNOmlt[t]= min(SNOmlt[t],SNO[t-1])
    SNO[t]= SNO[t-1] -SNOmlt[t]
  }
  print(t)
}
plot(date,SNO,type="l")

detach(TMWB)
detach(TMWB)

TMWB$Tsno=Tsno
TMWB$SNO=SNO
TMWB$SNOmlt=SNOmlt
rm(list=c("SNO", "SNOmlt", "Tsno"))

############################################# PET , ET , dp , Qmm ##########################################
TMWB$P = TMWB$P + 2   #  in order to get rid of this issue (negative values for ET which might be related to lapse rate) as the streamflow data are more reliable so we could increase the amount of the precipitation 
TMWB$PET = mean(TMWB$P,na.rm=T)-mean(TMWB$Qmm,na.rm=T)  # in mm/day 
TMWB$ET = TMWB$PET # in mm/day
TMWB$AWC=(0.45-0.15)*1000 #Fld Cap = .45, Wilt Pt = .15, z=1000mm
TMWB$dP = TMWB$P - TMWB$SNO + TMWB$SNOmlt - TMWB$ET

#####
attach(TMWB)# Remember to detach or it gets ugly

plot(date,Qmm,type = "l",col="black")
lines(date,P,type = "l",col="red")
lines(date,Qmm,type = "l",col="black") # We repeat to have Qmm on top of P
lines(date,ET,type = "l",col="blue")
legend("topright", c("P", "Qmm", "ET"), col = c("red", "black", "blue"),
       lty = 1:2, cex = 0.8)

detach(TMWB) # IMPORTANT TO DETACH
detach(TMWB) # IMPORTANT TO DETACH
dev.off()

colors <- c("P" = "blue", "Qmm" = "red", "ET" = "green")

ggplot(TMWB, aes(x=date)) +
  geom_line(aes( y=P, color = "P"),size = 0.3) +
  geom_line(aes( y=Qmm, color = "Qmm"),size = 0.3) +
  geom_line(aes( y=ET, color = "ET"),size = 0.2) +
  xlab("day")+
  ylab("Precipitation(mm/day)")+
  scale_color_manual("", values = colors)+
  labs(x = "", colour = "Legend")+
  scale_x_date(date_breaks = "3 months", expand = c(0.005,0.005))+
  scale_y_continuous(name = "Precipitation (mm/day) ",sec.axis = sec_axis(~.*1, 
                                                                      name="Qmm (mm/day)"))+
  ggtitle('MUDDY RIVER BELOW CLEAR CREEK NEAR COUGAR, WA')+
  theme(legend.position = "bottom",
        legend.justification = c("center"),
        legend.box.just = "left",
        legend.background = element_blank(),
        axis.text.x.bottom = element_text(vjust = 0,size=10, angle = 90 ),
        axis.text.y.left = element_text( ),
        plot.title = element_text(hjust = 0.5, vjust = 0, size=10),
        text=element_text(family="calibri light"),

        axis.ticks = element_blank()) 

#soildrying = ???
#soil_wetting = ???
#soil_wetting_above_capacity = ???

############################################ Aw , Excess ################################################
#soil_wetting function
soilwetting<-function(AWprev,dP_func,AWC_func){
  AW_func<-AWprev+dP_func
  excess_func<-0.0
  c(AW_func,excess_func)
} 

# soildrying function
soildrying<-function(AWprev,dP_func,AWC_func){
  AW_func=AWprev*exp(dP_func/AWC_func)
  excess_func<-0.0
  c(AW_func,excess_func)
}

# soil_wetting_above_capacity function
soil_wetting_above_capacity<-function(AWprev,dP_func,AWC_func){
  AW_func<-AWC_func
  excess_func<-AWprev+dP_func-AWC_func
  c(AW_func,excess_func)
}


TMWB$AWC=(0.45-0.15)*1000 #Fld Cap = .45, Wilt Pt = .15, z=1000mm


TMWB$AW=NA  #Assigns all values in column with “NA” (Not available)
TMWB$AW[1]=250
TMWB$Excess=NA
TMWB$Excess[1]=0
head(TMWB)


# Here we go looping through our functions….

attach(TMWB)

for (t in 2:length(date)){
  if (dP[t]< 0) {  
    values<-soildrying(AW[t-1],dP[t],AWC[t])
  } else if (AW[t-1]+dP[t]>AWC[t]) {
    values<-soil_wetting_above_capacity(AW[t-1],dP[t],AWC[t])
  } else {
    values<-soilwetting (AW[t-1],dP[t],AWC[t])
  }
  AW[t]<-values[1]
  Excess[t]<-values[2]
  
}
detach(TMWB)
detach(TMWB)

TMWB$AW <-AW
TMWB$Excess<-Excess
rm(list=c("AW","Excess"))

####

colors <- c("AW" = "darkblue", "Excess" = "coral4")
ggplot(TMWB, aes(x=date)) +
  geom_line(aes( y=AW, color = "AW"),size = 0.2) +
  geom_line(aes( y=Excess, color = "Excess"),size = 0.2) +
  xlab("")+
  ylab("AW(mm)")+
  scale_color_manual("", values = colors)+
  labs(x = "", colour = "Legend")+
  scale_x_date(date_breaks = "3 month", expand = c(0.005,0.005))+
  scale_y_continuous(name = "AW (mm/day) ",sec.axis = sec_axis(~.*1, name="Excess (mm/day)"))+
  ggtitle('MUDDY RIVER BELOW CLEAR CREEK NEAR COUGAR, WA')+
  theme(legend.position = "bottom",
        legend.justification = c("center"),
        legend.box.just = "left",
        legend.background = element_blank(),
        axis.text.x.bottom = element_text(vjust = 0,size=10, angle = 90 ),
        axis.text.y.left = element_text( ),
        plot.title = element_text(hjust = 0.5, vjust = 0, size=12),
        text=element_text(family="calibri light"),
        axis.ticks = element_blank()) 


attach(TMWB)# Remember to detach or it gets ugly

plot(date,Qmm,type = "l",col="black")
lines(date,P,type = "l",col="red")
lines(date,Qmm,type = "l",col="black") # We repeat to have Qmm on top of P
lines(date,ET,type = "l",col="blue")
lines(date,AW/10,type = "l",col="green")
lines(date,Excess/2,type = "p",col="cyan")

legend("topright", c("P", "Qmm", "ET" , "AW/10" , "Excess/2"), col = c("red", "black", "blue" , "green", "cyan"),
       lty = 1:2, cex = 0.8)



detach(TMWB) # IMPORTANT TO DETACH
detach(TMWB) # IMPORTANT TO DETACH

dev.off()

####################################################  Qpred , S ##################################
TMWB$Qpred=NA
TMWB$Qpred[1]=0
TMWB$S=NA
TMWB$S[1]=0

attach(TMWB)

fcres=.3  # reservoir coefficient
for (t in 2:length(date)){
  S[t]=S[t-1]+Excess[t]     
  Qpred[t]=fcres*S[t]
  S[t]=S[t]-Qpred[t]
}

detach(TMWB) # IMPORTANT TO DETACH
detach(TMWB) # IMPORTANT TO DETACH

TMWB$S=S
TMWB$Qpred=Qpred # UPDATE vector BEFORE DETACHING
rm(list=c("S","Qpred"))


#####
colors <- c("Qpred" = "blue", "S" = "darkgreen","Qmm" = "red")
ggplot(TMWB, aes(x=date)) +
  geom_line(aes( y=Qpred, color = "Qpred"),size = 0.4) +
  geom_line(aes( y=S, color = "S"),size = 0.25) +
  geom_line(aes( y=Qmm, color = "Qmm"),size = 0.4) +
  
  xlab("")+
  ylab("Qpred(mm/day)")+
  scale_color_manual("", values = colors)+
  labs(x = "", colour = "Legend")+
  scale_x_date(date_breaks = "3 months", expand = c(0.005,0.005))+
  scale_y_continuous(name = "Qpred (mm/day) ",sec.axis = sec_axis(~.*1, name="S (mm/day)"))+
  ggtitle('MUDDY RIVER BELOW CLEAR CREEK NEAR COUGAR, WA')+
  theme(legend.position = "bottom",
        legend.justification = c("center"),
        legend.box.just = "left",
        legend.background = element_blank(),
        axis.text.x.bottom = element_text(vjust = 0,size=10, angle = 90 ),
        axis.text.y.left = element_text( ),
        plot.title = element_text(hjust = 0.5, vjust = 0, size=12),
        text=element_text(family="calibri light"),
        panel.border = element_rect(fill= NA, color = "white",size = 0.3),
        panel.grid.major.y = element_line(colour = 'white', size = 0.3),
        panel.grid.major.x =element_line(colour = 'white', size = 0.3),
        panel.grid.minor.y =element_line(colour = 'white', size = 0.3),
        panel.grid.minor.x =element_line(colour = 'white', size = 0.3),
        axis.ticks = element_blank()) 




attach(TMWB)# Remember to detach or it gets ugly

plot(date,Qmm,type = "l",col="black")
lines(date,P,type = "l",col="red")
lines(date,Qmm,type = "l",col="black") # We repeat to have Qmm on top of P
lines(date,ET,type = "l",col="blue")
lines(date,AW/10,type = "l",col="green")
lines(date,Excess/2,type = "p",col="cyan")
lines(date,Qpred,type = "l",col="darkorchid")

legend("topright", c("P", "Qmm", "ET" , "AW/10" , "Excess/2" , "Qpred" ), col = c("red", "black", "blue" , "green", "cyan" , "darkorchid"),
       lty = 1:2, cex = 0.8)



detach(TMWB) # IMPORTANT TO DETACH
detach(TMWB) # IMPORTANT TO DETACH

data <- data.frame(
  day=TMWB$date,Qmm=TMWB$Qmm,ET=TMWB$ET,P=TMWB$P,AW=TMWB$AW,Excess=TMWB$Excess,
  Qpred=TMWB$Qpred)

View(data)
p <- ggplot(data, aes(x=day)) +
  
  #geom_line( aes(y=TMWB$Qpred, color="Qpred"), size=0.2)+
  #geom_line( aes(y=Qmm, color="Qmm"), size=0.3) + 
  #geom_line( aes(y=ET, color="ET"), size=0.2) + 
  geom_line( aes(y=Excess, color="Excess"), size=0.3)+
  geom_line( aes(y=AW/5, color="AW/10"), size=0.3)+
  geom_bar(aes(y=ET, fill = "ET (mm/day)"), stat="identity")+
  #geom_bar(aes(y=P, fill = "P (mm/day)"), stat="identity")+
  
  coord_cartesian(ylim=c(0,80))+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Excess (mm/day)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.5, name="AW/10 (mm/day)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = "black", size=10),
    axis.title.y.right = element_text(color = "black", size=10)
  ) +
  
  ggtitle("") +
  ggeasy::easy_center_title() +
  
  scale_colour_manual(name ="", values = c("Qmm"="coral2", "ET"="cyan3", "Qpred"="blue" , "AW/10"="green", "Excess"="darkviolet" ))+
  scale_fill_manual  (name ="", values = c("P (mm/day)"="darkolivegreen4", "ET (mm/day)"="cyan3" ))

plot (p)

#########################################  Calibration  ###################################

#######################################  Tsno ,SNO  ,SNOmlt  ###################################
TMWB=BasinData

SFTmp = 1  # referred to as SFTMP in SWAT input (Table 1)
bmlt6 = 7  # referred to as SMFMX in SWAT input (Table 1)
bmlt12 = 7 # referred to as SMFMN in SWAT input adjusted for season
Tmlt = SFTmp  # Assumed to be same as SnowFall Temperature
Tlag = 1  # referred to as TIMP in SWAT input (Table 1)
TMWB$AvgTemp=(TMWB$MaxTemp-TMWB$MinTemp)/2
TMWB$bmlt = (bmlt6 + bmlt12)/2 + (bmlt6 - bmlt12)/2 *  sin(2*pi/365*(julian(TMWB$date,origin = as.Date("2000-01-01"))-81))
# Initialize SNO, Tsno as well as the first values of each
TMWB$SNO = 0  # Snow Depth (mm)
TMWB$Tsno = 0  # Snow Temp (C)
TMWB$SNOmlt = 0  # Snow Melt (mm)

attach(TMWB)

for (t in 2:length(date)){
  Tsno[t]= Tsno[t-1] * (1.0-Tlag) +  AvgTemp[t] * Tlag
  if(AvgTemp[t] < SFTmp){
    SNO[t]= SNO[t-1] + P[t]
  }  else {
    SNOmlt[t]= bmlt[t] * SNO[t-1] * ((Tsno[t]+MaxTemp[t])/2 - Tmlt) 
    SNOmlt[t]= min(SNOmlt[t],SNO[t-1])
    SNO[t]= SNO[t-1] -SNOmlt[t]
  }
  print(t)
}
plot(date,SNO,type="l")

detach(TMWB)
detach(TMWB)

TMWB$Tsno=Tsno
TMWB$SNO=SNO
TMWB$SNOmlt=SNOmlt
rm(list=c("SNO", "SNOmlt", "Tsno"))

############################################# PET , ET , dp , Qmm ##########################################
TMWB$P = TMWB$P + 2   #  in order to get rid of this issue (negative values for ET which might be related to lapse rate) as the streamflow data are more reliable so we could increase the amount of the precipitation 
TMWB$PET = mean(TMWB$P,na.rm=T)-mean(TMWB$Qmm,na.rm=T)  # in mm/day 
TMWB$ET = TMWB$PET # in mm/day
TMWB$AWC=(0.45-0.1)*1000 #Fld Cap = .45, Wilt Pt = .15, z=1000mm
TMWB$dP = TMWB$P - TMWB$SNO + TMWB$SNOmlt - TMWB$ET



############################################ Aw , Excess ################################################
#soil_wetting function
soilwetting<-function(AWprev,dP_func,AWC_func){
  AW_func<-AWprev+dP_func
  excess_func<-0.0
  c(AW_func,excess_func)
} 

# soildrying function
soildrying<-function(AWprev,dP_func,AWC_func){
  AW_func=AWprev*exp(dP_func/AWC_func)
  excess_func<-0.0
  c(AW_func,excess_func)
}

# soil_wetting_above_capacity function
soil_wetting_above_capacity<-function(AWprev,dP_func,AWC_func){
  AW_func<-AWC_func
  excess_func<-AWprev+dP_func-AWC_func
  c(AW_func,excess_func)
}


TMWB$AWC=(0.45-0.1)*1000 #Fld Cap = .45, Wilt Pt = .15, z=1000mm


TMWB$AW=NA  #Assigns all values in column with “NA” (Not available)
TMWB$AW[1]=250
TMWB$Excess=NA
TMWB$Excess[1]=0
head(TMWB)


# Here we go looping through our functions….

attach(TMWB)

for (t in 2:length(date)){
  if (dP[t]< 0) {  
    values<-soildrying(AW[t-1],dP[t],AWC[t])
  } else if (AW[t-1]+dP[t]>AWC[t]) {
    values<-soil_wetting_above_capacity(AW[t-1],dP[t],AWC[t])
  } else {
    values<-soilwetting (AW[t-1],dP[t],AWC[t])
  }
  AW[t]<-values[1]
  Excess[t]<-values[2]
  
}
detach(TMWB)
detach(TMWB)

TMWB$AW <-AW
TMWB$Excess<-Excess
rm(list=c("AW","Excess"))



####################################################  Qpred , S ##################################
TMWB$Qpred=NA
TMWB$Qpred[1]=0
TMWB$S=NA
TMWB$S[1]=0

attach(TMWB)

fcres=.3  # reservoir coefficient
for (t in 2:length(date)){
  S[t]=S[t-1]+Excess[t]     
  Qpred[t]=fcres*S[t]
  S[t]=S[t]-Qpred[t]
}

detach(TMWB) # IMPORTANT TO DETACH
detach(TMWB) # IMPORTANT TO DETACH

TMWB$S=S
TMWB$Qpred=Qpred # UPDATE vector BEFORE DETACHING
rm(list=c("S","Qpred"))


NSE=function(Yobs,Ysim){
  return(1-sum((Yobs-Ysim)^2,na.rm=TRUE)/sum((Yobs-mean(Yobs, na.rm=TRUE))^2, na.rm=TRUE))
}

NSEValue=NSE(TMWB$Qmm,TMWB$Qpred)
NSEValue





