rm(list=objects()) # Removes ALL the objects… so be careful here.
# What is going to change from use case to use case 
Sys.getenv('USER')
LabNo="/Lab09"
#
# What needs to be loaded
#
if (!require("pacman")) install.packages("pacman")
myhomedir=Sys.getenv("HOME")
datadir=paste0(myhomedir,"/data",LabNo)
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)
# Setting the directory for where the GitHub project exists. 
# This depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in... or if later in the project:
# 
mygitdir=rstudioapi::getActiveProject()
mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
dir.create(mypdfdir)
# 
setwd(mygitdir)
system("git config --global user.email 'hosseinahmadi@vt.edu' ") 
system("git config --global user.name 'HosseinVT' ")
system("git config pull.rebase false")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr,EcoHydRology,curl,elevatr,raster,rgdal,
               data.table,foreign,maptools,dataRetrieval,gdistance)
setwd(datadir)
#
# Note we have a new library to access USGS Waterdata
# https://owi.usgs.gov/R/dataRetrieval.html
# https://owi.usgs.gov/R/training-curriculum/usgs-packages/dataRetrieval-readNWIS/
#
##?dataRetrieval  # Review the man page for this package
##?readNWISuv
##?readNWISdv
##?readNWISdata
#
#
# Need to figure out which data to download. 
# https://nwis.waterdata.usgs.gov/nwis/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&pm_search=&casrn_search=&srsname_search=&format=html_table&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units
# 
##url="https://nwis.waterdata.usgs.gov/nwis/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&pm_search=&casrn_search=&srsname_search=&format=html_table&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units"
##browseURL(url)
#
# Yeah, these databases are complex to get to know, remember our 
# SSURGO?
#
# Before you begin your modeling project, confirm what your model outputs 
# has a value to calibrate against, i.e. match parameter and units. For 
# this lab we are looking for Gage Height, while historically, we have been 
# looking at Discharge. NOT ALL PARAMETERS ARE AVAILABLE!
#
##url="https://help.waterdata.usgs.gov/parameter_cd?group_cd=%"
##browseURL(url)
##View(parameterCdFile)

##############################################
# 0205551460 LICK RUN ABOVE PATTON AVENUE AT ROANOKE, VA
##############################################

make_usgs_gage_list = function (
    siteNo = "0205551460",
    parameterCd = c("00060","00065"),
    start.date = "2017-05-01",  # Not frozen to not frozen
    end.date = "2017-11-01"    # to still not frozen
)
{
  
  #
  # For each gage location, let's keep the data organized as a 
  # list.
  USGS=list()   # Organize the data in a nice list as in previous labs
  USGS[["flowdata"]]<- readNWISuv(siteNumbers = siteNo,parameterCd = parameterCd,startDate = start.date,endDate = end.date)
  #View(USGS$flowdata)
  #head(USGS$flowdata)  # Note that we have 00060 and 00065...
  #USGS[["flowdata"]]<- get_usgs_gage(flowgage_id = siteNo, begin_date = start.date, end_date = end.date)
  
  #  agency_cd	site_no        	dateTime X_00060_00000 X_00060_00000_cd
  #1  	USGS 0205551460 2017-05-01 04:00:00      	6.38            	A
  #2  	USGS 0205551460 2017-05-01 04:05:00      	6.38            	A
  #  X_00065_00000 X_00065_00000_cd tz_cd
  #1      	2.74            	A   UTC
  #2      	2.74            	A   UTC
  #
  # And of course we want to work in SI units so:
  USGS$flowdata$depth_m=USGS$flowdata$X_00065_00000*0.3048
  # m/ft depth
  USGS$flowdata$cms=USGS$flowdata$X_00060_00000*.02832
  # m3/ft3 flow
  #
  # Let's add in the USGS gage site information to the list and inspect
  USGS[["site"]]=readNWISsite(siteNo)
  #View(USGS$site)
  #head(USGS$site)
  #class(USGS$site$dec_lat_va)
  #
  # Set the Manning Coefficient in the USGS Gage's Site Table
  #
  #url="https://www.google.com/search?q=manning%27s+n+for+stream"
  #browseURL(url)
  #url="https://www.fsl.orst.edu/geowater/FX3/help/8_Hydraulic_Reference/Mannings_n_Tables.htm"
  #browseURL(url)
  USGS$site$man_n=.035/1.49
  #
  # Create a SpatialPointsDataFrame out of the site dataframe in the USGS list
  coordinates(USGS$site)=~dec_long_va+dec_lat_va
  return(USGS)
}

#testlist = make_usgs_gage_list()
#pacman :: p_load(usefull)
#compare.list (testlist,USGS02055000)


USGS02056000   = make_usgs_gage_list(siteNo = "02056000" )
USGS0205551460 = make_usgs_gage_list(siteNo = "0205551460" )
USGS02055100   = make_usgs_gage_list(siteNo = "02055100" )
USGS02055000   = make_usgs_gage_list(siteNo = "02055000" )
USGS02054530   = make_usgs_gage_list(siteNo = "02054530" )

ab_ll=rbind(USGS02056000$site,
            USGS0205551460$site,
            USGS02055100$site,
            USGS02055000$site,
            USGS02054530$site)
class(ab_ll)
ab_ll@proj4string
proj4_utm = paste0("+proj=utm +zone=",
                   trunc((180+coordinates(USGS02055000$site)[1])/6+1), 
                   " +datum=WGS84 +units=m +no_defs")
print(proj4_utm)
# Lat/Lon (_ll) is much easier!
proj4_ll = "+proj=longlat"
crs_ll=CRS(proj4_ll)
crs_utm=CRS(proj4_utm)
proj4string(ab_ll)=proj4_ll
ab_utm=spTransform(ab_ll,crs_utm)
ab_utm@coords
mydem=get_aws_terrain(locations=ab_utm@coords, 
                      z = 12, prj = proj4_utm,expand=1)
#
# Lets plot the DEM and the gage locations so we can guess 
# what gages connect with what gages
#
plot(mydem)
plot(ab_utm,add=T)
text(ab_utm, labels=ab_utm@data$site_no, cex=0.6, font=2,pos=1)
# Streams as USGS sees them, I know I can get an overview of streams with the 
# USGS H
url="https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/Shape/NHD_H_03010101_HU8_Shape.zip"
#browseURL(url)
curl_download(url,"NHD_H_03010101_HU8_Shape.zip")
unzip("NHD_H_03010101_HU8_Shape.zip",exdir="03010101")
streams=readOGR("03010101/Shape/NHDFlowline.dbf")
streams_utm=spTransform(streams,crs_utm)
plot(streams_utm,col="blue",add=T)

#
# Breaking things for educational purposes
View(USGS02056000$flowdata)
USGS02056000$flowdata=USGS02056000$flowdata[,c(1,2,3,4,5,8,10)]
View(USGS02056000$flowdata)
# Oh Noooooo!!!! This gage for some reason doesn't have "Gage height"
# 00065! What can we do!?!? OK, no worries, we do have "Discharge" 00060	
###########################################
# 02056000 ROANOKE RIVER AT NIAGARA, VA
###########################################
# and use the readNWISrating() function to grab it for this gage
head(USGS02056000$flowdata,2)

USGS02056000[["rating"]]=readNWISrating(USGS02056000$site$site_no)
plot(USGS02056000$rating$DEP,USGS02056000$rating$INDEP,xlab="DEP",ylab="INDEP")
#
USGS02056000$flowdata$X_00065_00000=approx(USGS02056000$rating$DEP,
                                           USGS02056000$rating$INDEP, xout = USGS02056000$flowdata$X_00060_00000, ties = min)$y
points(USGS02056000$flowdata$X_00060_00000,USGS02056000$flowdata$X_00065_00000,
       col="red")
#
USGS02056000$flowdata$depth_m=USGS02056000$flowdata$X_00065_00000*0.3048
# m/ft depth
#
###########################################
# 0205551460  LICK RUN ABOVE PATTON AVENUE AT ROANOKE, VA
###########################################
# and use the readNWISrating() function to grab it for this gage

head(USGS0205551460$flowdata)

#USGS0205551460[["rating"]]=readNWISrating(USGS0205551460$site$site_no)
#plot(USGS0205551460$rating$DEP,USGS0205551460$rating$INDEP,xlab="DEP",ylab="INDEP")
#
#USGS0205551460$flowdata$X_00065_00000=approx(USGS0205551460$rating$DEP,
                                           #USGS0205551460$rating$INDEP, xout = USGS0205551460$flowdata$X_00060_00000, ties = min)$y
#points(USGS0205551460$flowdata$X_00060_00000,USGS0205551460$flowdata$X_00065_00000,
 #      col="red")
#
#USGS0205551460$flowdata$depth_m=USGS0205551460$flowdata$X_00065_00000*0.3048
# m/ft depth
##########################################################################################################
# Correction for stage-discharge rating curve
USGS0205551460[["rating"]]=readNWISrating(USGS0205551460$site$site_no)
plot(USGS0205551460$rating$DEP,USGS0205551460$rating$INDEP,xlab="DEP",ylab="INDEP")
USGS0205551460$flowdata$X_00065_00000=approx(USGS0205551460$rating$DEP,
                                             USGS0205551460$rating$INDEP, xout = USGS0205551460$flowdata$X_00060_00000, ties = min)$y
USGS0205551460$flowdata$X_00065_00000 = USGS0205551460$flowdata$X_00065_00000 -0.8
points(USGS0205551460$flowdata$X_00060_00000,USGS0205551460$flowdata$X_00065_00000,
       col="red")
plot(USGS0205551460$flowdata$X_00060_00000,USGS0205551460$flowdata$X_00065_00000,xlab="Discharge (cms)",ylab="Depth (m)")
title(main = 'USGS0205551460 ')

USGS0205551460$flowdata$depth_m=USGS0205551460$flowdata$X_00065_00000*0.3048




#vignette("Overview", package = "gdistance")
# Set the starting and ending locations
# determine the river reach length and slope using the gdistance package.
#
A=SpatialPoints(USGS0205551460$site)# Up gradient site Lick Run
B=SpatialPoints(USGS02056000$site) # Down gradient site ROA River atNiagara
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
cropmydem=crop(mydem,extend(extent(ab_utm),600))
cropmydem=trim(cropmydem)
cropmydem=cropmydem*1000.0
plot(cropmydem)
plot(ab_utm,add=T)
# Set up the weighting functions
altDiff <- function(x){x[2] - x[1]}
hd<- transition(cropmydem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
# Find and plot the flow path
AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
plot(AtoB,add=T)
plot(streams_utm,col="blue",add=T)
plot(AtoB,add=T)
SpatialLinesLengths(AtoB)
USGS0205551460$site$L=SpatialLinesLengths(AtoB) # km to m
USGS0205551460$site$L # reach length in m
USGS0205551460$site$slope=(raster::extract(mydem,A_utm)-
                             raster::extract(mydem,B_utm))/USGS0205551460$site$L
USGS0205551460$site$slope

# So now we have flow depth (y "$depth_m"), manning's n ("$man_n"), Q ("$cms"), and slope ("$slope") rearrange to solve for B
# B=(n*Q)/(y^(5/3)*sqrt(So)
USGS0205551460$flowdata$B=(USGS0205551460$site$man_n*
                             USGS0205551460$flowdata$cms)/(USGS0205551460$flowdata$depth_m^(5/3)*
                                                             sqrt(USGS0205551460$site$slope))
head(USGS0205551460$flowdata)
# Lets look at how B changes with flow.    
plot(USGS0205551460$flowdata$dateTime,USGS0205551460$flowdata$B, main="LICK RUN TO ROANOKE RIVER AT NIAGARA, VA",
     xlab = 'Time', ylab='flow width (m)')
# Does this seem reasonable (...like order of magnitude reasonable)? You can 
# perform a quick and dirty check using google earth and measuring the channel 
# width in a few places.
#
plot(USGS0205551460$flowdata$cms,USGS0205551460$flowdata$depth_m, main="LICK RUN TO ROANOKE RIVER AT NIAGARA, VA",
     xlab = 'discharge (cms)', ylab='depth (m)')




# ck
USGS0205551460$flowdata$ck = 5/3*((sqrt(USGS0205551460$site$slope))/USGS0205551460$site$man_n) * USGS0205551460$flowdata$depth_m^(2/3)

# ANS
mean(USGS0205551460$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
USGS0205551460$flowdata$dt = USGS0205551460$site$L/USGS0205551460$flowdata$ck
mean(USGS0205551460$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result


plot(USGS0205551460$flowdata$dateTime,USGS0205551460$flowdata$dt, main="LICK RUN TO ROANOKE RIVER AT NIAGARA, VA",
     xlab = 'Time', ylab=' time of travel (s)')
USGS0205551460$flowdata$outTime=USGS0205551460$flowdata$dateTime+USGS0205551460$flowdata$dt

# Find the beginning of  Waves assuming a new wave starts at 110% of prior 
# flow. This might need to change for your homework
WaveStartDecPercent=1.10
USGS0205551460$flowdata$newwave=
  USGS0205551460$flowdata$cms * WaveStartDecPercent <
  data.table::shift(USGS0205551460$flowdata$cms)
summary(USGS0205551460$flowdata$newwave)
# Add plot of the point found
len=length(USGS0205551460$flowdata$newwave)
USGS0205551460$flowdata$newwave[is.na(USGS0205551460$flowdata$newwave)]=F
# Removes repeated finds by going through loop backwords
for (i in seq(len,2)){
  print(i)
  if(USGS0205551460$flowdata$newwave[i]==T &
     USGS0205551460$flowdata$newwave[i-1]==T){
    USGS0205551460$flowdata$newwave[i]=F
  }
}
plot(USGS0205551460$flowdata$dateTime,USGS0205551460$flowdata$cms,type="l",
     main="LICK RUN TO ROANOKE RIVER AT NIAGARA, VA",
     xlab = 'Time', ylab=' discharge (cms)')
points(USGS0205551460$flowdata$dateTime[USGS0205551460$flowdata$newwave],
       USGS0205551460$flowdata$cms[USGS0205551460$flowdata$newwave],col=2)

# Find the time locations where waves begin
which(USGS0205551460$flowdata$newwave == TRUE)
plot(USGS0205551460$flowdata$dateTime,USGS0205551460$flowdata$cms,
     type="l",main="LICK RUN TO ROANOKE RIVER AT NIAGARA, VA",
     xlab = 'Time', ylab=' discharge (cms)',
     xlim=c(USGS0205551460$flowdata$dateTime[1109],
                     USGS0205551460$flowdata$dateTime[1109+200]))
lines(USGS0205551460$flowdata$outTime,USGS0205551460$flowdata$cms,col=2)

#1109
#1109+200



##############################################################################################################
#USGS 02055100 TINKER CREEK NEAR DALEVILLE, VA
##############################################################################################################
A=SpatialPoints(USGS02055100$site)# Up gradient site Lick Run
B=SpatialPoints(USGS02056000$site) # Down gradient site ROA River atNiagara
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
cropmydem=crop(mydem,extend(extent(ab_utm),600))
cropmydem=trim(cropmydem)
cropmydem=cropmydem*1000.0
plot(cropmydem)
plot(ab_utm,add=T)
# Set up the weighting functions
altDiff <- function(x){x[2] - x[1]}
hd<- transition(cropmydem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
# Find and plot the flow path
AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
plot(AtoB,add=T)
plot(streams_utm,col="blue",add=T)
plot(AtoB,add=T)
SpatialLinesLengths(AtoB)
USGS02055100$site$L=SpatialLinesLengths(AtoB) # km to m
USGS02055100$site$L # reach length in m
USGS02055100$site$slope=(raster::extract(mydem,A_utm)-
                             raster::extract(mydem,B_utm))/USGS02055100$site$L
USGS02055100$site$slope

# So now we have flow depth (y "$depth_m"), manning's n ("$man_n"), Q ("$cms"), and slope ("$slope") rearrange to solve for B
# B=(n*Q)/(y^(5/3)*sqrt(So)
USGS02055100$flowdata$B=(USGS02055100$site$man_n*
                             USGS02055100$flowdata$cms)/(USGS02055100$flowdata$depth_m^(5/3)*
                                                             sqrt(USGS02055100$site$slope))
head(USGS02055100 $flowdata)
# Lets look at how B changes with flow.    
plot(USGS02055100$flowdata$dateTime,USGS02055100 $flowdata$B, main="TINKER CREEK NEAR DALEVILLE, VA",
     xlab = 'Time', ylab='flow width (m)')
# Does this seem reasonable (...like order of magnitude reasonable)? You can 
# perform a quick and dirty check using google earth and measuring the channel 
# width in a few places.
#
plot(USGS02055100$flowdata$cms,USGS02055100$flowdata$depth_m, main="TINKER CREEK NEAR DALEVILLE, VA",
     xlab = 'discharge (cms)', ylab='depth (m)')


# ck
USGS02055100$flowdata$ck = 5/3*((sqrt(USGS02055100$site$slope))/USGS02055100$site$man_n) * USGS02055100$flowdata$depth_m^(2/3)

# ANS
mean(USGS02055100$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
USGS02055100$flowdata$dt = USGS02055100$site$L/USGS02055100$flowdata$ck
mean(USGS02055100$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result


plot(USGS02055100$flowdata$dateTime,USGS02055100$flowdata$dt,main="TINKER CREEK NEAR DALEVILLE, VA",
     xlab = 'Time', ylab=' time of travel (s)')
plot(USGS02055100$flowdata$dateTime,USGS02055100$flowdata$ck,main="TINKER CREEK NEAR DALEVILLE, VA",
     xlab = 'Time', ylab=' kinematic celerity (m/s)')
USGS02055100$flowdata$outTime=USGS02055100$flowdata$dateTime+USGS02055100$flowdata$dt

# Find the beginning of  Waves assuming a new wave starts at 110% of prior 
# flow. This might need to change for your homework
WaveStartDecPercent=1.10
USGS02055100$flowdata$newwave=
  USGS02055100$flowdata$cms * WaveStartDecPercent <
  data.table::shift(USGS02055100$flowdata$cms)
summary(USGS02055100$flowdata$newwave)
# Add plot of the point found
len=length(USGS02055100$flowdata$newwave)
USGS02055100$flowdata$newwave[is.na(USGS02055100$flowdata$newwave)]=F
# Removes repeated finds by going through loop backwords
for (i in seq(len,2)){
  print(i)
  if(USGS02055100$flowdata$newwave[i]==T &
     USGS02055100$flowdata$newwave[i-1]==T){
    USGS02055100$flowdata$newwave[i]=F
  }
}
plot(USGS02055100$flowdata$dateTime,USGS02055100$flowdata$cms,type="l",main="TINKER CREEK NEAR DALEVILLE, VA",
     xlab = 'Time', ylab=' discharge (cms)')
points(USGS02055100$flowdata$dateTime[USGS02055100$flowdata$newwave],
       USGS02055100$flowdata$cms[USGS02055100$flowdata$newwave],col=2)

# Find the time locations where waves begin
which(USGS02055100$flowdata$newwave == TRUE)
plot(USGS02055100$flowdata$dateTime,USGS02055100$flowdata$cms,
     type="l",main="TINKER CREEK NEAR DALEVILLE, VA",xlab = 'Time', ylab=' discharge (cms)',
     xlim=c(USGS02055100$flowdata$dateTime[1109],
                     USGS02055100$flowdata$dateTime[1109+200]))
lines(USGS02055100$flowdata$outTime,USGS02055100$flowdata$cms,col=2)

#1109
#1109+200
###################################################################################################
#USGS 02054530 ROANOKE RIVER AT GLENVAR, VA
###################################################################################################
A=SpatialPoints(USGS02054530$site)# Up gradient site Lick Run
B=SpatialPoints(USGS02056000$site) # Down gradient site ROA River atNiagara
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
cropmydem=crop(mydem,extend(extent(ab_utm),600))
cropmydem=trim(cropmydem)
cropmydem=cropmydem*1000.0
plot(cropmydem)
plot(ab_utm,add=T)
# Set up the weighting functions
altDiff <- function(x){x[2] - x[1]}
hd<- transition(cropmydem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
# Find and plot the flow path
AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
plot(AtoB,add=T)
plot(streams_utm,col="blue",add=T)
plot(AtoB,add=T)
SpatialLinesLengths(AtoB)
USGS02054530$site$L=SpatialLinesLengths(AtoB) # km to m
USGS02054530$site$L # reach length in m
USGS02054530$site$slope=(raster::extract(mydem,A_utm)-
                             raster::extract(mydem,B_utm))/USGS02054530$site$L
USGS02054530$site$slope

# So now we have flow depth (y "$depth_m"), manning's n ("$man_n"), Q ("$cms"), and slope ("$slope") rearrange to solve for B
# B=(n*Q)/(y^(5/3)*sqrt(So)
USGS02054530$flowdata$B=(USGS02054530$site$man_n*
                             USGS02054530$flowdata$cms)/(USGS02054530$flowdata$depth_m^(5/3)*
                                                             sqrt(USGS02054530$site$slope))
head(USGS02054530$flowdata)
# Lets look at how B changes with flow.    
plot(USGS02054530$flowdata$dateTime,USGS02054530$flowdata$B, main="ROANOKE RIVER AT GLENVAR, VA",
     xlab = 'Time', ylab='flow width (m)')
# Does this seem reasonable (...like order of magnitude reasonable)? You can 
# perform a quick and dirty check using google earth and measuring the channel 
# width in a few places.
#
plot(USGS02054530$flowdata$cms,USGS02054530$flowdata$depth_m, main="ROANOKE RIVER AT GLENVAR, VA",
     xlab = 'discharge (cms)', ylab='depth (m)')




# ck
USGS02054530$flowdata$ck = 5/3*((sqrt(USGS02054530$site$slope))/USGS02054530$site$man_n) * USGS02054530$flowdata$depth_m^(2/3)

# ANS
mean(USGS02054530$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
USGS02054530$flowdata$dt = USGS02054530$site$L/USGS02054530$flowdata$ck
mean(USGS02054530$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result


plot(USGS02054530$flowdata$dateTime,USGS02054530$flowdata$dt ,main="ROANOKE RIVER AT GLENVAR, VA",
     xlab = 'Time', ylab=' time of travel (s)')
plot(USGS02055100$flowdata$dateTime,USGS02055100$flowdata$ck ,main="ROANOKE RIVER AT GLENVAR, VA",
     xlab = 'Time', ylab=' kinematic celerity (m/s)')

USGS02054530$flowdata$outTime=USGS02054530$flowdata$dateTime+USGS02054530$flowdata$dt

# Find the beginning of  Waves assuming a new wave starts at 110% of prior 
# flow. This might need to change for your homework
WaveStartDecPercent=1.03
USGS02054530$flowdata$newwave=
  USGS02054530$flowdata$cms * WaveStartDecPercent <
  data.table::shift(USGS02054530$flowdata$cms)
summary(USGS02054530$flowdata$newwave)
# Add plot of the point found
len=length(USGS02054530$flowdata$newwave)
USGS02054530$flowdata$newwave[is.na(USGS02054530$flowdata$newwave)]=F
# Removes repeated finds by going through loop backwords
for (i in seq(len,2)){
  print(i)
  if(USGS02054530$flowdata$newwave[i]==T &
     USGS02054530$flowdata$newwave[i-1]==T){
    USGS02054530$flowdata$newwave[i]=F
  }
}
plot(USGS02054530$flowdata$dateTime,USGS02054530$flowdata$cms,type="l",main="ROANOKE RIVER AT GLENVAR, VA",
     xlab = 'Time', ylab=' discharge (cms)')
points(USGS02054530$flowdata$dateTime[USGS02054530$flowdata$newwave],
       USGS02054530$flowdata$cms[USGS02054530$flowdata$newwave],col=2)

# Find the time locations where waves begin
which(USGS02054530$flowdata$newwave == TRUE)
plot(USGS02054530$flowdata$dateTime,USGS02054530$flowdata$cms,main="ROANOKE RIVER AT GLENVAR, VA",
     xlab = 'Time', ylab=' discharge (cms)',
     type="l",xlim=c(USGS02054530$flowdata$dateTime[1109],
                     USGS02054530$flowdata$dateTime[1109+200]))
lines(USGS02054530$flowdata$outTime,USGS02054530$flowdata$cms,col=2)

#1109
#1109+200



###################################################################################################
#USGS 02055000 ROANOKE RIVER AT ROANOKE, VA
###################################################################################################
A=SpatialPoints(USGS02055000$site)# Up gradient site Lick Run
B=SpatialPoints(USGS02056000$site) # Down gradient site ROA River atNiagara
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
cropmydem=crop(mydem,extend(extent(ab_utm),600))
cropmydem=trim(cropmydem)
cropmydem=cropmydem*1000.0
plot(cropmydem)
plot(ab_utm,add=T)
# Set up the weighting functions
altDiff <- function(x){x[2] - x[1]}
hd<- transition(cropmydem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
# Find and plot the flow path
AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
plot(AtoB,add=T)
plot(streams_utm,col="blue",add=T)
plot(AtoB,add=T)
SpatialLinesLengths(AtoB)
USGS02055000$site$L=SpatialLinesLengths(AtoB) # km to m
USGS02055000$site$L # reach length in m
USGS02055000$site$slope=(raster::extract(mydem,A_utm)-
                             raster::extract(mydem,B_utm))/USGS02055000$site$L
USGS02055000$site$slope

# So now we have flow depth (y "$depth_m"), manning's n ("$man_n"), Q ("$cms"), and slope ("$slope") rearrange to solve for B
# B=(n*Q)/(y^(5/3)*sqrt(So)
USGS02055000$flowdata$B=(USGS02055000$site$man_n*
                             USGS02055000$flowdata$cms)/(USGS02055000$flowdata$depth_m^(5/3)*
                                                             sqrt(USGS02055000$site$slope))
head(USGS02055000$flowdata)
# Lets look at how B changes with flow.    
plot(USGS02055000$flowdata$dateTime,USGS02055000$flowdata$B, main="ROANOKE RIVER AT ROANOKE, VA",
     xlab = 'Time', ylab='flow width (m)')
# Does this seem reasonable (...like order of magnitude reasonable)? You can 
# perform a quick and dirty check using google earth and measuring the channel 
# width in a few places.
#
plot(USGS02055000$flowdata$cms,USGS02055000$flowdata$depth_m, main="ROANOKE RIVER AT ROANOKE, VA",
     xlab = 'discharge (cms)', ylab='depth (m)')




# ck
USGS02055000$flowdata$ck = 5/3*((sqrt(USGS02055000$site$slope))/USGS02055000$site$man_n) * USGS02055000$flowdata$depth_m^(2/3)

# ANS
mean(USGS02055000$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
USGS02055000$flowdata$dt = USGS02055000$site$L/USGS02055000$flowdata$ck
mean(USGS02055000$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result


plot(USGS02055000$flowdata$dateTime,USGS02055000$flowdata$dt,main="ROANOKE RIVER AT ROANOKE, VA",
     xlab = 'Time', ylab=' time of travel (s)')
plot(USGS02055100$flowdata$dateTime,USGS02055100$flowdata$ck,main="ROANOKE RIVER AT ROANOKE, VA",
     xlab = 'Time', ylab=' kinematic celerity (m/s)')
USGS02055000$flowdata$outTime=USGS02055000$flowdata$dateTime+USGS02055000$flowdata$dt

# Find the beginning of  Waves assuming a new wave starts at 110% of prior 
# flow. This might need to change for your homework
WaveStartDecPercent=1.05
USGS02055000$flowdata$newwave=
  USGS02055000$flowdata$cms * WaveStartDecPercent <
  data.table::shift(USGS02055000$flowdata$cms)
summary(USGS02055000$flowdata$newwave)
# Add plot of the point found
len=length(USGS02055000$flowdata$newwave)
USGS02055000$flowdata$newwave[is.na(USGS02055000$flowdata$newwave)]=F
# Removes repeated finds by going through loop backwords
for (i in seq(len,2)){
  print(i)
  if(USGS02055000$flowdata$newwave[i]==T &
     USGS02055000$flowdata$newwave[i-1]==T){
    USGS02055000$flowdata$newwave[i]=F
  }
}
plot(USGS02055000$flowdata$dateTime,USGS02055000$flowdata$cms,type="l",main="ROANOKE RIVER AT ROANOKE, VA",
     xlab = 'Time', ylab=' discharge (cms)')
points(USGS02055000$flowdata$dateTime[USGS02055000$flowdata$newwave],
       USGS02055000$flowdata$cms[USGS02055000$flowdata$newwave],col=2)

# Find the time locations where waves begin
which(USGS02055000$flowdata$newwave == TRUE)
plot(USGS02055000$flowdata$dateTime,USGS02055000$flowdata$cms,
     type="l",main="ROANOKE RIVER AT ROANOKE, VA",
     xlab = 'Time', ylab=' discharge (cms)',
     xlim=c(USGS02055000$flowdata$dateTime[1000],
                     USGS02055000$flowdata$dateTime[1400]))
lines(USGS02055000$flowdata$outTime,USGS02055000$flowdata$cms,col=2)

#1109
#1109+200
####################################################################################
#USGS gages on one of the major Continental US river systems
####################################################################################
USGS07010000  = make_usgs_gage_list(siteNo = "07010000" )
USGS07010035  = make_usgs_gage_list(siteNo = "07010035" )
USGS07010040  = make_usgs_gage_list(siteNo = "07010040" )
USGS07010090  = make_usgs_gage_list(siteNo = "07010090" )


ab_ll=rbind(USGS07010000$site,
            USGS07010035$site,
            USGS07010040$site,
            USGS07010090$site)
class(ab_ll)
ab_ll@proj4string
proj4_utm = paste0("+proj=utm +zone=",
                   trunc((180+coordinates(USGS07010000$site)[1])/6+1), 
                   " +datum=WGS84 +units=m +no_defs")
print(proj4_utm)
# Lat/Lon (_ll) is much easier!
proj4_ll = "+proj=longlat"
crs_ll=CRS(proj4_ll)
crs_utm=CRS(proj4_utm)
proj4string(ab_ll)=proj4_ll
ab_utm=spTransform(ab_ll,crs_utm)
ab_utm@coords
mydem=get_aws_terrain(locations=ab_utm@coords, 
                      z = 12, prj = proj4_utm,expand=1)
#
# Lets plot the DEM and the gage locations so we can guess 
# what gages connect with what gages
#
plot(mydem)
plot(ab_utm,add=T)
text(ab_utm, labels=ab_utm@data$site_no, cex=0.6, font=2,pos=1)
# Streams as USGS sees them, I know I can get an overview of streams with the 
# USGS H
url="https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/Shape/NHD_H_03010101_HU8_Shape.zip"
#browseURL(url)
curl_download(url,"NHD_H_03010101_HU8_Shape.zip")
unzip("NHD_H_03010101_HU8_Shape.zip",exdir="03010101")
streams=readOGR("03010101/Shape/NHDFlowline.dbf")
streams_utm=spTransform(streams,crs_utm)
plot(streams_utm,col="blue",add=T)

##############################################################################################################
# USGS07010035	 MEngelholm Creek near Wellston, MO
##############################################################################################################
A=SpatialPoints(USGS07010035$site)# Up gradient site Lick Run
B=SpatialPoints(USGS07010000$site) # Down gradient site ROA River atNiagara
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
cropmydem=crop(mydem,extend(extent(ab_utm),600))
cropmydem=trim(cropmydem)
cropmydem=cropmydem*1000.0
plot(cropmydem)
plot(ab_utm,add=T)
# Set up the weighting functions
altDiff <- function(x){x[2] - x[1]}
hd<- transition(cropmydem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
# Find and plot the flow path
AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
plot(AtoB,add=T)
plot(streams_utm,col="blue",add=T)
plot(AtoB,add=T)
SpatialLinesLengths(AtoB)
USGS07010035$site$L=SpatialLinesLengths(AtoB) # km to m
USGS07010035$site$L # reach length in m
USGS07010035$site$slope=(raster::extract(mydem,A_utm)-
                           raster::extract(mydem,B_utm))/USGS07010035$site$L
USGS07010035$site$slope

# So now we have flow depth (y "$depth_m"), manning's n ("$man_n"), Q ("$cms"), and slope ("$slope") rearrange to solve for B
# B=(n*Q)/(y^(5/3)*sqrt(So)
USGS07010035$flowdata$B=(USGS07010035$site$man_n*
                           USGS07010035$flowdata$cms)/(USGS07010035$flowdata$depth_m^(5/3)*
                                                         sqrt(USGS07010035$site$slope))
head(USGS07010035 $flowdata)
# Lets look at how B changes with flow.    
plot(USGS07010035$flowdata$dateTime,USGS07010035 $flowdata$B, main="Engelholm Creek near Wellston, MO",
     xlab = 'Time', ylab='flow width (m)')
# Does this seem reasonable (...like order of magnitude reasonable)? You can 
# perform a quick and dirty check using google earth and measuring the channel 
# width in a few places.
#
plot(USGS07010035$flowdata$cms,USGS07010035$flowdata$depth_m, main="Engelholm Creek near Wellston, MO",
     xlab = 'discharge (cms)', ylab='depth (m)')


# ck
USGS07010035$flowdata$ck = 5/3*((sqrt(USGS07010035$site$slope))/USGS07010035$site$man_n) * USGS07010035$flowdata$depth_m^(2/3)

# ANS
mean(USGS07010035$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
USGS07010035$flowdata$dt = USGS07010035$site$L/USGS07010035$flowdata$ck
mean(USGS07010035$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result


plot(USGS07010035$flowdata$dateTime,USGS07010035$flowdata$dt,main="Engelholm Creek near Wellston, MO",
     xlab = 'Time', ylab=' time of travel (s)')
plot(USGS07010035$flowdata$dateTime,USGS07010035$flowdata$ck,main="Engelholm Creek near Wellston, MO",
     xlab = 'Time', ylab=' kinematic celerity (m/s)')
USGS07010035$flowdata$outTime=USGS07010035$flowdata$dateTime+USGS07010035$flowdata$dt

# Find the beginning of  Waves assuming a new wave starts at 110% of prior 
# flow. This might need to change for your homework
WaveStartDecPercent=1.30
USGS07010035$flowdata$newwave=
  USGS07010035$flowdata$cms * WaveStartDecPercent <
  data.table::shift(USGS07010035$flowdata$cms)
summary(USGS07010035$flowdata$newwave)
# Add plot of the point found
len=length(USGS07010035$flowdata$newwave)
USGS07010035$flowdata$newwave[is.na(USGS07010035$flowdata$newwave)]=F
# Removes repeated finds by going through loop backwords
for (i in seq(len,2)){
  print(i)
  if(USGS07010035$flowdata$newwave[i]==T &
     USGS07010035$flowdata$newwave[i-1]==T){
    USGS07010035$flowdata$newwave[i]=F
  }
}
plot(USGS07010035$flowdata$dateTime,USGS07010035$flowdata$cms,type="l",main="Engelholm Creek near Wellston, MO",
     xlab = 'Time', ylab=' discharge (cms)')
points(USGS07010035$flowdata$dateTime[USGS07010035$flowdata$newwave],
       USGS07010035$flowdata$cms[USGS07010035$flowdata$newwave],col=2)

# Find the time locations where waves begin
which(USGS07010035$flowdata$newwave == TRUE)
plot(USGS07010035$flowdata$dateTime,USGS07010035$flowdata$cms,
     type="l",main="Engelholm Creek near Wellston, MO",xlab = 'Time', ylab=' discharge (cms)',
     xlim=c(USGS07010035$flowdata$dateTime[500],
            USGS07010035$flowdata$dateTime[1000]))
lines(USGS07010035$flowdata$outTime,USGS07010035$flowdata$cms,col=2)

#1109
#1109+200
###################################################################################################
#USGS07010040 Denny Creek at Ladue, MO 
###################################################################################################
A=SpatialPoints(USGS07010040$site)# Up gradient site Lick Run
B=SpatialPoints(USGS07010000$site) # Down gradient site ROA River atNiagara
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
cropmydem=crop(mydem,extend(extent(ab_utm),600))
cropmydem=trim(cropmydem)
cropmydem=cropmydem*1000.0
plot(cropmydem)
plot(ab_utm,add=T)
# Set up the weighting functions
altDiff <- function(x){x[2] - x[1]}
hd<- transition(cropmydem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
# Find and plot the flow path
AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
plot(AtoB,add=T)
plot(streams_utm,col="blue",add=T)
plot(AtoB,add=T)
SpatialLinesLengths(AtoB)
USGS07010040$site$L=SpatialLinesLengths(AtoB) # km to m
USGS07010040$site$L # reach length in m
USGS07010040$site$slope=(raster::extract(mydem,A_utm)-
                           raster::extract(mydem,B_utm))/USGS07010040$site$L
USGS07010040$site$slope

# So now we have flow depth (y "$depth_m"), manning's n ("$man_n"), Q ("$cms"), and slope ("$slope") rearrange to solve for B
# B=(n*Q)/(y^(5/3)*sqrt(So)
USGS07010040$flowdata$B=(USGS07010040$site$man_n*
                           USGS07010040$flowdata$cms)/(USGS07010040$flowdata$depth_m^(5/3)*
                                                         sqrt(USGS07010040$site$slope))
head(USGS07010040$flowdata)
# Lets look at how B changes with flow.    
plot(USGS07010040$flowdata$dateTime,USGS07010040$flowdata$B, main="Denny Creek at Ladue, MO ",
     xlab = 'Time', ylab='flow width (m)')
# Does this seem reasonable (...like order of magnitude reasonable)? You can 
# perform a quick and dirty check using google earth and measuring the channel 
# width in a few places.
#
plot(USGS07010040$flowdata$cms,USGS07010040$flowdata$depth_m, main="Denny Creek at Ladue, MO ",
     xlab = 'discharge (cms)', ylab='depth (m)')




# ck
USGS07010040$flowdata$ck = 5/3*((sqrt(USGS07010040$site$slope))/USGS07010040$site$man_n) * USGS07010040$flowdata$depth_m^(2/3)

# ANS
mean(USGS07010040$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
USGS07010040$flowdata$dt = USGS07010040$site$L/USGS07010040$flowdata$ck
mean(USGS07010040$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result


plot(USGS07010040$flowdata$dateTime,USGS07010040$flowdata$dt ,main="Denny Creek at Ladue, MO ",
     xlab = 'Time', ylab=' time of travel (s)')
plot(USGS07010040$flowdata$dateTime,USGS07010040$flowdata$ck ,main="Denny Creek at Ladue, MO ",
     xlab = 'Time', ylab=' kinematic celerity (m/s)')

USGS07010040$flowdata$outTime=USGS07010040$flowdata$dateTime+USGS07010040$flowdata$dt

# Find the beginning of  Waves assuming a new wave starts at 110% of prior 
# flow. This might need to change for your homework
WaveStartDecPercent=1.3
USGS07010040$flowdata$newwave=
  USGS07010040$flowdata$cms * WaveStartDecPercent <
  data.table::shift(USGS07010040$flowdata$cms)
summary(USGS07010040$flowdata$newwave)
# Add plot of the point found
len=length(USGS07010040$flowdata$newwave)
USGS07010040$flowdata$newwave[is.na(USGS07010040$flowdata$newwave)]=F
# Removes repeated finds by going through loop backwords
for (i in seq(len,2)){
  print(i)
  if(USGS07010040$flowdata$newwave[i]==T &
     USGS07010040$flowdata$newwave[i-1]==T){
    USGS07010040$flowdata$newwave[i]=F
  }
}
plot(USGS07010040$flowdata$dateTime,USGS07010040$flowdata$cms,type="l",main="Denny Creek at Ladue, MO ",
     xlab = 'Time', ylab=' discharge (cms)')
points(USGS07010040$flowdata$dateTime[USGS07010040$flowdata$newwave],
       USGS07010040$flowdata$cms[USGS07010040$flowdata$newwave],col=2)

# Find the time locations where waves begin
which(USGS07010040$flowdata$newwave == TRUE)
plot(USGS07010040$flowdata$dateTime,USGS07010040$flowdata$cms,main="Denny Creek at Ladue, MO ",
     xlab = 'Time', ylab=' discharge (cms)',
     type="l",xlim=c(USGS07010040$flowdata$dateTime[600],
                     USGS07010040$flowdata$dateTime[1200]))
lines(USGS07010040$flowdata$outTime,USGS07010040$flowdata$cms,col=2)

#1109
#1109+200



###################################################################################################
#USGS07010090 MacKenzie Creek near Shrewsbury, MO 
###################################################################################################
A=SpatialPoints(USGS07010090$site)# Up gradient site Lick Run
B=SpatialPoints(USGS07010000$site) # Down gradient site ROA River atNiagara
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
cropmydem=crop(mydem,extend(extent(ab_utm),600))
cropmydem=trim(cropmydem)
cropmydem=cropmydem*1000.0
plot(cropmydem)
plot(ab_utm,add=T)
# Set up the weighting functions
altDiff <- function(x){x[2] - x[1]}
hd<- transition(cropmydem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
# Find and plot the flow path
AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
plot(AtoB,add=T)
plot(streams_utm,col="blue",add=T)
plot(AtoB,add=T)
SpatialLinesLengths(AtoB)
USGS07010090$site$L=SpatialLinesLengths(AtoB) # km to m
USGS07010090$site$L # reach length in m
USGS07010090$site$slope=(raster::extract(mydem,A_utm)-
                           raster::extract(mydem,B_utm))/USGS07010090$site$L
USGS07010090$site$slope

# So now we have flow depth (y "$depth_m"), manning's n ("$man_n"), Q ("$cms"), and slope ("$slope") rearrange to solve for B
# B=(n*Q)/(y^(5/3)*sqrt(So)
USGS07010090$flowdata$B=(USGS07010090$site$man_n*
                           USGS07010090$flowdata$cms)/(USGS07010090$flowdata$depth_m^(5/3)*
                                                         sqrt(USGS07010090$site$slope))
head(USGS07010090$flowdata)
# Lets look at how B changes with flow.    
plot(USGS07010090$flowdata$dateTime,USGS07010090$flowdata$B, main="MacKenzie Creek near Shrewsbury, MO ",
     xlab = 'Time', ylab='flow width (m)')
# Does this seem reasonable (...like order of magnitude reasonable)? You can 
# perform a quick and dirty check using google earth and measuring the channel 
# width in a few places.
#
plot(USGS07010090$flowdata$cms,USGS07010090$flowdata$depth_m, main="MacKenzie Creek near Shrewsbury, MO ",
     xlab = 'discharge (cms)', ylab='depth (m)')




# ck
USGS07010090$flowdata$ck = 5/3*((sqrt(USGS07010090$site$slope))/USGS07010090$site$man_n) * USGS07010090$flowdata$depth_m^(2/3)

# ANS
mean(USGS07010090$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
USGS07010090$flowdata$dt = USGS07010090$site$L/USGS07010090$flowdata$ck
mean(USGS07010090$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result


plot(USGS07010090$flowdata$dateTime,USGS07010090$flowdata$dt,main="MacKenzie Creek near Shrewsbury, MO ",
     xlab = 'Time', ylab=' time of travel (s)')
plot(USGS07010090$flowdata$dateTime,USGS07010090$flowdata$ck,main="MacKenzie Creek near Shrewsbury, MO ",
     xlab = 'Time', ylab=' kinematic celerity (m/s)')
USGS07010090$flowdata$outTime=USGS07010090$flowdata$dateTime+USGS07010090$flowdata$dt

# Find the beginning of  Waves assuming a new wave starts at 110% of prior 
# flow. This might need to change for your homework
WaveStartDecPercent=1.2
USGS07010090$flowdata$newwave=
  USGS07010090$flowdata$cms * WaveStartDecPercent <
  data.table::shift(USGS07010090$flowdata$cms)
summary(USGS07010090$flowdata$newwave)
# Add plot of the point found
len=length(USGS07010090$flowdata$newwave)
USGS07010090$flowdata$newwave[is.na(USGS07010090$flowdata$newwave)]=F
# Removes repeated finds by going through loop backwords
for (i in seq(len,2)){
  print(i)
  if(USGS07010090$flowdata$newwave[i]==T &
     USGS07010090$flowdata$newwave[i-1]==T){
     USGS07010090$flowdata$newwave[i]=F
  }
}
plot(USGS07010090$flowdata$dateTime,USGS07010090$flowdata$cms,type="l",main="MacKenzie Creek near Shrewsbury, MO ",
     xlab = 'Time', ylab=' discharge (cms)')
points(USGS07010090$flowdata$dateTime[USGS07010090$flowdata$newwave],
       USGS07010090$flowdata$cms[USGS07010090$flowdata$newwave],col=2)

# Find the time locations where waves begin
which(USGS07010090$flowdata$newwave == TRUE)
plot(USGS07010090$flowdata$dateTime,USGS07010090$flowdata$cms,
     type="l",main="MacKenzie Creek near Shrewsbury, MO , VA",
     xlab = 'Time', ylab=' discharge (cms)',
     xlim=c(USGS07010090$flowdata$dateTime[5000],
            USGS07010090$flowdata$dateTime[10000]))
lines(USGS07010090$flowdata$outTime,USGS07010090$flowdata$cms,col=2)

#1109
#1109+200

