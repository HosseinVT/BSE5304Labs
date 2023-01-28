install.packages("pacman")
pacman::p_load(rgdal, parallel, htsr, ggplot2, dplyr, patchwork, hrbrthemes, rnoaa, ggeasy, tibble, dplyr, scales )

system("git config --global user.email 'h.ahmadi@vt.edu' ") 
system("git config --global user.name 'HosseinVT' ")



# Latitude and Longitude 

HTlat <- 38.754919180319526
HTlong <- -77.44796544507564

# Weather Data

stns=meteo_distance(
  station_data=ghcnd_stations(),
  lat=HTlat,
  long=HTlong,
  units = "deg",
  radius = 20,
  limit = NULL
)

View(stns)

#USC00445204
WXData=meteo_pull_monitors(
  monitors=stns[8,1],    # replace the *** with index you find
  keep_flags = FALSE,
  date_min = "2022-01-01",
  date_max = "2022-12-31",
  var = c("TMAX","TMIN","PRCP") 
)
View(WXData)

#USGS Data

source("https://goo.gl/Cb8zGn")
myflowgage_id="01658500"
myflowgage=get_usgs_gage(myflowgage_id, begin_date="2022-01-01",end_date="2022-12-31")
plot(myflowgage$flowdata$mdate,myflowgage$flowdata$flow,
     main=myflowgage$gagename,xlab = "Date",
     ylab="Flow m^3/day",type="l")
barplot(myflowgage$flowdata$flow,main="USGS stream flow",
        xlab="Day", ylab = "Flow m^3/day")

# Plot

data <- data.frame(
  day=WXData$date,tmin=WXData$tmin/10,tmax=WXData$tmax/10,prcp=WXData$prcp/10)

View(data)
p <- ggplot(data, aes(x=day)) +
  
 geom_line( aes(y=myflowgage$flowdata$flow/5000, color="FLOW"), size=0.5)+
 geom_line( aes(y=tmin, color="TMIN"), size=0.2) + 
 geom_line( aes(y=tmax, color="TMAX"), size=0.2) + 
 geom_bar(aes(y=prcp, fill = "PRCP (mm)"), stat="identity")+
  
  coord_cartesian(ylim=c(-20,60))+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature (Celsius °)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name="Streamflow/5000 (m^3/day)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = "black", size=10),
    axis.title.y.right = element_text(color = "black", size=10)
  ) +
  
  ggtitle(" Hometown Weather and Streamflow Data") +
  ggeasy::easy_center_title() +
  
  scale_colour_manual(name ="", values = c("TMIN"="coral2", "TMAX"="cyan3", "FLOW"="goldenrod4" ))+
  scale_fill_manual  (name ="", values = c("PRCP (mm)"="darkolivegreen4" ))

plot (p)



