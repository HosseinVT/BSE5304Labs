url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/Lab05SetupDRF.R"
# This will grab the solution for last weeks Lab03 Homework
download.file(url,"Lab05SetupDRF.R")
file.edit("Lab05SetupDRF.R")

# Grab out models for Snow and TMWB
# https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TMWBFuncs.R
# becomes: 
url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TMWBFuncs.R"
# This will grab the solution for last weeks Lab03 Homework
download.file(url,"TMWBFuncs.R")
file.edit("TMWBFuncs.R")
# I actually am starting to trust my snow model
url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TISnow.R"
# This will grab the solution for last weeks Lab03 Homework
source(url)

url="https://raw.githubusercontent.com/HosseinVT/BSE5304Labs/main/R/CNModel.R"
# This will grab the CN model
download.file(url,"CNmodel.R")
file.edit("CNmodel.R")

########################################   TMWB Model   #############################################

TMWBoptFunc <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- x[4]
  x5 <- x[5]
  x6 <- x[6]
  x7 <- x[7]
  x8 <- x[8]
  x9 <- x[9]
  
  
  
  outTMWB=TMWBmodel(TMWBdf=TMWB , fcres = x1 , FldCap = x2 , WiltPt = x3 , Z = x4 ,
                    SFTmp = x5 , bmlt6 = x6 , bmlt12 = x7 , Tmlt = x8 , Tlag = x9)
  return(1-NSE(Yobs = outTMWB$Qmm, Ysim = outTMWB$Qpred))
  

}


lower <- c(.2, 0.01, 0.01, 300,  1 , 0.1 , 0.1 , 1 , 0)
upper <- c(.95, 0.99, 0.99, 3000, 6 , 7   ,  7  , 4 , 1)
outDEoptim=DEoptim(TMWBoptFunc,lower,upper,
                   DEoptim.control(NP = 200,
                                   itermax = 25, F = 1.2, CR = 0.7))


TMWBnew=TMWBmodel(TMWBdf=TMWB , fcres =  0.201141    , FldCap = 0.589968    , WiltPt = 0.387111 , 
                  Z = 1077.360636    , SFTmp = 1.084522    , bmlt6 = 4.370660    ,    
                  bmlt12 = 4.316808 , Tmlt = 3.588804    , Tlag = 0.889353 )

plot(TMWBnew$date,TMWBnew$Qmm,type = "l",col="red", xlab = "Year", ylab="Streamflow (mm/day)")
lines(TMWBnew$date,TMWBnew$Qpred,col="blue")
legend("topright", c("Qmm", "Qpred"), col = c("red", "blue"),
       lty = 1:2, cex = 0.8)
NSE(TMWBnew$Qmm,TMWBnew$Qpred)


############################################## CN model ################################################
CNoptFunc <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- x[4]
  x5 <- x[5]
  x6 <- x[6]
  x7 <- x[7]

  

  outCN=CNmodel(CNmodeldf=TMWB , CNavg = x1 , IaFrac = x2 , fnc_slope = x3 , fnc_aspect = x4 ,
                    func_DAWC = x5 , func_z = x6 , fnc_fcres = x7 )
  return(1-NSE(Yobs = outCN$Qmm, Ysim = outCN$Qpred))
  
  
  
}


lower <- c(35, 0.01, 0.01, 0.01, 0.1,  500 , 0.1)
upper <- c(99, 0.25, 0.25, 0.25, 0.35, 1000 , 0.3)
outDEoptim=DEoptim(CNoptFunc,lower,upper,
                   DEoptim.control(NP = 200,
                                   itermax = 25, F = 1.2, CR = 0.7))


CNnew=CNmodel(CNmodeldf=TMWB ,CNavg=97.758172    ,IaFrac = 0.018553    ,fnc_slope=0.095379    ,
              fnc_aspect=0.134783,func_DAWC=0.340626  ,func_z=971.035345,fnc_fcres=0.250289 )

plot(CNnew$date,CNnew$Qmm,type = "l",col="red", xlab = "Year", ylab="Streamflow (mm/day)")
lines(CNnew$date,CNnew$Qpred,col="blue")
legend("topright", c("Qmm", "Qpred"), col = c("red", "blue"),
       lty = 1:2, cex = 0.8)
NSE(CNnew$Qmm,CNnew$Qpred)
