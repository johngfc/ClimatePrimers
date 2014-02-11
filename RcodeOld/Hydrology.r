setwd("ClimateData\\Yellowstone\\hydro3")
a<-open.ncdf("ClimateData\\Yellowstone\\hydro3\\Extraction_et.nc")
 ET<-get.var.ncdf(nc=a)
   
   Time = c('1950-01-01T00:00:00Z','2099-12-31T00:00:00Z') 
  TimeStep = "1960-01-01T00:00:00Z"
  OutputFile<-"Extraction_swe.nc"
   LongitudeOffset =360
   
 SpatialMeanSDPlot<-function(OutputFile,Time,LongitudeOffset,TimeStep){
 #This code designed for the 
     #lon, lat, time proj Period:1950Jan through 2099Dec from the metadata jan 1950-Dec2099
       Clim<-open.ncdf(file.path(OutputFile))
              Lat<-Clim$dim$lat$vals
              Lon<-Clim$dim$lon$vals-LongitudeOffset  
              Time = seq(from = (as.numeric(substr(Time[1],1,4))+as.numeric(substr(Time[1],6,7))/12),
                 to = (as.numeric(substr(Time[2],1,4))+as.numeric(substr(Time[2],6,7))/12),
                 by = 1/12)-1/12 #so january ends up being a round number and decmber is in ther current year
          # Check that I have the right length at least
         
         
         if(length(Time)!=length(Clim$dim$time$vals)) stop("time dimensions are wrong")
               Projections <- Clim$dim$projection$vals
               TimeStep<-as.numeric(substr(TimeStep,1,4))+as.numeric(substr(TimeStep,6,7))/12
              Match<-which(Time==TimeStep,arr.ind=TRUE)
               RastArray<-get.var.ncdf(Clim, start=c(1,1,Match,1), count=c(length(Lon),length(Lat),1,length(Projections)) )
    ProjMean<-apply(RastArray,c(1,2),mean)
    ProjSD<-apply(RastArray,c(1,2),sd)
    Zlim<-range(c(range(ProjSD,na.rm=TRUE),range(ProjMean,na.rm=TRUE)))
    par(mfrow=c(2,2))
    image(ProjMean,zlim=Zlim,col=rev(terrain.colors(34)),breaks=seq(from=Zlim[1],to=Zlim[2],length=35))
    image(ProjSD,zlim=Zlim,col=rev(terrain.colors(34)),breaks=seq(from=Zlim[1],to=Zlim[2],length=35))
}