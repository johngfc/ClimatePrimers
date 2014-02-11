Start <- c(2015,2055,2095)
End <- c(2018,2058,2098)

Start <- c(2015,2095)
End <- c(2018,2098)
setwd("/data/nccsc/Private/TalbertM/MariansProjects/Climate")
#setwd("/data")

source("ChkLibs.r")
ChkLibs(list("ncdf4","shapefiles","maptools","raster"))

source("InputConstants.r")
source("BlodgettOpenDapAccessMarianTries.r")
source("GetParkBox.r")
source("RequestNetCDF.R")
source("GetParkBoundary.r")
 ParkCode = "Yell"
    ParkName = "Mesa Verde"
    NpsShapes ="NPS_boundaries/nps_boundary.shp" #the folder containg the NPS shape file
    NpsShapes="PACES/PACES_merge.shp"
    OutputNcdfPath="/data/nccsc/Private/TalbertM/MariansProjects/Climate" #a linux path as this will only run
    HistoricTime = c('1895-01-01T00:00:00Z','2013-02-1T00:00:00Z') 
    Buffer="30km"  
 #on linux
#for testing making the model list smallish 
 NexLst<-list(rcp45 = ModelList[[1]][c(1,2,3,52,53,54,94,95,96,70,71,72)],
          rcp85=ModelList[[2]][c(1,2,3,46,47,48,88,89,90,70,71,72)])
 )
 
 PrsmLst<-list()
 PrsmLst[[1]]<-ModelList[[3]]     
# URIs I'll eventually need as input for now just the NEX is used
Prism="http://cida.usgs.gov/thredds/dodsC/prism"
#Daymet="http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet",
Nex="http://dataserver.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd"    
#===============================================================
#                      end of input here    

BoundBox<- GetParkBox(Bndry=NpsShapes,ParkCode,Extend=TRUE) 

RequestNetCDF(BoundBox,URI=Nex,ModelList=NexLst,
    Start,End,OutputNcdfPath,FileName="Nex.nc",ParkCode)
RequestNetCDF(BoundBox,URI=Prism,ModelList=PrsmLst,Start=HistoricTime[1],End=HistoricTime[2],
   OutputNcdfDir=OutputNcdfPath,FileName="Prism.nc",ParkCode=ParkCode)
                               