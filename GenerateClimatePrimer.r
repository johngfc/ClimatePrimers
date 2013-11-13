#=============================================
# reading in my helper functions 
#  and all required libraries
#=============================================
    setwd("H:\\Desktop\\Climate\\Rcode")
    #setwd("C:\\Users\\mallen\\Desktop\\Climate\\Rcode")
    sourceList<-list("ChkLibs.r","ClipToPolygon.r","MyPlotSetup.r","GetParkBoundary.r","GetParkBox.r","PlotClimOverMap.r",
       "GDPNexPrismGrab.r","InputConstants.r")
    unlist(lapply(sourceList,source))
    
    ChkLibs(list("maptools","rgdal","raster","OpenStreetMap","mapdata","ncdf","fields","maps","RNCEP","rGDP","ggplot2","zoo"))
    setwd("H:\\Desktop\\Climate")
    #setwd("C:\\Users\\mallen\\Desktop\\Climate")
#============================================
# Inputting all data that is required to produce
# the primer 
#============================================
    ParkCode = "YELL"
    ParkName = "Yellowstone National Park"
    NpsShapes ="Boundary2\\Export_output.shp" #the folder containg the NPS shape file
    LongitudeOffset =360 #in the last dataset 150 worked I'm not sure why
    Region = c("wyoming","montana") #for plotting background maps I might actually not need this
                                    #if the spatial plotting extent is set by the bounding box of the shape
                                    #might just use usa
    ModelKeyPath = "bcsd5\\Projections5.txt" #associates keys with park names
    Present=2013
    ES = c("rcp26","rcp45","rcp60","rcp85","historical")
    Model = ModelList # further subset this based on what I need
         Model[[1]]<-ModelList[[1]][c(2,3,65,66,47,48)]
         Model[[2]]<-ModelList[[2]][c(2,3,65,66,47,48)]
             #the climate models for a given emissions scenario
             #unfortunately models differ for each emissions scenario so get these using
             #getDataIDs(rGDP) once the url is set
    GCMTime = c('2000-01-01T00:00:00Z','2099-12-31T00:00:00Z') #Year-Month-Day and then time 
             #Nex is only 1/14/1950-1/1/2100 best to end on the last day of the year
             #then I don't have to worry about partial years 
    HistoricTime = c('1910-01-01T00:00:00Z','2011-12-31T00:00:00Z')  
             #Prisim is only 1/1/1895-11/1/2011  
    OutputDir = "H:\\Desktop\\Climate\\ClimateData" 
    MovAvgPeriod = 10 
#===========================================    
#get a bounding box for the park for now later we can just grab the shape 
 
  BoundBox <- GetParkBox(Bndry=NpsShapes,ParkCode) 
  Boundary <- GetParkBoundary(Bndry=NpsShapes,ParkCode)  
  
#read in PRISM and NEX CMIP5 for the desired park by submitting the shape file 
#===========================================
#using rGDP

GDPNexPrismGrab(BoundBox,GCMTime,HistoricTime,Model,OutputDir)
  
#Clim<-open.ncdf("bcsd5\\Extraction_tas.nc") #this is old and coarser
#============================================
# we can also use the following website:
# http://gdo-dcp.ucllnl.org/downscaled_cmip_projections/dcpInterface.html
# for larger areas at a coarser resolution
# I need to write something to format the data in a way I can consume


#============================================
# Opening the netCDF, extracting what I need and
# and reading in the Park shape file
Prismppt<-ReadClimateNetCDF(OutputDir,Time=HistoricTime,"prism","ppt")
YearlyLinePlot(Prismppt,MovAvgPeriod=30,Boundary)

#I think this would work if this command worked from the CSC
 PlotClimOverMap(Boundary,Prismppt[,,1],Lon=as.numeric(dimnames(Prismppt)[[1]]),Lat=as.numeric(dimnames(Prismppt)[[2]]))
 
    
    
    Clim<-open.ncdf("C:\\Users\\mtalbert\\Downloads\\e0c8d36d-1707-4a0f-9e56-231acc78dbd5OUTPUT.d97a2137-85b6-42ee-aad1-bad7c611525a.nc")
    
    Lat<-Clim$dim$lat$vals
    Lon<-(Clim$dim$lon$vals-LongitudeOffset) # I really have no idea how the longitudes got changed...
    Time<-Clim$dim$time$vals
    Year<-1950+floor(Time/365)
    Month<-rep(c("Jan","Feb","March","April","May","June",
       "July","Aug","Sep","Oct","Nov","Dec"),times=length(unique(Year)))
    
    #clip off the extra if the last year wasn't complete
    Month<-Month[1:length(Year)]
    
    #these are projections from 43 climate models 
    TempAvg<-get.var.ncdf(nc=Clim,varid=VarID)
    
    #generating a quick plot to check what I have
    ColorScale<-tim.colors()
    
    #for any of these we can plot the spatial projection I'm doing the first and last
    image.plot(TempAvg[,,1],col=tim.colors())
    
   
#produce several potential graphics and summaries based on this
PlotClimOverMap(Bndy,TempAvg)