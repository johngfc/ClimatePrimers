#=============================================
# reading in my helper functions 
#  and all required libraries
#=============================================
    setwd("H:\\Desktop\\Climate\\Rcode")
    sourceList<-list("ChkLibs.r","ClipToPolygon.r","MyPlotSetup.r","GetParkBound.r","PlotClimOverMap.r")
    unlist(lapply(sourceList,source))
    
    ChkLibs(list("maptools","rgdal","raster","OpenStreetMap","mapdata","ncdf","fields","maps","RNCEP"))
    setwd("H:\\Desktop\\Climate")

#============================================
# Inputting all data that is required to produce
# the primer 
#============================================
    ParkCode="YELL"
    ParkName="Yellowstone National Park"
    VarID<-"ACCESS1-0_pr" #from the netCDF
    NpsShapes<-"H:\\Desktop\\Climate\\Boundary2\\Export_output.shp"
    LongitudeOffset<-360 #in the last dataset 150 worked I'm not sure why
    Region=c("wyoming","montana")
    ModelKeyPath="H:\\Desktop\\Climate\\bcsd5\\Projections5.txt"
    Present=2013
#read in PRISM and NEX CMIP5 for the desired park by submitting the shape file 
#===========================================
#using rGDP
#eventually this should come from geoDat Portal but not today....
#Clim<-open.ncdf("bcsd5\\Extraction_tas.nc") #this is old and coarser
#============================================




#============================================
# Opening the netCDF, extracting what I need and
# and reading in the Park shape file 
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
    
    Bound<-GetParkBound(Bndry=NpsShapes,ParkCode)
#produce several potential graphics and summaries based on this
PlotClimOverMap(Bndy,TempAvg)