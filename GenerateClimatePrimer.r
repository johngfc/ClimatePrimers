#=============================================
# reading in my helper functions 
#  and all required libraries
#=============================================
    setwd("C:\\GoogleDrive\\Climate\\Rcode")
    #setwd("C:\\Users\\mallen\\Desktop\\Climate\\Rcode")
    sourceList<-list("ChkLibs.r","ClipToPolygon.r","MyPlotSetup.r","GetParkBoundary.r","GetParkBox.r","PlotClimOverMap.r",
       "GDPNexPrismGrab.r","InputConstants.r","YearlyLinePlot.r","AnomalyPlot.r","ReadClimateNetCDF.r",
       "month.day.year.r","GenerateColors.r","Convert","PlotMappedDataClass","ClassesAndMethods")
    unlist(lapply(sourceList,source))
    
    ChkLibs(list("maptools","rgdal","raster","mapdata","ncdf","fields","maps","RNCEP","rGDP","ggplot2","zoo","XML","RCurl","RColorBrewer"))
    setwd("C:\\GoogleDrive\\Climate")
   
#============================================
# Inputting all data that is required to produce
# the primer 
#============================================
#Park Information
    ParkCode = "YELL"
    ParkName = "Yellowstone National Park"
    NpsShapes ="NPS_boundaries\\nps_boundary.shp" #the folder containg the NPS shape file
  
# GCM Parameters for coarse resolution climate data only
    GDOPath = "C:\\GoogleDrive\\Climate\\bcsd5\\Extraction_tas.nc"   
    ModelKeyPath = "bcsd5\\Projections5.txt" #associates keys with cmip 5 projections for the GDO datasets
    GCMGDO = c('1950-01-01T00:00:00Z','2099-01-01T00:00:00Z') #Year-Month-Day and then time 
# Hydrology paths
    ET<-"ClimateData\\Yellowstone\\hydro3\\Extraction_et.nc"
    SWE<-"ClimateData\\Yellowstone\\hydro3\\Extraction_swe.nc"    
# GCM and Prism pull info    
    Present=2013
    ES = c("rcp26","rcp45","rcp60","rcp85","historical")
    Model = ModelList[[3]] # further subset this based on what I need for now just getting prism
         #Model[[1]]<-ModelList[[1]][c(2,3,65,66,47,48)]
         #Model[[2]]<-ModelList[[2]][c(2,3,65,66,47,48)]
             #the climate models for a given emissions scenario
             #unfortunately models differ for each emissions scenario so get these using
             #getDataIDs(rGDP) once the url is set
    GCMTime = c('2000-01-01T00:00:00Z','2030-12-31T00:00:00Z') #Year-Month-Day and then time 
             #Nex is only 1/14/1950-1/1/2100 best to end on the last day of the year
             #then I don't have to worry about partial years 
    HistoricTime = c('1895-01-01T00:00:00Z','2013-02-1T00:00:00Z')  
  
             #Prism is only 1/1/1895-11/1/2011  
    OutputDir = "C:\\GoogleDrive\\Climate\\ClimateData\\Yellowstone" 
# Parameters for graphics devices    
    MovAvgPeriod = 10 
    #just for working on spatial graphics here are two a high res examples for yellowstone
     INMCM85<-open.ncdf("C:\\GoogleDrive\\Climate\\ClimateData\\Yellowstone\\r1i1p1_inmcm4_tasmin.nc")
     Clim<-open.ncdf("C:\\GoogleDrive\\Climate\\ClimateData\\Yellowstone\\e0c8d36d-1707-4a0f-9e56-231acc78dbd5OUTPUT.d97a2137-85b6-42ee-aad1-bad7c611525a.nc")
    #this is a hillshade we can put under the maps
    baselayer<-"C:\\GoogleDrive\\Climate\\ClimateData\\HillShade\\hs_250.tif"
#===========================================    
#get a bounding box for the park for now later we can just grab the shape 
#============================================ 
  BoundBox <- GetParkBox(Bndry=NpsShapes,ParkCode) 
  Boundary <- GetParkBoundary(Bndry=NpsShapes,ParkCode)  
  
#read in PRISM and NEX CMIP5 for the desired park by submitting the shape file 
#===========================================
#using rGDP
#requestNetCDF(BoundBox,URI,ModelList,Start,End,OutputNcdfPath=filepath(,varname)
#GDPNexPrismGrab(BoundBox,GCMTime,HistoricTime,Model,OutputDir)
 
#This will probably be replaced by downloading directly from the GDO http://gdo-dcp.ucllnl.org/downscaled_cmip_projections/dcpInterface.html#Welcome
# for coarse resolution, hydrology, swe, and mauer 1/8th degree
#into the correctly labeled folder using BlodgettOpenDapAccess with commands similar to BlodgettTest for the NEX and maybe prism as well  
#============================================
# Data formatting steps 
#============================================
Prismppt <- ReadClimateNetCDF(OutputDir,Time=HistoricTime,Model="prism",Var="ppt",Boundary=Boundary)
PrismTmin <- ReadClimateNetCDF(OutputDir,Time=HistoricTime,Model="prism",Var="tmn",Boundary=Boundary)
PrismTmax <- ReadClimateNetCDF(OutputDir,Time=HistoricTime,Model="prism",Var="tmx",Boundary=Boundary)

#============================================
# Producing line plots with rolling averages 
#============================================
TminPlot<-YearlyLinePlot(PrismTmin,MovAvgPeriod=10,Ylab=(expression(paste(Tmin, ~({}^o*C)))),Xlab=(""),MovAvg=FALSE,LM=TRUE,maCol="orange")
pptPlot<-YearlyLinePlot(Prismppt,MovAvgPeriod=10,Ylab="Precip (mm / yr)",Xlab=(""),MovAvg=TRUE,LM=TRUE,maCol="skyblue")
TmaxPlot<-YearlyLinePlot(PrismTmax,MovAvgPeriod=10,Ylab=(expression(paste(Tmax, ~({}^o*C)))),Xlab=(""),MovAvg=TRUE,LM=FALSE,maCol="red")

#can do TmaxPlot<-YearlyLinePlot(PrismTmax,MovAvgPeriod=10,Ylab=(expression(paste(Tmax, ~({}^o*C)))),Xlab=(""),MovAvg=TRUE,LM=FALSE,maCol="red",Months=c(1,2,3))
#for example to get spring trendsTmin

#This little bit if all graphics are to be included in one figure but I think it'se easy enough to move them...
grid.newpage()
pushViewport(viewport(layout=grid.layout(3,1)))
vplayout<- function(x,y)
	viewport(layout.pos.row=x, layout.pos.col=y)
print(TminPlot, vp=vplayout(1,1))
print(pptPlot, vp=vplayout(2,1))
print(TmaxPlot, vp=vplayout(3,1))

#============================================
# Anomaly Plot
#============================================
AnomalyPlot(PrismTmin,Baseline=c(1910,2010),Ylab="Difference from Baseline")

#============================================
# Plotting climate over maps
#============================================
TminDat<-MappedData(SourcePath="C:\\GoogleDrive\\Climate\\ClimateData\\Yellowstone\\r1i1p1_inmcm4_tasmin.nc",
              Time='2008-01-01T00:00:00Z',
               UnitMap=UnitLookup$Nex,
               Var="Tmin",
               PlotUnits="C")
               
Precip<-MappedData(SourcePath="C:\\GoogleDrive\\Climate\\ClimateData\\Yellowstone\\r1i1p1_inmcm4_tasmin.nc",
              Time='2008-01-01T00:00:00Z',
               UnitMap=UnitLookup$Nex,
               Var="Precip",
               PlotUnits="kgm2s1")
Precip<-MappedData(SourcePath="C:\\GoogleDrive\\Climate\\ClimateData\\Yellowstone\\r1i1p1_inmcm4_tasmin.nc",
              Time='2008-01-01T00:00:00Z',
               UnitMap=UnitLookup$Nex,
               Var="Precip",
               PlotUnits="kgm2s1")               

PChng<-Precip
PChng@Var<-"PrecipChng"  
                             
par(mfrow=c(3,2))               
plot(x=TminDat,Bound=Boundary)
plot(x=Precip,Bound=Boundary)
plot(x=PChng,Bound=Boundary)
PChng@Var<-"TempChng" 
plot(x=PChng,Bound=Boundary)
PChng@Var<-"PurOrn" 
plot(x=PChng,Bound=Boundary)
PChng@Var<-"GrnPnk" 
plot(x=PChng,Bound=Boundary)


 
    jet.colors=colorRampPalette( c("white", "deeppink4") )
    color <- jet.colors(20)   #for those of the bloody earth perswasion
  
    #taking 12 months to get an anual average for end of century
    a<-get.var.ncdf(INMCM85,start=c(1,1,1116),count=c(361,217,12))
     #end of century projections
     endCent<-apply(a,c(1,2),mean)-273 #convert to celcius from Kelvin
     #beginning of century projections
    b<-get.var.ncdf(INMCM85,start=c(1,1,1),count=c(361,217,1))
     begCent<-apply(a,c(1,2),mean)-273
    Main<-"2099 Mean Tmin Celcius Projections for INMCM RCP 85"
   
n<-MappedData(SourcePath="C:\\GoogleDrive\\Climate\\ClimateData\\Yellowstone\\r1i1p1_inmcm4_tasmin.nc",
              Time='2008-01-01T00:00:00Z',
               UnitMap=UnitLookup$Nex,
               Var="Temp",
               PlotUnits="C")

plot(n)                
 PlotClimOverMap(Boundary,b,Lon=get.var.ncdf(INMCM85,varid="lon")-LongitudeOffset,Lat=get.var.ncdf(INMCM85,varid="lat"),Main=Main,Border="black",Clip=FALSE,mapType="PrecipChng")
     Main<-"Change in Tmin Mean Anual Projections"
 PlotClimOverMap(Boundary,b,Lon=get.var.ncdf(INMCM85,varid="lon")-LongitudeOffset,Lat=get.var.ncdf(INMCM85,varid="lat"),Colors=heat.colors(25),Border="black",Clip=TRUE)     
    b<-get.var.ncdf(Clim,start=c(1,1,1116),count=c(293,177,1120))
    jet.colors=colorRampPalette( c("azure2", "slateblue3") )
    color <- c("white",jet.colors(20))
 PlotClimOverMap(Boundary,a,Lon=get.var.ncdf(Clim,varid="lon")-LongitudeOffset,Lat=get.var.ncdf(Clim,varid="lat"),Colors=color)      
 
  PlotClimOverMap(Boundary,b,Lon=get.var.ncdf(Clim,varid="lon")-LongitudeOffset,Lat=get.var.ncdf(Clim,varid="lat"),Colors=color,Clip=FALSE,Border="black",Main=ParkName)  
#============================================
# Emissions line and SD plots
#============================================


ToPlot<-ReadGDOClimate(GDOPath,Time=GCMGDO,Boundary,WholeYear=TRUE,LongitudeOffset=LongitudeOffset,varid="tas",ModelKeyPath=ModelKeyPath)
EmissionSDPlot(ToPlot,ParkName,Present)
EmissionLinePlot(ToPlot,ParkName,Present)
       
GCMGDO = c('1950-01-01T00:00:00Z','2099-01-01T00:00:00Z') #Year-Month-Day and then time 
Precip<-ReadGDOClimate("H:\\Desktop\\Climate\\bcsd5\\Extraction_pr.nc",Time=GCMGDO,Boundary,WholeYear=TRUE,LongitudeOffset=LongitudeOffset,varid="pr",ModelKeyPath=ModelKeyPath)
Tavg<-apply(ToPlot[121:149,],2,mean)-mean(apply(ToPlot[1:20,],2,mean))
Pr<-apply(Precip[121:149,],2,mean)-mean(apply(Precip[1:20,],2,mean))
MyPlot(Pr,Tavg,bgCol="grey85",xlab="Average annual Precip.",,ylab="Average Annual Celcius Temp",main="Projected Change from 1950-1969 baseline to 2070-2098")
text(Pr,Tavg,labels=substr(names(Pr),start=1,stop=Tot-6),col=c("green","goldenrod3","orangered","red4")[factor(substr(names(Precip),start=Tot-4,stop=Tot))],cex=1.2)
legend("bottomright",legend= levels(factor(substr(names(Precip),start=Tot-4,stop=Tot))),fill=c("green","goldenrod3","orangered","red4"))


## now subsetting to rcp60
Tot<-nchar(names(Pr))
Pr<-Pr[factor(substr(names(Precip),start=Tot-4,stop=Tot))=="rcp60"]
Tavg<-Tavg[factor(substr(names(Precip),start=Tot-4,stop=Tot))=="rcp60"]
Tot<-nchar(names(Pr))
MyPlot(Pr,Tavg,bgCol="grey85",xlab="Average annual Precip.",,ylab="Average Annual Celcius Temp",main="Projected Change from 1950-1969 baseline to 2070-2098 \nfor RCP60")
text(Pr,Tavg,labels=substr(names(Pr),start=1,stop=Tot-6),col="orangered",cex=1.2)

#================================================================
### now trying to produce some circular plots
ToPlot<-ReadGDOClimate(GDOPath,Time=GCMGDO,Boundary,WholeYear=TRUE,LongitudeOffset=LongitudeOffset,varid="tas",ModelKeyPath=ModelKeyPath,aggrToYear=FALSE)   
#produce several potential graphics and summaries based on this
PlotClimOverMap(Bndy,TempAvg)