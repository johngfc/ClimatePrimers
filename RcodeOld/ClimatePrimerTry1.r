#Export_out.shp contains all of the National Park bounaries
#NPSunits(2).csv has the lookup codes for each national park
#NPS shape data is in the geodatbase K:\GIS_LIBRARY\GAP\National\PADUS
library(maptools)
library(rgdal)
library(raster)
library(OpenStreetMap)
library(mapdata)

setwd("H:\\Desktop\\Climate")


#===============================================================
# reading in the shape file for Yellowstone
#===============================================================
a<-readShapePoly("H:\\Desktop\\Climate\\Boundary2\\Export_output.shp")
a<-a[-c(1,2),]
NP<-a[a$S_Loc_Nm=="YELL",]

#================================================================
#converting the utm to lat lon for entering in the CMIP5 database
#NPS shapefile Projection:USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
#"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
#this should be correct for all the shape files

#================================================================
ex<-extent(NP)
SP <- SpatialPoints(cbind(c(ex@xmin,ex@xmin,ex@xmax,ex@xmax), c(ex@ymin,ex@ymax,ex@ymin,ex@ymax)), 
  proj4string=CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"))
Bounds<-spTransform(SP, CRS("+proj=longlat"))

#================================================================
#Plot a google map underneath the region
#map<-openmap(Bounds[2,c(2,1)],bounds
#================================================================
map <- openmap(c(46,-112), c(42,-108),type="bing")
map_longlat<-openproj(map,projection="+proj=longlat")
plot(map_longlat,raster=TRUE)
map("state",col="yellow",add=TRUE,lwd=2)

#================================================================
#untar the cmip5 this works but is extremely slow
#untar("YellowStoneClimateData\\1_8obs.tar.gz")
#untar("YellowStoneClimateData\\bcsd5.tar.gz")
#I have to set the projection which isn't recorded correctly in the shp file
#================================================================
  proj4string(NP)<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
Bounds<-spTransform(NP, CRS("+proj=longlat")) 
ex<-extent(Bounds)

#================================================================
# clipping TempAvg requires knowing it's imensions then setting up 
# the spatial points data frame for the desired time and projection
#      TempAvg[Long,Lat,time,Proj]
#================================================================
ClippedClim<- ClipToPolygon(Lon,Lat,TempAvg[,,1,1],Bounds)
image(ClippedClim,add=TRUE)
plot(Bounds,add=TRUE,lwd=5,border="blue")


setwd("H:\\Desktop\\Climate")
setwd("C:\\Users\\mallen\\Desktop\\VisualizingClimateData\\ClimateData\\SWCo1950To2100")
Clim<-open.ncdf("bcsd5\\Extraction_tas.nc")
LongitudeOffset<-360 #in the last dataset 150 worked I'm not sure why
Region=c("wyoming","montana")
#===============================
# Extracting info
Lat<-Clim$dim$latitude$vals
Lon<-(Clim$dim$longitude$vals-LongitudeOffset) # I really have no idea how the longitudes got changed...
Time<-Clim$dim$time$vals
Year<-1950+floor(Time/365)
Month<-rep(c("Jan","Feb","March","April","May","June",
   "July","Aug","Sep","Oct","Nov","Dec"),times=length(unique(Year)))

#clip off the extra if the last year wasn't complete
Month<-Month[1:length(Year)]

#these are projections from 43 climate models 
TempAvg<-get.var.ncdf(nc=Clim,varid="tas")

#===============================
#getting ready for plotting
ColorScale<-tim.colors()
cut(range(range(TempS[,,i],na.rm=TRUE),range(TempM[,,i],na.rm=TRUE)))



#for any of these we can plot the spatial projection I'm doing the first and last
image.plot(TempAvg[,,1,1],col=tim.colors())
image.plot(TempAvg[,,dim(TempAvg)[3],dim(TempAvg)[4]],col=tim.colors())


#==============================================================
# map with heat image  
 #not that there is any reasonable justificatin for using 1950 to 1970 as the baseline
 #but we can do that to 
map("state",region = Region)
rect(-101.125,35,-106.125,40,col="blue") #weather data boundaries
image(rev(Lon),Lat,TempAvg[,,1,1],add=TRUE)
title("Colorado Boundary and \nRegion of Analysis\nblue square is weather boundary")