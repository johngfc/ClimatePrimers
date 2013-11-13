library(ncdf)
library(fields)
library(maps)
library(RNCEP) #for some nice color scales
#reading in the data this is avgerage temperature I believe but I also have max, min and precip
# this data I down'loaded from a ling on barsuglis page it's cmip 3 projections
#These are cmip3 data from the site:
#http://gdo-dcp.ucllnl.org/downscaled_cmip_projections/dcpInterface.html
#these are cmip5 projections downloaded BY HAND from the above
setwd("H:\\Desktop\\Climate")
#setwd("C:\\Users\\mallen\\Desktop\\VisualizingClimateData\\ClimateData\\SWCo1950To2100")
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


#=============================================================================
# Pulling in the weather data 
# to compare maps and look at baselines
# This is Run after Climate from NCDF so that I can get a baseline to compare with projections
# this is every 6 hours so we'll need to aggregate it
 SoCoWeather <- NCEP.gather(variable='air', level=850,
    months.minmax=c(1,12), years.minmax=c(1950,1970),
    lat.southnorth=range(Lat), lon.westeast=range(Lon),
    reanalysis2 = FALSE, return.units = TRUE)
NCEP.restrict(SoCoWeather,)

YearlyWeather<-NCEP.aggregate(SoCoWeather,YEARS=TRUE,MONTHS=FALSE,DAYS=FALSE,HOURS=FALSE,fxn='mean')
#These dogs put temperature in Kelvin!!! converting to celcius.  Oddly the maps look quite different in celcius and kelvin
#it's interestesting from looking at yearly weather to realize these are matricies of size 9 are there 9 weather stations here?
SpatYearlyWeather<-YearlyWeather-273.15
for(i in 1:21) {
NCEP.vis.area(SpatYearlyWeather,layer=i)
Sys.sleep(1)
}

#getting a baseline weather average from the 1950 to 1970 average
YearlyWeather<-apply(SpatYearlyWeather,3,mean)
baseline<- mean(YearlyWeather)

#===================================
# with regional baseline calculated we can plot

#this is four dimensional lat, lon, time, projection
#we can average over the area and produce line plots for each projection
AreaAvg<-apply(TempAvg,c(3,4),mean)
#then aggregate to year
Yearly<-aggregate(AreaAvg,by=list(Year),FUN=mean)
#remove last year because it's incomplete
Yearly<-Yearly[-c(nrow(Yearly)),]
plot(range(Yearly[,1]),range(Yearly[,2:ncol(Yearly)],na.rm=TRUE),cex=.2)
for (i in 2:ncol(Yearly))
  lines(Yearly[,1],Yearly[,i],col="slateblue")
lines(seq(from=1950,to=1970),YearlyWeather) 
title("Weather Baseline and CMIP5 Don't agree") 
# and isn't that fun the baseline is much much higher than the CMIP5  
#could be that the different spatial extents are causing this  

#===================================
#Ok well let's just assume we could calculate the baseline from weather data and move on...
baseline<-mean(as.matrix(Yearly[1:20,2:ncol(Yearly)]))
ModelKey<-(read.table(ModelKeyPath,stringsAsFactors=FALSE))[,1]
Emissions<-factor(substr(ModelKey,start= nchar(ModelKey)-4,stop= nchar(ModelKey)))
EmissionsCol<-c("cyan","yellow","darkorange","red3")
MyPlot(Yearly[,1],range(Yearly[,-1]),drawGrid=TRUE,cexMult=1,bgCol="grey38")
for (i in 2:ncol(Yearly))
  lines(Yearly[Yearly[Year<=Present],1],Yearly[,i],col=EmissionsCol[Emissions[i-1]],lwd=1)
#abline(h=baseline,lwd=2)
#whatever the hell the mean projection means -replace with weather data
MeanProj<- apply(Yearly[,2:ncol(Yearly)],1,mean)
lines(Yearly[,1],MeanProj,lwd=3,col="yellow")
title("CMIP5 Projections for southern Colorado")
#I wonder if I can break it down by emissions scenario







#TempAvg<-get.var.ncdf(nc=Clim,varid="tas_ss")
colnames(TempAvg)<-seq(1:66)
jpeg("C:\\Users\\mallen\\Desktop\\VisualizingClimateData\\LinePlot.jpeg",width=3000,height=3000,quality=100)

par(mar=c(7,7,5,2),oma=c(11,0,11,0))
ClimateLinePlot(TempAvg[,1:4],Month,Year,Nrow=4,cexMult=3.5)
mtext("CMIP3 Projections",side=3,line=1,outer=TRUE,cex=1.2*3)
dev.off()

#=============================================================================
#### giving up on CMIP5 above
#=============================================================================
library(fields)
library(ncdf)
library(chron)
#=============================================================================
# This dataset happens to be the control experiment
control = open.ncdf(con="C:\\Users\\mallen\\Desktop\\VisualizingClimateData\\ClimateData\\Daily_b06_45.nc", write=FALSE, readunlim=FALSE)

cat(paste(control$filename,"has",control$nvars,"variables"), fill=TRUE)

lonmat  = get.var.ncdf(nc=control,varid="longitude")   # reads entire matrix
latmat  = get.var.ncdf(nc=control,varid="latitude")    # ditto
timearr = get.var.ncdf(nc=control,varid="time")        # reads entire time array

plot( lonmat, latmat, main='The (MM5) grid locations')
US( add=T )

# source summary function look at this for the gory details of accessing
# details of ncdf object

source("summary.ncdf.r")

# this prints out sensible information
summary(control)

#----------------------------------------------------------------------
# Find a day of interest and read the other variables for JUST that day.
# netCDF has some "philosophy" associated with this
# We need to make a "start" and "count" array
#----------------------------------------------------------------------

# It is very common for netCDF files to be created with a monotonic time array.
# Enough information should be given in the time attributes to decode the time
# array into a calendar date. R has several ways to convert calendar dates
# into monotonic time arrays --

# the metadata in the netcdf file indicates the first time is 1 July 1995
# and the base time is in units such that noon on 1 Jan 1601 is t = 0.5.

t1 = julian(x=7, d=1, y=1995,  origin=c(month = 1, day = 1, year = 1601))
t1
timearr[1]

# given that, there's lots of ways to find a specific date

targettime = julian(x=7,d=4,y=1995,origin=c(month = 1, day = 1, year = 1601))
inds       = (1:dim(timearr))
tind       = inds[targettime == timearr]

# tind = 4,  the 4th time point in this case.

ndims    = control$var[['PRCP']]$ndims      # also works (3 in this case)
varsize  = control$var[['PRCP']]$varsize    # 61 49 7

start = c(    1,          1,      tind)     # knowing time is the last dimension
count = c(varsize[1], varsize[2],    1)
# read in this slice of the data

cntlprcp1 = get.var.ncdf(nc=control,varid="PRCP",start,count)

x = 1:nrow(cntlprcp1)   # R plots rows along X axis!
y = 1:ncol(cntlprcp1)
image.plot(x,y,cntlprcp1,col=tim.colors())

# If you want to get 3 timesteps (starting from a date), for example
# This also illustrates a special case if you want ENTIRE dimensions

start = c(   1,  1, tind)
count = c(  -1, -1,   3)
cntlprcp2 = get.var.ncdf(nc=control,varid="PRCP",start,count)
# plot 'em up
set.panel(3,1)
for ( k in 1:3){
image.plot( x,y,cntlprcp2[,,k],col=tim.colors())
}

# an example of subsetting on the other space dimensions.
# Lets assume for a moment that you want to ignore the edges
# i=1,2, N-2,N-1,N ...
# (for just one timestep, you get the picture)

start = c(      3,            3,        tind)
count = c(  varsize[1]-5, varsize[2]-5,    1)
cntlprcp3 = get.var.ncdf(nc=control,varid="PRCP",start,count)

#
# Grabbing a time series for just one location (25,30)
# lon,lat location: lonmat[25,30],latmat[25,30]

start = c(      25,         30,       1)
count = c(  1, 1, -1)
cntltmax = get.var.ncdf(nc=control,varid="TMAX",start,count)
plot( cntltmax, type="h")

# The result of this matches the comments above ...
month.day.year(t1, origin=c(month = 1, day = 1, year = 1601) )
#========================================================================
# station data example
station = open.ncdf(con="C:\\Users\\mallen\\Desktop\\VisualizingClimateData\\ClimateData\\STN_050258.nc", write=FALSE, readunlim=FALSE)

cat(paste(station$filename,"has",station$nvars,"variables"), fill=TRUE)

lons    = get.var.ncdf(nc=station,varid="longitude")   # reads entire coordinate variable
lats    = get.var.ncdf(nc=station,varid="latitude")    # kinda boring, since the station
elev    = get.var.ncdf(nc=station,varid="elevation")   # didn't move. They usually do.
timearr = get.var.ncdf(nc=station,varid="time")        # reads entire time array
prcp    = get.var.ncdf(nc=station,varid="PRCP")        # reads entire precip array
tmin    = get.var.ncdf(nc=station,varid="TMIN")
tmax    = get.var.ncdf(nc=station,varid="TMAX")
tobs    = get.var.ncdf(nc=station,varid="TOBS")
snow    = get.var.ncdf(nc=station,varid="SNOW")

# plot 'em up
set.panel(5,1)
plot( timearr, snow, main='SNOW' )
plot( timearr, prcp, main='PRCP')
plot( timearr, tmin, main='TMIN')
plot( timearr, tmax, main='TMAX')
plot( timearr, tobs, main='TOBS')

# note that there are many more observations of snow and precip than temperature.
# The unobserved temperature data must be coded as 'missing' to use the same time
# coordinate variable.


