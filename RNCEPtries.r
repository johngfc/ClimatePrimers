#This is Run after Climate from NCDF so that I can get a baseline to compare with projections
#this is every 6 hours so we'll need to aggregate it
#this is pretty coarse resolution only 9 points in yellowstone 
Strt<-Sys.time()  
 Weather <- NCEP.gather(variable='air.sig995', level='surface',
    months.minmax=c(1,12), years.minmax=c(1950,1970),
    lat.southnorth=range(BoundBox@coords[,2]), lon.westeast=range(BoundBox@coords[,1]),
    reanalysis2 = FALSE, return.units = TRUE)
TotTime<-Sys.time()-Strt
TotTime

TimeSteps<-unlist(dimnames(Weather)[3])
Year<-factor(substr(TimeSteps,start=1,stop=4))
Month<-factor(substr(TimeSteps,start=6,stop=7))
a<-aggregate(Weather[1,1,],FUN=max,by=list(Month,Year))
#we're trying to get to monthly max, monthly min
    
NCEP.restrict(SoCoWeather,)
CelciusCnvt<-function(x){
YearlyWeather<-NCEP.aggregate(SoCoWeather,YEARS=TRUE,MONTHS=FALSE,DAYS=FALSE,HOURS=FALSE,fxn='mean')
#These dogs put temperature in Kelvin!!! converting to celcius.  Oddly the maps look quite different in celcius and kelvin
YearlyWeather<-YearlyWeather-273.15
for(i in 1:21) {
NCEP.vis.area(YearlyWeather,layer=i)
Sys.sleep(1)
}

#getting a baseline weather average from the 1950 to 1970 average
baseline<- mean(YearlyWeather)
