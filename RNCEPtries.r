#This is Run after Climate from NCDF so that I can get a baseline to compare with projections
#this is every 6 hours so we'll need to aggregate it
 SoCoWeather <- NCEP.gather(variable='air', level=850,
    months.minmax=c(1,12), years.minmax=c(1950,1970),
    lat.southnorth=range(Lat), lon.westeast=range(Lon),
    reanalysis2 = FALSE, return.units = TRUE)
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
