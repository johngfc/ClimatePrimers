#TempAvg is an array dimensions longitude,lat,time, and projection the first has 112 time steps 
#unfortunately these projections don't list their emissions scenario...
AvgTemp<-open.ncdf("1_8obs\\Extraction_Tavg.nc")
ModelCode<-read.table("H:\\Desktop\\Climate\\bcsd5\\Projections5.txt",as.is=TRUE)

#The information on the emissions scenario isn't in the .nc file so I match with the Projections file
#and create a factor
f<-function(x){
 l<-nchar(x)
 substr(x,(l-4),l)
 }
Emissions<-as.factor(apply(ModelCode,1,f))

#this is four dimensional lat, lon, time, projection
#we can average over the area and produce line plots for each projection
AreaAvg<-apply(TempAvg,c(3,4),mean)

#then aggregate to year
Yearly<-aggregate(AreaAvg,by=list(Year),FUN=mean)
#remove last year because it's incomplete
Yearly<-Yearly[-c(nrow(Yearly)),]
MyPlot(range(Yearly[,1]),range(Yearly[,2:ncol(Yearly)],na.rm=TRUE),bgCol="gray23")

for (i in 2:ncol(Yearly))
  lines(Yearly[,1],Yearly[,i],col=c("pink2","orange","yellow","darkolivegreen1")[Emissions[i]])


lines(seq(from=1950,to=1970),YearlyWeather) 
title("Weather Baseline and CMIP5 Don't agree") 