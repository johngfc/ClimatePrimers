GCMGDO = c('1950-01-01T00:00:00Z','2099-01-01T00:00:00Z') #Year-Month-Day and then time
Precip<-ReadGDOClimate("bcsd5\\Extraction_pr.nc",Time=GCMGDO,Boundary,WholeYear=TRUE,LongitudeOffset=360,varid="pr",ModelKeyPath=ModelKeyPath,Clip=FALSE)

Tavg<-apply(ToPlot[121:149,],2,mean)-mean(apply(ToPlot[1:20,],2,mean))

Pr<-apply(Precip[121:149,],2,mean)-mean(apply(Precip[1:20,],2,mean))
MyPlot(Tavg,Pr,bgCol="grey85",ylab="Average annual Precip.",xlab="Average Annual Celcius Temp",main="Projected Change    from 1950-1969 baseline to 2070-2098")
Tot=nchar(names(Pr))
text(Pr,Tavg,labels=substr(names(Pr),start=1,stop=Tot-6),col=c("green","goldenrod3","orangered","red4")[factor(substr(names(Precip),start=Tot-4,stop=Tot))],cex=1.2)
legend("bottomright",legend= levels(factor(substr(names(Precip),start=Tot-4,stop=Tot))),fill=c("green","goldenrod3","orangered","red4"))

## now subsetting to rcp60
Tot<-nchar(names(Pr))
Pr<-Pr[factor(substr(names(Precip),start=Tot-4,stop=Tot))=="rcp60"]
Tavg<-Tavg[factor(substr(names(Precip),start=Tot-4,stop=Tot))=="rcp60"]
Tot<-nchar(names(Pr))
MyPlot(Pr,Tavg,bgCol="grey85",xlab="Average annual Precip.",,ylab="Average Annual Celcius Temp",main="Projected Change from 1950-1969 baseline to 2070-2098 \nfor RCP60")
text(Pr,Tavg,labels=substr(names(Pr),start=1,stop=Tot-6),col="orangered",cex=1.2)