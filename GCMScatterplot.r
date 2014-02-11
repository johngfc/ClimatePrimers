 GCMTmax<-GDOAvgTmax
 GCMTmin<-GDOAvgTmin
 xlabel<-"Tmin"
 ylabel<-"Tmax"


GCMScatterplot<-function(GCMTmax,GCMTin,timeBreaks=c(1975,2050),Col=c("magenta","slateblue","olivedrab1"),MonthSubset=seq(1:12)){

year <- round(as.numeric(rownames(GCMPrecip)))
month <- rownames(GCMPrecip)%%1

#incrasing the transparance
color.box<-col2rgb(Col,alpha=TRUE)
                           color.box[4,]<-50
                           temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
                           Col<-apply(color.box/255,2,temp.fct)
MyPlot(GCMTmax,GCMTmin,drawGrid=TRUE,cexMult=1,bgCol="grey48",
        main=paste("Climate Space Over Time",ParkName,"\nUnder Four Emissions Scenarios",sep=""),xlab=xlabel,ylab=ylabel)

points(as.vector(GCMTmax),as.vector(GCMTmin),col=rep(Col[cut(year,2)],times=ncol(GCMTmin)),pch=19,cex=.05)
}
