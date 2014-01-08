InputDat<-PrismTmin
ImagePlot<-function(InputDat,Ylab,Xlab,Plot=TRUE,Months){
#this doesn't look great for my current dataset but maybe eventually 
    Year<-unique(round(as.numeric(names(InputDat))))
    if(as.numeric(names(InputDat)[1])%%1==0) InputDat<-matrix(data=InputDat,nrow=12,byrow=FALSE)
    ID<-(InputDat-apply(InputDat,1,mean))/apply(InputDat,1,sd)
    image.plot(z=ID,y=Year,xlab="Month",xaxt="n")
}