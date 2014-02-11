
ImagePlot<-function(InputDat,Ylab,Xlab,Plot=TRUE,Months,Colors,Baseline=c(1,30),Main="",ColVar="TempChng",cex.mult=1){
#this doesn't look great for my current dataset but maybe eventually 
    Year<-unique(round(as.numeric(names(InputDat))))
    if(as.numeric(names(InputDat)[1])%%1==0) InputDat<-matrix(data=InputDat,nrow=12,byrow=FALSE)
    ID<-(InputDat-apply(InputDat[,Baseline[1]:Baseline[2]],1,mean)) #/apply(InputDat,1,sd)
    if(missing(Colors)) Colors<-GenerateColors(ColVar)
    Colors=two.colors(n=256, start=Colors[1], end=Colors[length(Colors)], middle="gray89",alpha=1.0)
    Breaks<-SetBreaks(ID,"diff",Colors)
    image.plot(z=ID,x=seq(1:12),y=Year,xlab="Month",col=Colors,main=Main,cex.lab=cex.mult,cex.axis=cex.mult,legend.mar=7.1,breaks=Breaks)    
}

