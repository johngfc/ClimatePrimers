 Year <- floor(as.numeric(names(InputDat)))
InputDat<-PrismTmin
 Yr10 <- signif(Year,digits=3) 
  boxplot(InputDat~Year)
 Ylab<-(expression(paste(Tmin, ~({}^o*C)))) 
 par(mfrow=c(2,2),mar=c(5,5,3,1),oma=c(0,0,4,0))
 Months<-list(Winter=c(12,1,2),Spring=c(3,4,5),Summer=c(6,7,8),Autumn=c(9,10,11))
 Col<-c("royalblue4","springgreen4","red","chocolate4")
 
 
 for(i in 1:4){
     Month <- as.numeric(names(InputDat))%%1 
     Keep <- round(Month,digits=3)%in%round(c((Months[[i]]-1)/12),digits=3)
     InputDat2 <- InputDat[Keep]
     YrKeep<-Yr10[Keep]
      jet.colors=colorRampPalette( c("white", Col[i]) )
      MyPlot(YrKeep,InputDat2,bgCol="gray85",xlab="",ylab="") 
  boxplot(InputDat2~YrKeep,col=jet.colors(length(table(Yr10))),main=names(Months)[i],ylab=Ylab,add=TRUE,at=unique(YrKeep),boxwex=8,xlab="Year")
  mtext("Trends in Minimum Monthly Temperature",side=3,outer=TRUE,cex=2)
  }
