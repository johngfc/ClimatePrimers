
###Preparing the data
setwd("N:\\Research\\nccsc\\Private\\Projects\\VisTrails\\DevelopmentWorkspace\\Marian\\Workspace\\GeoDataDebugging")
ppt<-read.csv("PrismPpt.csv",header=TRUE,skip=3,col.names=c("TimeStep","PrecipJE","PrecipSW"))
tmn<-read.csv("PrismTmn.csv",header=TRUE,skip=3,col.names=c("TimeStep","MinTempJE","MinTempSW"))
tmx<-read.csv("PrismTmx.csv",header=TRUE,skip=3,col.names=c("TimeStep","MaxTempJE","MaxTempSW"))
StartYear = 1950
Dat<-merge(ppt,tmn)
Dat<-merge(Dat,tmx)

YearSplit<-do.call("rbind",(strsplit(as.character(Dat$TimeStep),split="-")))
Dat$Year<-as.numeric(as.character(YearSplit[,1]))
Dat$Month<-as.numeric(as.character(YearSplit[,2]))
Dat<-Dat[Dat$Year>StartYear,]
###Calculating monthy mean and sd
MonthlyAvg<-aggregate(Dat[,2:7],by=list(Dat$Month),FUN=mean)
MonthlySd<-aggregate(Dat[,2:7],by=list(Dat$Month),FUN=sd)

############################################
#### playing with some plots to see what looks good

jpeg(file=paste("N:\\Research\\nccsc\\Private\\Projects\\VisTrails\\DevelopmentWorkspace\\Marian\\Workspace\\GeoDataDebugging","1.jpg",sep="\\"),
width=1000,height=1000,pointsize=13,quality=100)
ColorScale<-rev(heat.colors(floor(nrow(Dat)/12)))
par(mfrow=c(3,1),mar=c(1,4,0,2),oma=c(10,4,6,2))
#######################
ToPlot<-c(2,4,6)
Title<-"Low River Report"
########################
ToPlot<-c(3,5,7)
Title<-"Can't Weld"
#######################
PlotNames<-names(Dat)
PlotNames<-sub("JE","",PlotNames)
PlotNames<-sub("SW","",PlotNames)

cexMult<-3
jpeg(file=paste("N:\\Research\\nccsc\\Private\\Projects\\VisTrails\\DevelopmentWorkspace\\Marian\\Workspace\\GeoDataDebugging","1.jpg",sep="\\"),
width=2000,height=3000,pointsize=13,quality=100)

par(mfrow=c(3,1),mar=c(3,16,0,4),oma=c(13,6,15,2))
for(i in ToPlot){
  plot(c(1.25,11.75),range(Dat[,i]),type="n",ylab=PlotNames[i],xaxt="n",xlab="",cex.lab=1.5*cexMult,cex.axis=cexMult)
  xLim<-extendrange(c(1.25,11.75),f=.04)
  yLim<-extendrange(Dat[,i],f=.04)
  rect(xLim[1],yLim[1],xLim[2],yLim[2],col="grey39")

  if(i==min(ToPlot)){
        boxSeq<-seq(from=150,to=200,length=length(ColorScale))
      for(k in 1:length(ColorScale)){
          rect(11.5,boxSeq[k],12,boxSeq[k]+5,col=ColorScale[k],lty="blank")
          if(unique(Dat$Year)[k]%%10==0) text(11.25,boxSeq[k]+2.5,as.character(unique(Dat$Year)[k]),col="white",cex=cexMult)
          }
  }
  for(j in 1:floor(nrow(Dat)/12)){
      Strt<-(j*12)-11
      End<-(j*12)
      lines(seq(1:12),Dat[seq(from=Strt,to=End),i],col=ColorScale[j],lwd=cexMult)
    #  segments(x0=c(1:11),y0=Dat[Strt:(End-1),i],
 # x1=(2:12),y1=Dat[(Strt+1):End,i],col=ColorScale[j],lwd=1.5) 
    }
      #for the last incomplete yera
     segments(x0=c(1:7),y0=Dat[(End+1):(nrow(Dat)-1),i],
  x1=(2:8),y1=Dat[(End+2):nrow(Dat),i],col=ColorScale[j],lwd=2) 
}
mtext(c("Jan","Feb","March","April","May","June",
   "July","Aug","Sep","Oct","Nov","Dec"),side=1,line=1,at=seq(1:12),las=2,cex=cexMult)
mtext("Month",side=1,line=10,outer=TRUE,cex=1.5*cexMult) 
mtext(Title,side=3,line=6,outer=TRUE,cex=1.7*cexMult)
mtext("Average Monthly Values for the County",side=3,line=1,outer=TRUE,cex=1.2*cexMult)
   
dev.off()

#==============================================================================#
### Trying to plot the most recent year of data with the lines for mean and sd ###
 ggplot(aes(x=seq(1:12),y=Dat[1:12,i]))+geom_ribbon(aes(ymin=MonthlyAvg[,i]-MonthlySd[,i],ymax=MonthlyAvg[,i]+MonthlySd[,i]),alpha=.2)+geom_line()



############### Now look at differences from monthly mean
jpeg(file=paste("N:\\Research\\nccsc\\Private\\Projects\\VisTrails\\DevelopmentWorkspace\\Marian\\Workspace\\GeoDataDebugging","2.jpg",sep="\\"),
width=1000,height=1000,pointsize=13,quality=100)
par(mfrow=c(3,1),mar=c(0,4,0,2),oma=c(10,4,6,2))
for(i in ToPlot){
  plot(c(1.25,11.75),range(Dat[,i]-c(rep(MonthlyAvg[,i],times=floor(nrow(Dat)/12)),MonthlyAvg[1:8,i])),
    type="n",ylab=PlotNames[i],xaxt="n",cex.lab=1.5)
  if(i==min(ToPlot)){
        boxSeq<-seq(from=50,to=100,length=length(ColorScale))
      for(k in 1:length(ColorScale)){
          rect(11.5,boxSeq[k],12,boxSeq[k]+5,col=ColorScale[k],lty="blank")
          if(unique(Dat$Year)[k]%%10==0) text(11.25,boxSeq[k]+2.5,as.character(unique(Dat$Year)[k]))
          }
  }
  for(j in 1:floor(nrow(Dat)/12)){
      Strt<-(j*12)-11
      End<-(j*12)
      segments(x0=c(1:11),y0=Dat[Strt:(End-1),i]-MonthlyAvg[1:11,i],
  x1=(2:12),y1=Dat[(Strt+1):End,i]-MonthlyAvg[2:12,i],col=ColorScale[j],lwd=1.5) 
    }
      #for the last incomplete year
     segments(x0=c(1:7),y0=Dat[(End+1):(nrow(Dat)-1),i]-MonthlyAvg[1:7,i],
  x1=(2:8),y1=Dat[(End+2):nrow(Dat),i]-MonthlyAvg[2:8,i],col=ColorScale[j],lwd=2) 
}
mtext(c("Jan","Feb","March","April","May","June",
   "July","Aug","Sep","Oct","Nov","Dec"),side=1,line=1,at=seq(1:12),las=2)
mtext("Month",side=1,line=6,outer=TRUE,cex=1.5) 
mtext(Title,side=3,line=3,outer=TRUE,cex=1.7)
mtext("Difference from Mean Monthly Values for the County",side=3,line=1,outer=TRUE,cex=1.2)   

############################################################################
###### CUSUM
i=2
for(i in ToPlot){
AbbDat<-(Dat[,i]-c(rep(MonthlyAvg[,i],times=floor(nrow(Dat)/12)),MonthlyAvg[1:8,i]))
q <- cusum(AbbDat[1:500], newdata=AbbDat[501:length(AbbDat)])
}


#### Gam fits don't look great
par(mfrow=c(3,2))
for(i in 2:ncol(MonthlyAvg)){
    gam.obj<-gam(Dat[,i]~s(Month,6),data=Dat)
    plot(gam.obj,se=TRUE,main=names(Dat)[i],col="blue",lwd=1.5,rugplot=FALSE)
    segments(x0=c(1:11),y0=Dat[(132:142),i],
  x1=(2:12),y1=Dat[(133:143),i],col="red")  
}

#### this looks like crap!
plot(c(1,12),range(MonthlyAvg[,i]),type="n")
segments(x0=seq(from=0,to=12,length=1200),x1=seq(from=0,to=12,length=1200),
    y0=rep(MonthlyAvg[1:(nrow(MonthlyAvg)-1),i]-MonthlySd[1:(nrow(MonthlyAvg)-1),i],each=100),
    y1=rep(MonthlyAvg[1:(nrow(MonthlyAvg)-1),i]+MonthlySd[1:(nrow(MonthlyAvg)-1),i],each=100)) 
segments(x0=c((1:11),1:7))y0=MonthlyAvg[1:(nrow(MonthlyAvg)-1),i],
  x1=c((2:12),2:8),y1=MonthlyAvg[2:143,i],col="red",lwd=3,rugplot=FALSE)     
}
plot(seq(1:length(ppt[,2])),ppt[,2])