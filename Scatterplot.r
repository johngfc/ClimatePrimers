
Scatter<-function(Tavg,Pr,Baseline=c(1950,1980),PlotTime=c(2050,2060),rcp,DisplayOutput,OutputGraphics,Main,cexMult){
 # compare the Average temperature and preciptation for a historic baseline to projections
 # under the selected RCPs
      if(missing(rcp)) rcp=c("rcp45","rcp85") 
      if(missing(Main)) Main=paste("Projected Change from",Baseline[1],"to",Baseline[2], "baseline to",
             PlotTime[1],"to",PlotTime[2], "\nfor", paste(rcp,collapse=","))
       if(!DisplayOutput){ png(file.path(OutputGraphics,
             paste("RCP", paste(rcp,collapse="_"),"Scatter.png",sep="_")),height=1000,width=1000)
              on.exit(dev.off()) 
        }
      # aggregate to years        
      Tavg<-YrAgg(Tavg)
      Pr<-YrAgg(Pr)
      
      BasePeriod<-Pr@Year>=Baseline[1] & Pr@Year<Baseline[2]
      PlotPeriod<-Pr@Year>=PlotTime[1] & Pr@Year<PlotTime[2]
     
       # aggregate the data to the PlotTime for each model and divide by the average of everything
       # in the baseline period
      PrDat<-apply(Pr@Ts[PlotPeriod,Pr@Rcp%in%rcp],2,mean)/mean(Pr@Ts[BasePeriod,Pr@Rcp%in%rcp])
      TDat<-apply(Tavg@Ts[PlotPeriod,Tavg@Rcp%in%rcp],2,mean)/mean(Tavg@Ts[BasePeriod,Tavg@Rcp%in%rcp])
      
      MyPlot(TDat,PrDat,bgCol="grey85",ylab="Average annual Precip.",xlab="Average Annual Celcius Temp",main=Main)
      text(TDat,PrDat,labels=Pr@Proj[Pr@Rcp%in%rcp],col=c("green","goldenrod3","orangered","red4")[Pr@Rcp[Pr@Rcp%in%rcp]],cex=1.2)
      if(length(rcp>1)) legend("bottomright",legend=levels(Pr@Rcp)[levels(Pr@Rcp)%in%rcp],
                          fill=c("green","goldenrod3","orangered","red4")[levels(Pr@Rcp)%in%rcp])

}