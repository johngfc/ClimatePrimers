YearlyLinePlot<-function(InputDat,MovAvgPeriod=10,MovAvg=FALSE,LM=TRUE,LMPeriod,Ylab,Xlab,Plot=TRUE,maCol,Months,Main="",cexMult,DisplayOutput,OutputGraphics){
    # in order to produce some line plots we need to do a couple things
    # remove incomplete years
    # clip each month to the correct shape
    # aggregate over year
    # then we can plot the data with our moving average
    # InputDat     =  a vector with the value names ar year with decimal 0:11/12
    # MovAvgPeriod = the number of years for which to produce a lagging average
    # LM           = Boolean indicating if a linear trend with error bars should be added
    # Ylab         = Character string y lab
    # Xlab         = Character string x lab
    # Plot         = boolean indicating if we'd like to plot it now
    # maCol        = color for the moving avg line
    # Months       = Produce the plot only on the specified months c(1,2,3) for example
    
    # @data contains the data of the clipped a
       #now average over the months for each year and over the pixels
    if(missing(Ylab)) Ylab=GenerateLab(InputDat)
    if(missing(Main)) Main=paste(ParkName,"\n Historic ",LongName(InputDat@Var),sep=" ")
  
    if(!DisplayOutput){ png(file.path(OutputGraphics,
       paste(InputDat@Var,min(InputDat@Year),"to",max(InputDat@Year),"Line.png",sep="_")),height=1000,width=1000) 
       on.exit(dev.off())
       }
    if(missing(Months)) Months=seq(1:12)
    
     Keep <- InputDat@Month%in%Months
     
    YearlyPkAvg <- aggregate(InputDat@Ts[Keep],FUN=mean,by=list(Year=InputDat@Year[Keep]))[,2]
    rollAvg <- c(rep(NA,times=MovAvgPeriod-1),rollmean(YearlyPkAvg, MovAvgPeriod))
    YearDat<-data.frame(Year=unique(InputDat@Year[Keep]),PkAvg=YearlyPkAvg,rollAvg=rollAvg)
    
    PlotOut <- ggplot(aes(Year, PkAvg), data=YearDat) + geom_line() + geom_point() +
    		theme(axis.text.y = element_text(size = rel(cexMult))) +
    		theme(axis.text.x = element_text(size = rel(cexMult))) +
    		theme(axis.title.y = element_text(size = rel(cexMult), angle = 90)) +
    		theme(axis.title.x = element_text(size = rel(cexMult)))+
    		theme(plot.title =element_text(size=rel(1.5*cexMult)))+
    		theme(plot.margin =unit(c(10,10,10,10),"mm"))+
    		ylab(Ylab) + xlab(Xlab) + ggtitle(Main)+
    		scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000)) 
    		
    		if(LM) PlotOut <- PlotOut + geom_smooth(method="lm")
        if(MovAvg) PlotOut <- PlotOut + geom_line(aes(y=rollAvg),colour=maCol,size=2)
        if(Plot)plot(PlotOut)
    
		return(PlotOut)
}

