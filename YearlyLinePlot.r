YearlyLinePlot<-function(InputDat,MovAvgPeriod=10,Bound,MovAvg=FALSE,LM=TRUE,Ylab,Xlab){
    # in order to produce some line plots we need to do a couple things
    # remove incomplete years
    # clip each month to the correct shape
    # aggregate over year
    # then we can plot the data with our moving average
    Year<-floor(as.numeric(dimnames(InputDat)[[3]]))
    if(any(table(Year)!=12)){ #we get some funkiness averaging over partial years
                              #so remove them here
                              browser() #test this
        InputDat<-InputDat[,,-c(which(Year%in%(names(table(Year))[table(Year)!=12]),arr.ind=TRUE))]
        }
        #I might want to move clipping out where I can use it often
        for (i in 1:dim(InputDat)[3]){
        a<-ClipToPolygon(as.numeric(dimnames(InputDat)[[1]]),as.numeric(dimnames(InputDat)[[2]]),(InputDat[,,i]),Boundary)
        if(i==1)
         ClippedDat<-a@data
        else
          ClippedDat<-cbind(ClippedDat,a@data)
        }  
    # clips a 2 dimensional raster to the shape
    # the output is a spatial pixels dataframe
    
    # @data contains the data of the clipped a
       #now average over the months for each year and over the pixels
    YearlyPkAvg <- apply(aggregate(t(ClippedDat),FUN=mean,by=list(Year=Year)),1,mean)
    rollAvg <- c(rep(NA,times=MovAvgPeriod-1),rollmean(YearlyPkAvg, MovAvgPeriod))
    YearDat<-data.frame(Year=unique(Year),YearlyPkAvg=YearlyPkAvg,rollAvg=rollAvg)
    
    PlotOut <- ggplot(aes(Year, YearlyParkAvg), data=YearDat) + geom_line() + geom_point() +
    		theme(axis.text.y = element_text(size = 7)) +
    		theme(axis.title.y = element_text(size = 10, angle = 90)) +
    		ylab(Ylab) + xlab(Xlab) +
    		scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000)) 
    		
    		if(LM) PlotOut <- PlotOut + geom_smooth(method="lm")
        if(MovAvg) PlotOut <- PlotOut + geom_line(aes(y=rollAvg),colour="red",size=2)
		return(PlotOut)
}

