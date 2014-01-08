
ReadClimateNetCDF<-function(OutputDir,Time,Model,Var,Boundary,WholeYear=TRUE,CollapsePixels=TRUE){
    # This function reads climate data from netcdf into a 3 dimensional array 
    # it then clips to the desired shape file
    # optionally it can remove partial years and collapse the pixels within the shape to return one value for each month 
    
    # OutputDir = the directory where we wrote the Clim output
    # Time = specified in Generate Climate Primer 
    # Model = is either Prism or an emissions scenario at present
    # var = is the specific variable for a given model for Prism it's ppt tmax timin
    #       for others it's actually the model for the emissions scenario
    
     Clim<-open.ncdf(file.path(OutputDir,"ncdf",
                  paste(Model,Var,".nc",sep=""))) 
     Lat<-get.var.ncdf(Clim,"lat")
     Lon<-get.var.ncdf(Clim,"lon")
        
      #Time is recorded as days since 1858/11/17 for prism so to calcuate current time I can use something like
      #(Time)/365.25+1858.8794521  but it's easier just to parse the time range and generate a sequence
      # not deal with leap years and rounding blah, blah, blah...
     
      Time = seq(from = (as.numeric(substr(Time[1],1,4))+as.numeric(substr(Time[1],6,7))/12),
          to = (as.numeric(substr(Time[2],1,4))+as.numeric(substr(Time[2],6,7))/12),
          by = 1/12)-1/12 #so january ends up being a round number and decmber is in ther current year
      # Check that I have the right length at least
       
         tm<- get.var.ncdf(Clim,"time")

     if(length(Time)!=length(get.var.ncdf(Clim,"time"))) stop("time dimensions are wrong")
    RastArray<-get.var.ncdf(nc=Clim)
    dimnames(RastArray)[[1]]<-Lon
    dimnames(RastArray)[[2]]<-Lat
    dimnames(RastArray)[[3]]<-Time
  
   if(WholeYear){ #remove years for which we only have a portion of the year
       Year<-floor(as.numeric(dimnames(RastArray)[[3]]))
      if(any(table(Year)!=12)){ #we get some funkiness averaging over partial years
                                #so remove them here
                              
          RastArray<-RastArray[,,-c(which(Year%in%(names(table(Year))[table(Year)!=12]),arr.ind=TRUE))]
          Time<-Time[-c(which(Year%in%(names(table(Year))[table(Year)!=12]),arr.ind=TRUE))]
          Year<-Year[-c(which(Year%in%(names(table(Year))[table(Year)!=12]),arr.ind=TRUE))]
          }
     }   
    for (i in 1:dim(RastArray)[3]){
        a<-ClipToPolygon(as.numeric(dimnames(RastArray)[[1]]),as.numeric(dimnames(RastArray)[[2]]),(RastArray[,,i]),Boundary)
        if(i==1)
           ClippedDat<-a@data
        else
           ClippedDat<-cbind(ClippedDat,a@data)
    }  
    colnames(ClippedDat)<-Time
    if(CollapsePixels) ClippedDat<-apply(ClippedDat,2,mean)
   return(ClippedDat) 
}

   
  