
ReadGDOClimate<-function(OutputFile,Time,Boundary,WholeYear=TRUE,varid,LongitudeOffset,aggrToYear=TRUE,ModelKeyPath){
    # This function reads climate data from netcdf into a 3 dimensional array 
    # it then clips to the desired shape file
    # optionally it can remove partial years and collapse the pixels within the shape to return one value for each month 
    
    # OutputDir = the directory where we wrote the Clim output
    # Time = specified in Generate Climate Primer 
    # Model = is either Prism or an emissions scenario at present
    # var = is the specific variable for a given model for Prism it's ppt tmax timin
    #       for others it's actually the model for the emissions scenario
  
          Clim<-open.ncdf(file.path(OutputFile))
          Lat<-Clim$dim$latitude$vals
          Lon<-Clim$dim$longitude$vals-LongitudeOffset  
          Time = seq(from = (as.numeric(substr(Time[1],1,4))+as.numeric(substr(Time[1],6,7))/12),
             to = (as.numeric(substr(Time[2],1,4))+as.numeric(substr(Time[2],6,7))/12),
             by = 1/12)-1/12 #so january ends up being a round number and decmber is in ther current year
      # Check that I have the right length at least
     
     if(length(Time)!=length(Clim$dim$time$vals)) stop("time dimensions are wrong")
           Projections <- Clim$dim$projection$vals
           
     for (j in 1:length(Projections)){
              RastArray<-get.var.ncdf(Clim, varid=varid, start=c(1,1,1,j), count=c(length(Lon),length(Lat),length(Time),1)) 
                  

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
                if(j==1){ #figure out the clipping just once because it's quite time consuming
                a<-ClipToPolygon(as.numeric(dimnames(RastArray)[[1]]),as.numeric(dimnames(RastArray)[[2]]),(RastArray[,,i]),Boundary)
                indicies=data.frame()
                       for(k in 1:length(Lat)){
                          for(m in 1:length(Lon)){
                          for(g in 1:nrow(a@coords)){
                          if(Lon[m]==a@coords[g,1] & Lat[k]==a@coords[g,2]){
                           if(nrow(indicies)==0) indicies=cbind(m,k)
                           else indicies<-rbind(indicies,cbind(m,k))
                          }} }}  
                          
                  } 
                  
                  ClippedDat<-RastArray[indicies[,1],indicies[2],] 
                        
            colnames(ClippedDat)<-Time
            #here we always have to collapse over the pixels
            ClippedDat<-apply(ClippedDat,2,mean)
            if(j==1) OutputFrame<-ClippedDat
            else OutputFrame<-cbind(OutputFrame,ClippedDat)
    }
    colnames(OutputFrame)<-Projections
    if(aggrToYear){
    Year <- floor(as.numeric(rownames(OutputFrame)))
     OutputFrame <- aggregate(OutputFrame,FUN=mean,by=list(Year=Year))
     rownames(OutputFrame)<-unique(Year)
    }
    
    OutputFrame<-OutputFrame[,-c(1)]
    ModelKey<-(read.table(ModelKeyPath,stringsAsFactors=FALSE))[,1]
    colnames(OutputFrame)<-ModelKey
    
   return(OutputFrame) 
}

   
  