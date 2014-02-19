
ReadGDOClimate<-function(OutputFile,Time,Boundary,WholeYear=TRUE,varid,LongitudeOffset,aggrToYear=TRUE,ModelKeyPath,Clip){
    # This function reads climate data from netcdf into a 3 dimensional array 
    # it then clips to the desired shape file
    # optionally it can remove partial years and collapse the pixels within the shape to return one value for each month 
    
    # OutputDir = the directory where we wrote the Clim output
    # Time = specified in Generate Climate Primer 
    # Model = is either Prism or an emissions scenario at present
    # var = is the specific variable for a given model for Prism it's ppt tmax timin
    #       for others it's actually the model for the emissions scenario
             
          Clim<-open.ncdf(file.path(OutputFile))
          Lat<-get.var.ncdf(Clim,"latitude")
          Lon<-get.var.ncdf(Clim,"longitude")-LongitudeOffset  
          Time = seq(from = (as.numeric(substr(Time[1],1,4))+as.numeric(substr(Time[1],6,7))/12),
             to = (as.numeric(substr(Time[2],1,4))+as.numeric(substr(Time[2],6,7))/12),
             by = 1/12)-1/12 #so january ends up being a round number and decmber is in ther current year
      # Check that I have the right length at least
     
     if(length(Time)!=length(Clim$dim$time$vals)) stop("time dimensions are wrong")
           Projections <- get.var.ncdf(Clim,"projection")
           
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
                if(j==1 & Clip){ 
                #I can't always clip because not always are there points in the region
                #figure out the clipping just once because it's quite time consuming
               
                a<-ClipToPolygon(as.numeric(dimnames(RastArray)[[1]]),as.numeric(dimnames(RastArray)[[2]]),(RastArray[,,j]),Boundary,Indicies=FALSE)
                indicies=data.frame()
                       for(k in 1:length(Lat)){
                          for(m in 1:length(Lon)){
                          for(g in 1:nrow(a@coords)){
                          if(Lon[m]==a@coords[g,1] & Lat[k]==a@coords[g,2]){
                           if(nrow(indicies)==0) indicies=cbind(m,k)
                           else indicies<-rbind(indicies,cbind(m,k))
                          }} }}  
                          
                  } 
                  if(!Clip) ClippedDat<-apply(RastArray,1,mean) # collapse this to one less dimension to match the next line
                  else ClippedDat<-apply(RastArray, 3, function(x) x[indicies]) 
                                   
            colnames(ClippedDat)<-Time
            #here we always have to collapse over the pixels
            ClippedDat<-apply(ClippedDat,2,mean)
            if(j==1) OutputFrame<-ClippedDat
            else OutputFrame<-cbind(OutputFrame,ClippedDat)
    }

    if(aggrToYear){
    Year <- floor(as.numeric(rownames(OutputFrame)))
     OutputFrame <- aggregate(OutputFrame,FUN=mean,by=list(Year=Year))
     rownames(OutputFrame)<-unique(Year)
    }
     ModelKey<-(read.table(ModelKeyPath,stringsAsFactors=FALSE))[,1]
    
     if(varid%in%c("tasmax","tasmin")){
      #not all models were run for tasmin and tasmax so we need to remove those that weren't
      RawMetaData<-scan(ModelKeyPath,what="raw",sep="\n")
      Rm<-grep(varid,RawMetaData)
      if(length(Rm)>0) ModelKey<-ModelKey[-c(Rm)]
     }
    
   
    if(ncol(OutputFrame)==length(ModelKey)){ #not the case for tasmin tasmax I need to 
      #read in and parse the metadata to figure it out
      colnames(OutputFrame)<-ModelKey
    }
   return(OutputFrame) 
}

   
  