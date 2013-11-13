#this should read in the Clim data and get it in a standard format for further processing
 path<-"C:\\Users\\mallen\\Desktop\\Climate\\ClimateData\\ncdf\\prismppt.nc"
 
ReadClimateData<-function(OutputDir,Time,Model,Var){
    # This function reads climate data from netcdf into a 3 dimensional array 
    
    # OutputDir = the directory where we wrote the Clim output
    # Time = specified in Generate Climate Primer 
    # Model = is either Prism or an emissions scenario at present
    # var = is the specific variable for a given model for Prism it's ppt tmax timin
    #       for others it's actually the model for the emissions scenario
    
     Clim<-open.ncdf(file.path(OutputDir,"ncdf",
                  paste(Model,Var,".nc",sep=""))) 
     Lat<-Clim$dim$lat$vals
     Lon<-Clim$dim$lon$vals
    
      #Time is recorded as days since 1858/11/17 so to calcuate current time I can use something like
      #(Time)/365.25+1858.8794521  but it's easier just to parse the time range and generate a sequence
      # not deal with leap years and rounding blah, blah, blah...
      Time = seq(from = (as.numeric(substr(Time[1],1,4))+as.numeric(substr(Time[1],6,7))/12),
          to = (as.numeric(substr(Time[2],1,4))+as.numeric(substr(Time[2],6,7))/12),
          by = 1/12)-1/12 #so january ends up being a round number and decmber is in ther current year
      # Check that I have the right length at least
     if(length(Time)!=length(Clim$dim$time$vals)) stop("time dimensions are wrong")
    RastArray<-get.var.ncdf(nc=Clim)
    dimnames(RastArray)[[1]]<-Lon
    dimnames(RastArray)[[2]]<-Lat
    dimnames(RastArray)[[3]]<-Time
   return(RastArray) 
}

   
  