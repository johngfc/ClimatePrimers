GDPNexPrismGrab<-function(Bound,GCMTime,HistoricTime,Model,OutputDir){
    # this function requests data from the Geodata Portal given 
    # a temporal and spatial extent for projections and historic 
    # climate data the desired emissions scenarios and the models 
    # associated with each
    # when the request is sucessful the output will be a directory with a ncdf for 
    # each model emissions combo
    # Bound = a bounding box for the shapefile eventually will be replaced with
    #         a shapefile code once the NPS shape is on the GDP
    # GCM = GCM start and end time
    # HistoricTime = Historic start and end time
    # Model = a list with names list names equal to emissions scenarios and 
    #         each list element a vector of appropriate model names for each 
    #         emissions scenario
    # OutputDir = the directory where output netcdf files are to be written
    
      gcmURI='http://esgdata1.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd/Emissions/r1i1p1.ncml' 
      ModelRetry = Model #failed models are returned in the same list structure
      dir.create(file.path(OutputDir,"ncdf"))
   
      
      #loop through the Emissions scenarios
      for (i in 1:length(Model)){
        #loop through the models for an emissions scenario  
          for(j in 1:length(Model[[i]])){
            #I've got to loop on object creation as well 
           # create rGDP object w/ defaults
            rGDP <- rGDP()
            # give this rGDP object a linear ring as the feature of interest (will be adding multiple rings in the future, but...)
            rGDP <- setFeature(rGDP,list(LinearRing=c(Bound@bbox[1,1], Bound@bbox[2,2],
                  Bound@bbox[1,1], Bound@bbox[2,1],
                  Bound@bbox[1,2], Bound@bbox[2,1],
                  Bound@bbox[1,2],  Bound@bbox[2,2],
                  Bound@bbox[1,1], Bound@bbox[2,2])))
                                         
            # set processing algorithm NetCDF
            rGDP <- setAlgorithm(rGDP,getAlgorithms(rGDP)[2]) # OPeNDAP Subset
          
                if(names(Model)[i]=="prism"){
               rGDP <-  setPostInputs(rGDP,
                        list('DATASET_URI'='http://cida.usgs.gov/thredds/dodsC/prism',
                        'TIME_START'=HistoricTime[1],
                        'TIME_END'=HistoricTime[2]))
               } else{
                # set the post inputs for the processing dataset
                rGDP <-  setPostInputs(rGDP,
                        list('DATASET_URI'=sub("Emissions",names(Model)[i],gcmURI),
                        'TIME_START'=GCMTime[1],
                        'TIME_END'=GCMTime[2]))
            }
         
                # set DATASET_ID to one of the list items that was returned
                rGDP <-  setPostInputs(rGDP,list('DATASET_ID'=Model[[i]][j]))
                  
               status.rGDP <-executeAndCheck(rGDP,OutputDir,Model,i,j)
              cat("\n",status.rGDP$status)
              # k=1
              # while(status.rGDP$status!='Process successful'){ 
              
              #  status.rGDP<-executeAndCheck(rGDP,OutputDir,Model,i,j)
              #  k=k+1
              #  cat("Try number",k,"\n")
              #  }
              #	cat(status.rGDP$status)
                  	
         
        } # end j loop
    } # end i loop   
 } # end GDPNexPrismGrab

 executeAndCheck<-function(rGDP,OutputDir,Model,i,j){
  # kick off your request
  #check that it suceeded and if so download it
                rGDP <- executePost(rGDP)
                
                status.rGDP  <-  checkProcess(rGDP)
                start.time<-Sys.time()
                
                while(is.null(status.rGDP$URL) & status.rGDP$status==""){
                  cat('checking process...\n')
                  Sys.sleep(100)
                  if (is.null(status.rGDP$URL)){
                    status.rGDP  <-  checkProcess(rGDP)
                  }
                }
                  Sys.time()-start.time
              if (status.rGDP$status=='Process successful'){
              	cat(paste(names(Model)[i], Model[[i]][j],'Completed Sucessfully',sep=' '))
              	download.file(url=status.rGDP$URL,
                  destfile=file.path(OutputDir,"ncdf",
                  paste(names(Model)[i],Model[[i]][j],".nc",sep="")),mode="wb")
                }
         return(status.rGDP)
}                