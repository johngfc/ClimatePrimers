RequestNetCDF<-function(BoundBox,URI,ModelList,Start,End,OutputNcdfDir,FileName="Nex.nc",ParkCode){
  # This function requests and builds a netCDF given a set of 
  # temporal and spatial constraints                                    
  # BoundBox   =  bounding box of the park of interest created using GetParkBox.r
  # URI        =  the base URI to access the data
  # ModelList  = a list of variables to be accessed and included in the netcdf
  # Start      = Start year formatted as 2000-01-01T00:00:00Z Historic or GCM time[1] only the year will be used
  # End        = End year formatted as 2000-01-01T00:00:00Z Historic or GCM time[2] only the year will be used
     
      #===========================================
      #   we're just picking off the indicies we need which are common so we can just look at the first
      #   without loss of generality
      #===========================================
      OutputNcdfPath<-file.path(OutputNcdfDir,FileName)
      
      if(FileName=="Nex.nc") OPeNDAP_URI<-paste(URI,"/",names(ModelList)[1],"/r1i1p1/",ModelList[[1]][1],".","ncml",sep="")
      if(FileName=="Prism.nc") OPeNDAP_URI<-URI
      
      dods_data <- nc_open(OPeNDAP_URI) 
      model<-ModelList[[1]][1]
     
      Start <- as.numeric(substr(Start,start=1,stop=5))
      End <- as.numeric(substr(End,start=1,stop=5))
      bbox_in<-c(BoundBox@coords[4,1],BoundBox@coords[3,2],BoundBox@coords[1,1],BoundBox@coords[2,2])
    
      bboxIndex<-request_bbox(dods_data,model,bbox_in)
        attach(bboxIndex); on.exit(detach(bboxIndex))
      dif_xs = mean(diff(x_index))
      dif_ys = mean(diff(y_index))
      
      #===========================================
      #looping through all time indicies for different periods
      #to get the indicies 
      #===========================================
      
      tIndex<-list()
      for(t in 1:length(Start)){
        tIndex[[t]]<-request_time_bounds(dods_data,Start[t],End[t])
      }  
     
      tm<- do.call("c",lapply(tIndex,function(lst){lst$time}))
      t_ind1<-do.call("c",lapply(tIndex,function(lst){lst$t_ind1})) 
       t_ind2<-do.call("c",lapply(tIndex,function(lst){lst$t_ind1}))
       
      #===========================================
      #   now define the 4 netcdf dimensions:
      #   lon,lat,projection,time 
      #===========================================    
      x <- ncdim_def( "lon", "degreesE", rev(x_index)+dif_xs/2)
      y <- ncdim_def( "lat", "degreesN", rev(y_index)-dif_ys/2)
      
      proje<- ncdim_def( "projection", "projInfo", seq(from=1,to=sum(unlist(lapply(ModelList,length)))))
      tm <- ncdim_def( "time", "days since 1900-01-01", tm)
      
      
      X<-ncvar_def("tas","K",list(x,y,tm,proje),dods_data$var$lon$missval) #name, unit, vals, missing value
       
      ncout<-nc_create(OutputNcdfPath,vars=list(X)) 
    
      count<-1
      ProjKey<-vector()
      #===========================================
      #   loop through rcp,model and time and pull 
      #   down the data requested
      #===========================================    
            
      for(rcp in 1:length(ModelList)){ 
        for(model in 1:length(ModelList[[rcp]])){
      
           if(FileName=="Nex.nc") dods_data=paste(URI,names(ModelList)[rcp],"r1i1p1",paste(ModelList[[rcp]][model],"ncml",sep="."),sep="/")
           if(FileName=="Prism.nc") dods_data=URI
           dods_data<-nc_open(dods_data)
           tmax_data<-list()
           timeCount<-1     
           for(t in 1:length(tIndex)){ 
             # !!! Make sure this is robust for network failures. !!!      
                  tmax<- ncvar_get(dods_data,ModelList[[rcp]][model], c(min(x1,x2),min(y1,y2),tIndex[[t]]$t_ind1),
                               c((abs(x1-x2)+1),(abs(y1-y2)+1),(tIndex[[t]]$t_ind2-tIndex[[t]]$t_ind1)))
                
                 ncvar_put(ncout,varid="tas",vals=tmax,
                   start=c(1,1,timeCount,count),
                   count=c(length(x_index),length(y_index),
                   (tIndex[[t]]$t_ind2-tIndex[[t]]$t_ind1),1))                 
                 timeCount<-timeCount+(tIndex[[t]]$t_ind2-tIndex[[t]]$t_ind1)
              }
             
           nc_close(dods_data)    
           count<-count+1
           print(paste(count,"of",length(tIndex)*length(ModelList[[1]])*length(ModelList)))
           ProjKey<-c(ProjKey,paste(names(ModelList)[rcp],ModelList[[rcp]][model],sep="_"))
          }
       } 
    #===========================================
    #   recording the projection and emissions 
    #   scenario information   
    #===========================================
    ncatt_put(ncout,varid="projection",attname="ProjKey",attval=paste(ProjKey,collapse=","))   
    nc_close(ncout)
}