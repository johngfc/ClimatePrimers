Start <- 2007
End <- 2017
setwd("/data/nccsc/Private/TalbertM/MariansProjects/Climate")
setwd("/data")

source("ChkLibs.r")
ChkLibs(list("ncdf4"))

source("InputConstants.r")
source("BlodgettOpenDapAccessMarianTries.r")
source("GetParkBox.r")

 ParkCode = "YELL"
    ParkName = "Yellowstone National Park"
    NpsShapes ="NPS_boundaries/nps_boundary.shp" #the folder containg the NPS shape file
    LongitudeOffset =360 #in the last 
    OutputNcdfPath="/data/nccsc/Private/TalbertM/MariansProjects/Climate/YellowstoneClimateData/Nex.nc" #a linux path as this will only run 
 #on linux
#for testing making the model list smallish 
 ModelList[[1]]<-ModelList[[1]][c(2,3,65,66,47,48)]
 ModelList[[2]]<-ModelList[[2]][c(2,3,65,66,47,48)]    
# URIs I'll eventually need as input for now just the NEX is used
#OPenDAP_URI<-list(Prism="http://cida.usgs.gov/thredds/dodsC/prism",
#Daymet="http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet",
Nex="http://dataserver.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd"    
#===============================================================
#                      end of input here    

BoundBox<- GetParkBox(Bndry=NpsShapes,ParkCode,OutputNcdfPath) 


model<-"inmcm4_tasmin"
OPeNDAP_URI<-"http://dataserver.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd/rcp85/r1i1p1/inmcm4_tasmin.ncml"
dods_data <- nc_open(OPeNDAP_URI)

requestNetCDF<-function(BoundBox,URI,ModelList,Start,End){
  # This function requests and builds a netCDF given a set of 
  # temporal and spatial constraints                                    
  # BoundBox   =  bounding box of the park of interest created using GetParkBox.r
  # URI        =  the base URI to access the data
  # ModelList  = a list of variables to be accessed and included in the netcdf
   # Start      = Start year formatted as 2000-01-01T00:00:00Z Historic or GCM time[1] only the year will be used
  # End        = End year formatted as 2000-01-01T00:00:00Z Historic or GCM time[2] only the year will be used
  # 
  
  Start <- as.numeric(substr(Start,start=1,stop=5))
  End <- as.numeric(substr(End,start=1,stop=5))
      bbox_in<-c(BoundBox@coords[4,1],BoundBox@coords[3,2],BoundBox@coords[1,1],BoundBox@coords[2,2])
      bboxIndex<-request_bbox(dods_data,model,bbox_in)
        attach(bboxIndex); on.exit(detach(bboxIndex))
      dif_xs = mean(diff(x_index))
      dif_ys = mean(diff(y_index))
      tIndex<-request_time_bounds(dods_data,Start,End)
          attach(tIndex); on.exit(detach(tIndex)) 
          
      x <- ncdim_def( "lon", "degreesE", rev(x_index)+dif_ys/2)
      y <- ncdim_def( "lat", "degreesN", rev(y_index)-dif_ys/2)
      proje<- ncdim_def( "projection", "days since 1900-01-01", seq(from=1,to=sum(unlist(lapply(ModelList,length)))))
      tm <- ncdim_def( "time", "days since 1900-01-01", time)
      
      
      X<-ncvar_def("tas","K",list(x,y,tm,proje),dods_data$var$lon$missval) #name, unit, vals, missing value
      ncout<-nc_create(OutputNcdfPath,vars=list(X)) 
      
      count<-1              
      for(rcp in 1:length(ModelList)){ 
        for(model in 1:length(ModelList[[rcp]])){ 
           dods_data=paste(Nex,names(ModelList)[rcp],"r1i1p1",paste(ModelList[[rcp]][model],"ncml",sep="."),sep="/")
           dods_data<-nc_open(dods_data)
            for (Time in seq(from=t_ind1,to=(t_ind2-1),by=12)) #still do this yearly to avoid bottlenecks
            { # !!! Make sure this is robust for network failures. !!!
                  tmax_data <- ncvar_get(dods_data,ModelList[[rcp]][model], c(min(x1,x2),min(y1,y2),Time),
                               c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1)))
                  print(paste("     ",Time))             
              }
            nc_close(dods_data)    
            ncvar_put(ncout,varid="tas",vals=tmax_data,start=c(1,1,1,count),count=c(length(x_index),length(y_index),length(time),1))
            count<-count+1
            print(count)
          }
       } 
    nc_close(ncout)
}