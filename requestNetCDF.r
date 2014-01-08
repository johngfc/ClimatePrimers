setwd("/data")
setwd("/data/nccsc/Private/TalbertM/MariansProjects/Climate")
source("BlodgettOpenDapAccessMarianTries.r")
source("GetParkBox.r")
source("ChkLibs.r")
source("InputConstants.r")
ChkLibs(list("raster","ncdf4","maptools"))
 ParkCode = "YELL"
    ParkName = "Yellowstone National Park"
    NpsShapes ="NPS_boundaries/nps_boundary.shp" #the folder containg the NPS shape file
    LongitudeOffset =360 #in the last 
    
 #on linux
#for testing making the model list smallish this works with the nex
Nex="http://dataserver.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd"
 ModelList[[1]]<-ModelList[[1]][c(2,3,65,66,47,48)]
 ModelList[[2]]<-ModelList[[2]][c(2,3,65,66,47,48)]  
 OutputNcdfPath="/data/nccsc/Private/TalbertM/MariansProjects/Climate/YellowstoneClimateData/Nex.nc" #a linux path as this will only run 
 Start <- 2007
End <- 2017
#hopefuly works for Prism
URI="http://cida.usgs.gov/thredds/dodsC/prism"
ModelList<-list(var=c("ppt","tmx","tmn"))  
Start <- 1900
End <- 2010
OutputNcdfPath="/data/nccsc/Private/TalbertM/MariansProjects/Climate/YellowstoneClimateData/Prism.nc"
requestNetCDF(BoundBox,URI,ModelList,Start,End,OutputNcdfPath,varname,NEX=FALSE)
# URIs I'll eventually need as input for now just the NEX is used
#OPenDAP_URI<-list(,
#Daymet="http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet",
    
#===============================================================
#                      end of input here    

BoundBox<- GetParkBox(Bndry=NpsShapes,ParkCode) 
requestNetCDF(BoundBox,URI,ModelList,Start,End,OutputNcdfPath,varname,NEX=FALSE)
OutpuNcdfPath

model<-"inmcm4_tasmin"
OPeNDAP_URI<-"http://dataserver.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd/rcp85/r1i1p1/inmcm4_tasmin.ncml"
dods_data <- nc_open(OPeNDAP_URI)

requestNetCDF<-function(BoundBox,URI,ModelList,Start,End,OutputNcdfPath,varname,NEX=FALSE){
  # This function requests and builds a netCDF given a set of 
  # temporal and spatial constraints                                    
  # BoundBox   =  bounding box of the park of interest created using GetParkBox.r
  # URI        =  the base URI to access the data
  # ModelList  = a list of variables to be accessed and included in the netcdf
  # Start      = numeric start year 
  # End        = numeric end year (whole years only for now)
  # varname    = the variable name to record in the netcdf tas for example
      bbox_in<-c(BoundBox@coords[4,1],BoundBox@coords[3,2],BoundBox@coords[1,1],BoundBox@coords[2,2])  
      count<-1              
      for(rcp in 1:length(ModelList)){ 
        for(model in 1:length(ModelList[[rcp]])){
           
           dods_data<-URI
           if(NEX) dods_data=paste(URI,names(ModelList)[rcp],"r1i1p1",paste(ModelList[[rcp]][model],"ncml",sep="."),sep="/")
          
           dods_data<-nc_open(dods_data)
           browser()
            if(rcp==1 & model==1)
            {
                    bboxIndex<-request_bbox(dods_data,ModelList[[rcp]][model],bbox_in)
                      attach(bboxIndex); on.exit(detach(bboxIndex))
                    dif_xs = mean(diff(x_index))
                    dif_ys = mean(diff(y_index))
                    tIndex<-request_time_bounds(dods_data,Start,End)
                        attach(tIndex); on.exit(detach(tIndex)) 
                        
                    x <- ncdim_def( "lon", "degreesE", rev(x_index)+dif_ys/2)
                    y <- ncdim_def( "lat", "degreesN", rev(y_index)-dif_ys/2)
                    proje<- ncdim_def( "projection", "days since 1900-01-01", seq(from=1,to=sum(unlist(lapply(ModelList,length)))))
                    tm <- ncdim_def( "time", "days since 1900-01-01", time)
                      
                    X<-ncvar_def(varname,"K",list(x,y,tm,proje),dods_data$var$lon$missval) #name, unit, vals, missing value
                    ncout<-nc_create(OutputNcdfPath,vars=list(X)) 
            }
            for (Time in seq(from=t_ind1,to=(t_ind2-1),by=12)) #still do this yearly to avoid bottlenecks
            { # !!! Make sure this is robust for network failures. !!!
                  ncData <- ncvar_get(dods_data,ModelList[[rcp]][model], c(min(x1,x2),min(y1,y2),Time),
                               c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1)))
                  print(paste("     ",Time))             
              }
            nc_close(dods_data)    
            ncvar_put(ncout,varid=varname,vals=ncData,start=c(1,1,1,count),count=c(length(x_index),length(y_index),length(time),1))
            count<-count+1
            print(count)
          }
       } 
    nc_close(ncout)
}