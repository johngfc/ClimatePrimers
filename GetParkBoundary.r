GetParkBoundary<-function(Bndry,ParkCode){
  
  a<-readShapePoly(Bndry)
a<-a[-c(1,2),]
NP<-a[a$S_Loc_Nm==ParkCode,]
  proj4string(NP)<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
Bounds<-spTransform(NP, CRS("+proj=longlat")) 
return(Bounds)
}
  
  