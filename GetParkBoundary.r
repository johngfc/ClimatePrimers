GetParkBoundary<-function(Bndry,ParkCode){
  
  a<-readShapePoly(Bndry)

NP<-a[a$UNIT_CODE==ParkCode,]
  proj4string(NP)<-"+proj=longlat +datum=NAD83 +no_defs "
Bounds<-spTransform(NP, CRS("+proj=longlat")) 
return(Bounds)
}
  
  