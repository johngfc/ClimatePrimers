GetParkBoundary<-function(Bndry,ParkCode,Buffer=NA){
    #this gets the park boundary creates a buffer if desired and
    #returns the boundary transformed to lat lon
    #   Bndry = the location of the shapefile
    #   Buffer = distance in meteres
    #   ParkCode = 4 letter character string specifying the park of interest
    
  ProjInfo<-readOGR(dirname(Bndry),gsub(".shp","",basename(Bndry)))
    ProjInfo<-proj4string(ProjInfo)
  
  a<-readShapePoly(Bndry)
 NP<-a[which(toupper(a$UNIT_CODE)==toupper(ParkCode),arr.ind=TRUE),]
   proj4string(NP)<-ProjInfo
  if(!is.na(Buffer)){
    #we have to transform to "blah" to lcc so we have same dist x and y
    NP<-spTransform(NP, CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))  
    #B<-spTransform(Bounds, CRS("+proj=utm")) 
    NP<-gBuffer(NP,width=Buffer)
  }
 
Bounds<-spTransform(NP, CRS("+proj=longlat")) 
return(Bounds)
}
