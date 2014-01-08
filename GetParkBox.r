GetParkBox<-function(Bndry,ParkCode){
    #This takes the shape file extracts the park and projects
    #a lot hardwired in here but the NPS boundary file shouldn't change much 
    a<-readShapePoly(Bndry)
    NP<-a[a$UNIT_CODE==ParkCode,]
    ex<-extent(NP)
    SP <- SpatialPoints(cbind(c(ex@xmin,ex@xmin,ex@xmax,ex@xmax), c(ex@ymin,ex@ymax,ex@ymin,ex@ymax)), 
      proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs"))
    Bounds<-spTransform(SP, CRS("+proj=longlat"))
}