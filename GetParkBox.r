GetParkBox<-function(Bndry,ParkCode){
    #This takes the shape file extracts the park and projects
    #a lot hardwired in here but the NPS boundary file shouldn't change much 
    a<-readShapePoly(Bndry)
    a<-a[-c(1,2),]
    NP<-a[a$S_Loc_Nm==ParkCode,]
    ex<-extent(NP)
    SP <- SpatialPoints(cbind(c(ex@xmin,ex@xmin,ex@xmax,ex@xmax), c(ex@ymin,ex@ymax,ex@ymin,ex@ymax)), 
      proj4string=CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"))
    Bounds<-spTransform(SP, CRS("+proj=longlat"))
}