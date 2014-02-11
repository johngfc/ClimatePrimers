GetParkBox<-function(Bndry,ParkCode,Buffer=NA){
    #This takes the shape file extracts the park and projects
    #a lot hardwired in here but the NPS boundary file shouldn't change much 
    # Buffer should have th last two characters be units
    ProjInfo<-readOGR(dirname(Bndry),gsub(".shp","",basename(Bndry)))
    ProjInfo<-proj4string(ProjInfo)
    a<-readShapePoly(Bndry)
    NP<-a[which(toupper(a$UNIT_CODE)==toupper(ParkCode),arr.ind=TRUE),]
    ex<-extent(NP)
    if(!is.na(Buffer)) {
        Dist<-as.numeric(substr(Buffer,start=1,stop=(nchar(Buffer)-2)))
        Units<-substr(Buffer,start=(nchar(Buffer)-1),stop=(nchar(Buffer)))
       
        
         ex@xmin<-gcDestination(lon=ex@xmin,lat=ex@ymin,bearing=270,dist=Dist,dist.units=Units,model="WGS84")[1]
         ex@xmax<-gcDestination(lon=ex@xmax,lat=ex@ymax,bearing=90,dist=Dist,dist.units=Units,model="WGS84")[1]
         ex@ymin<-gcDestination(lon=ex@xmin,lat=ex@ymin,bearing=180,dist=Dist,dist.units=Units,model="WGS84")[2]
         ex@ymax<-gcDestination(lon=ex@xmax,lat=ex@ymax,bearing=0,dist=Dist,dist.units=Units,model="WGS84")[2]
         
       
    }
   
    SP <- SpatialPoints(cbind(c(ex@xmin,ex@xmin,ex@xmax,ex@xmax), c(ex@ymin,ex@ymax,ex@ymin,ex@ymax)), 
      proj4string=CRS(ProjInfo))
    Bounds<-spTransform(SP, CRS("+proj=longlat"))
}
