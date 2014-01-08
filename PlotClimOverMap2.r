PlotClimOverMap<-function(Bound,Layer,Lon,Lat,Colors,Clip=TRUE,Border="blue",Main=""){
    # This function takes a boundary and a two dimensional raster 
    # long and lat should be its x and y dimenssions
     ex<-extent(Bound)
     LonRng<-extendrange(c(ex@xmin,ex@xmax))
     LatRng<-extendrange(c(ex@ymin,ex@ymax))
    
    #================================================================
    # clipping TempAvg requires knowing it's dimensions then setting up 
    # the spatial points data frame for the desired time and projection
    #      TempAvg[Long,Lat,time,Proj]
    #================================================================
      
    if(Clip){ 
      #================================================================
    #Plot a google map underneath the region
    #map<-openmap(Bounds[2,c(2,1)],bounds
    #================================================================
    map <- openmap(c(LatRng[2],LonRng[1]), c(LatRng[1],LonRng[2]),type="bing")
    map_longlat<-openproj(map,projection="+proj=longlat")
    plot(map_longlat,raster=TRUE)
    map("state",col="red",add=TRUE,lwd=2)
      ClippedClim<- ClipToPolygon(Lon,Lat,t(Layer),Bound)
        image(ClippedClim,col=Colors,add=TRUE)
    }else {image.plot(Lon,Lat,Layer,col=Colors,xlab="Longitude",ylab="Latitude")
   # legend("bottomright",legend=c("Park Boundary","State Boundary"),col=c("black","red"),lty=1,lwd=c(4,2),bg="white")
     map("state",col="red",add=TRUE,lwd=2)
    }
    plot(Bound,add=TRUE,lwd=7,border=Border)
    mtext(Main,cex=2)
   
   # colrange<-seq(from=min(Layer),to=max(Layer),length=length(Colors))
   # incLat<-diff(LatRng)*.01
   # incLon<-diff(LonRng)*.1
   #    rect(LonRng[2]-1*incLon,LatRng[1],LonRng[2],LatRng[1]+(length(Colors)+2)*incLat,col="white")
   #     for(i in 1:length(colrange)){
   #     rect(LonRng[2]-.5*incLon,LatRng[1]+incLat*i,LonRng[2],LatRng[1]+(i+1)*incLat,col=Colors[i],border=FALSE) 
   #    }      
    #legend("topleft",legend=c("1","2","3","4"),fill=Colors[c(2,4,6,8,10,12,14)],bg="white")
}