PlotClimOverMap<-function(Bound,Layer,Lon,Lat){
    # This function takes a boundary and a two dimensional raster 
    # long and lat should be its x and y dimenssions
     ex<-extent(Bound)
     LonRng<-extendrange(c(ex@xmin,ex@xmax))
     LatRng<-extendrange(c(ex@ymin,ex@ymax))
    
    #================================================================
    #Plot a google map underneath the region
    #map<-openmap(Bounds[2,c(2,1)],bounds
    #================================================================
    map <- openmap(c(LatRng[2],LonRng[1]), c(LatRng[1],LonRng[2]),type="bing")
    map_longlat<-openproj(map,projection="+proj=longlat")
    plot(map_longlat,raster=TRUE)
    map("state",col="yellow",add=TRUE,lwd=2)
    
    #================================================================
    # clipping TempAvg requires knowing it's dimensions then setting up 
    # the spatial points data frame for the desired time and projection
    #      TempAvg[Long,Lat,time,Proj]
    #================================================================
    ClippedClim<- ClipToPolygon(Lon,Lat,t(Layer),Bound)
    image(ClippedClim,add=TRUE)
    plot(Bound,add=TRUE,lwd=5,border="blue")
}