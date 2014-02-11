PlotClimOverMap<-function(Bound,Layer,Lon,Lat,Colors,Clip=TRUE,Border="blue",Main=""){
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
  
    
    if(Clip){
      ClippedClim<- ClipToPolygon(Lon,Lat,t(Layer),Bound)
      image(ClippedClim,add=TRUE,col=Colors)
    }else image(Lon,Lat,Layer,col=Colors)
    plot(Bound,add=TRUE,lwd=5,border=Border)
    mtext(Main,cex=3)
    colrange<-seq(from=min(Layer),to=max(Layer),length=length(Colors))
    incLat<-diff(LatRng)*.01
    incLon<-diff(LonRng)*.1
       rect(LonRng[2]-1*incLon,LatRng[1],LonRng[2],LatRng[1]+(length(Colors)+2)*incLat,col="white")
        for(i in 1:length(colrange)){
        rect(LonRng[2]-.5*incLon,LatRng[1]+incLat*i,LonRng[2],LatRng[1]+(i+1)*incLat,col=Colors[i],border=FALSE) 
        }      
    #legend("topleft",legend=c("1","2","3","4"),fill=Colors[c(2,4,6,8,10,12,14)],bg="white")
}