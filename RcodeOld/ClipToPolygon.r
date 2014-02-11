ClipToPolygon<-function(Lon,Lat,Z,Shape){
    LonLat<-cbind(expand.grid(Lon,Lat))
    LonLat<-LonLat[order(LonLat[,1]),]
    temp=SpatialPixels(SpatialPoints(cbind(LonLat[,1],LonLat[,2])),tolerance=.4)
    temp = SpatialPixelsDataFrame(temp, data.frame(tmax = as.vector(Z)),tolerance=.4)
    
    ## Now lets cookie-cut the surface to only include the City of London. 
    sel=!is.na(overlay(temp, Shape))
    
    clipped_grid= temp[sel,]
    clipped_grid
}
