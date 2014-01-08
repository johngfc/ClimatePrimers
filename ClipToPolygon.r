ClipToPolygon<-function(Lon,Lat,Z,Shape,Indicies=FALSE){
# clips a 2 dimensional raster to the shape
# the output is a spatial pixels dataframe
# @data contains the data of the clipped area
# @coords contains the coordinates if I want to merge

    LonLat<-cbind(expand.grid(Lon,Lat))
    LonLat<-LonLat[order(LonLat[,1]),]
    temp=SpatialPixels(SpatialPoints(cbind(LonLat[,1],LonLat[,2])),tolerance=.4)
    temp = SpatialPixelsDataFrame(temp, data.frame(tmax = as.vector(Z)),tolerance=.4)
    
    ## Now lets cookie-cut the surface 
    sel=!is.na(overlay(temp, Shape))
     browser()
    clipped_grid= temp[sel,]
    clipped_grid
}
