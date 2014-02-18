ClipToPolygon<-function(Lon,Lat,Z,Shape,Indicies=FALSE){
# clips a 2 dimensional raster to the shape
# the output is a spatial pixels dataframe
# @data contains the data of the clipped area
# @coords contains the coordinates if I want to merge
    
    LonLat<-cbind(expand.grid(Lon,Lat))
    temp=SpatialPixels(SpatialPoints(cbind(LonLat[,1],LonLat[,2])),tolerance=.4)
    temp = SpatialPixelsDataFrame(temp, data.frame(tmax = as.vector(Z)),tolerance=.4)
     # 
    ## Now lets cookie-cut the surface 
    sel=!is.na(overlay(temp, Shape))
      #
    clipped_grid= temp[sel,]
    
    #sometimes we just need the values returned
    if(!Indicies) return(clipped_grid)
    #often we need indicies since this takes a while to calculate and we'll be
    #subsetting other things
    #its a bit of a pain to get the indicies from vector back to matrix
    LonInd<-ceiling(clipped_grid@grid.index/length(Lon))
    LatInd<-clipped_grid@grid.index%%length(Lon)
    indicies<-cbind(LatInd,LonInd)
 
    return(indicies)
}
