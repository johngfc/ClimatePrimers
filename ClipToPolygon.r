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
    
    clipped_grid= temp[sel,]
    if(!Indicies) return(clipped_grid)
   
    #its a bit of a pain to get a matrix mask out of the sp object
    LL<-expand.grid(Lon,Lat)
    charStr<-paste(LL[,1],LL[,2],sep="")
    inBound<-paste(clipped_grid@coords[,1],clipped_grid@coords[,2],sep="")
    b<-match(charStr,inBound)
    LL<-LL[!is.na(b),]
    Mask<-matrix(b,nrow=length(Lon),byrow=FALSE)
    ClippedLayer<-t(Z)
    ClippedLayer[is.na(Mask)]<-NA
   
    return(ClippedLayer)
}
