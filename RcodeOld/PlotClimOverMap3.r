PlotClimOverMap<-function(Bound,Layer,Lon,Lat,Colors,Clip=TRUE,Border="blue",Main="",baselayer,mapType){
    # This function takes a boundary and a two dimensional raster 
    # long and lat should be its x and y dimenssions
     ex<-extent(Bound)
     LonRng<-extendrange(c(ex@xmin,ex@xmax))
     LatRng<-extendrange(c(ex@ymin,ex@ymax))
  
    # storing some standard colors for maps and borders which can be overriddent
    if(missing(Colors)) Colors<-GenerateColors(mapType)
    Breaks<-SetBreaks(Layer,mapType,Colors)
    
    #================================================================
    # clipping TempAvg requires knowing it's dimensions then setting up 
    # the spatial points data frame for the desired time and projection
    #      TempAvg[Long,Lat,time,Proj]
    #================================================================
   browser()   
    if(Clip){  Layer<- ClipToPolygon(Lon,Lat,t(Layer),Bound,Indicies=TRUE)
         #when clipping we have to remove extra rows and columns so plots look good
         f<-function(x){sum(is.na(x))}
         NAperRow<-apply(Layer,1,f)/dim(Layer)[2]
         NaperCol<-apply(Layer,2,f)/dim(Layer)[2]
         
                 # I have to reset these if we cropped our map
                 Breaks<-SetBreaks(Layer,mapType,Colors)
      #================================================================
    #Plot a google map underneath the region
    #map<-openmap(Bounds[2,c(2,1)],bounds
    #================================================================
          a<-as.matrix(Layer@data,nrow=length(Lon),byrow=TRUE)
       if(!missing(baselayer)){
       
         baselayer<-raster(baselayer) 
          background<-intersect(baselayer,Bound)
           background<-trim(background)
            background<-as.matrix(background)
              jet.colors=colorRampPalette( c("gray80", "gray10") )      
         image.plot(background,col=jet.colors(50))
        } else image(Layer,col=Colors)
        map("state",col="red",add=TRUE,lwd=2)
        image(Layer,col=Colors,add=TRUE)
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