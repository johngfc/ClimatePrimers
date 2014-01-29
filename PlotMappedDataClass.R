	
setMethod("plot", signature(x='MappedData',y='ANY'),  
    function(x,Bound,Colors,background,Main,...){ 
    # This function takes a boundary and a two dimensional raster 
    # long and lat should be its x and y dimenssions
   
    
     if(missing(Main)){ # we have units and the value being plotted
     #so we can stitch together a main title
     quant<-switch(x@Var,
              Temp    = "Temperature",
               Tas     = "Average~Temperature",
              Tmin    = "Minimum~Temperature",
              Tmax    = "Maximum~Temperature",
              Precip  = "Precipitation",
              GrnPnk     = "",
              PrecipChng = "Precipitation~Change",
              TempChng   = "Temperature~Change")
             
           if(toupper(x@PlotUnits)%in%c("C","F","K")){
               Main<-paste(quant, "~({}^o*",x@PlotUnits,")",sep="")
            }
            else Main = paste(quant, x@PlotUnits,sep="~")
            Main = paste(
                   ifelse(length(x@Month==1),paste(month.name[x@Month],"~",sep=""),""),
                   ifelse(length(x@Year==1),x@Year,paste(min(x@Year),"~to~",max(x@Year),sep="")),
                   "~",Main,
                 sep="")
            Main = parse(text=Main)
     }
    # storing some standard colors for maps and borders which can be overriddent
    if(missing(Colors)) Colors<-GenerateColors(x@Var)
    Breaks<-SetBreaks(x@Layer,x@Var,Colors)
    
    #================================================================
    # clipping TempAvg requires knowing it's dimensions then setting up 
    # the spatial points data frame for the desired time and projection
    #      TempAvg[Long,Lat,time,Proj]
    #================================================================
    
    if(!missing(background) & !missing(Bound)){
    browser()  
         Layer<- ClipToPolygon(x@Lon,x@Lat,t(x@Layer),Bound,Indicies=TRUE)
         #when clipping we have to remove extra rows and columns so plots look good
         f<-function(x){sum(is.na(x))}
         NAperRow<-apply(x@Layer,1,f)/dim(x@Layer)[2]
         NaperCol<-apply(x@Layer,2,f)/dim(x@Layer)[2]
         
                 # I have to reset these if we cropped our map
                 Breaks<-SetBreaks(x@Layer,x@Var,Colors)
      #================================================================
    #Plot a google map underneath the region
    #map<-openmap(Bounds[2,c(2,1)],bounds
    #================================================================
          a<-as.matrix(Layer@data,nrow=length(Lon),byrow=TRUE)
         
       
          baselayer<-raster(baselayer) 
          background<-intersect(baselayer,Bound)
          background<-trim(background)
          background<-as.matrix(background)
          jet.colors=colorRampPalette( c("gray80", "gray10") )      
          
          image.plot(background,col=jet.colors(50))
      
          map("state",col="red",add=TRUE,lwd=2)
          image(Layer,col=Colors,add=TRUE)
    }else{
          image.plot(x=x@Lon,y=x@Lat,z=x@Layer,col=Colors,xlab="Longitude",ylab="Latitude",main=Main)
      # legend("bottomright",legend=c("Park Boundary","State Boundary"),col=c("black","red"),lty=1,lwd=c(4,2),bg="white")
          map("state",col="black",add=TRUE,lwd=2)
    }
    plot(Bound,add=TRUE,lwd=2,border="yellow")
   
   
   # colrange<-seq(from=min(Layer),to=max(Layer),length=length(Colors))
   # incLat<-diff(LatRng)*.01
   # incLon<-diff(LonRng)*.1
   #    rect(LonRng[2]-1*incLon,LatRng[1],LonRng[2],LatRng[1]+(length(Colors)+2)*incLat,col="white")
   #     for(i in 1:length(colrange)){
   #     rect(LonRng[2]-.5*incLon,LatRng[1]+incLat*i,LonRng[2],LatRng[1]+(i+1)*incLat,col=Colors[i],border=FALSE) 
   #    }      
    #legend("topleft",legend=c("1","2","3","4"),fill=Colors[c(2,4,6,8,10,12,14)],bg="white")
} )