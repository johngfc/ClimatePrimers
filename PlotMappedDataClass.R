	
setMethod("plot", signature(x='MappedData',y='ANY'),  
    function(x,Bound,Colors,background,Main,midCol="white",...){ 
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
              PurOrn  ="",
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
    if(midCol!="white") Colors=two.colors(n=256, start=Colors[1], end=Colors[length(Colors)], middle=midCol,alpha=1.0)
    Breaks<-SetBreaks(x@Layer,x@Var,Colors)
    
    #================================================================
    # clipping TempAvg requires knowing it's dimensions then setting up 
    # the spatial points data frame for the desired time and projection
    #      TempAvg[Long,Lat,time,Proj]
    #================================================================
    
    if(!missing(background) & !missing(Bound)){
   
       #  x@Layer<- ClipToPolygon(x@Lon,x@Lat,t(x@Layer),Bound,Indicies=TRUE)
         #when clipping we have to remove extra rows and columns so plots look good
         #I think I want to get rid of this because we can crop in the initialize
         #if<-function(x){sum(is.na(x))}
         #NAperRow<-apply(x@Layer,1,f)/dim(x@Layer)[2]
         #NaperCol<-apply(x@Layer,2,f)/dim(x@Layer)[2]
         
                 # I have to reset these if we cropped our map
               #  Breaks<-SetBreaks(x@Layer,x@Var,Colors)
      #================================================================
    #Plot a google map underneath the region
    #map<-openmap(Bounds[2,c(2,1)],bounds
    #================================================================
  
          baselayer<-raster(baselayer) 
          background<-intersect(baselayer,Bound)
          background<-trim(background)
          background<-as.matrix(background)
          jet.colors=colorRampPalette( c("gray80", "gray10") ) 
          BgCol<-jet.colors(50)
          #add some transparencey     
              color.box<-col2rgb(BgCol,alpha=TRUE)
                           color.box[4,]<-30
                           temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
                           BgCol<-apply(color.box/255,2,temp.fct)
         
          par(oma=c( 0,0,0,4))
          image(x=x@Lon,y=x@Lat,x@Layer,col=Colors,xlab="Longitude",ylab="Latitude",main=Main)
          image(x=seq(from=Bound@bbox[1,1],to=Bound@bbox[1,2],length=nrow(background)),
                     y=seq(from=Bound@bbox[2,1],to=Bound@bbox[2,2],length=ncol(background)),
                     z=background,col=BgCol,add=TRUE)
      
          map("state",col="red",add=TRUE,lwd=2)
          image(x=x@Lon,y=x@Lat,x@Layer,col=Colors,add=TRUE)
          plot(Bound,add=TRUE,lwd=5,border="black")
         
    }else{
          par(oma=c( 0,0,0,4))
          image(x=x@Lon,y=x@Lat,z=x@Layer,col=Colors,xlab="Longitude",ylab="Latitude",main=Main)
          plot(Bound,add=TRUE,lwd=2,border="black")
      # legend("bottomright",legend=c("Park Boundary","State Boundary"),col=c("black","red"),lty=1,lwd=c(4,2),bg="white")
          map("state",col="black",add=TRUE,lwd=2)
    }
        par(oma=c( 0,0,0,1))
          image.plot(x@Layer,legend.only=TRUE,col=Colors)
  
} )