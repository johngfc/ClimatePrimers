GenerateColors<-function(mapType){
    #  These functions have some standard color ramps as suggested by Kaye
    #  it sets the break points so that when appropriate a zero change in the
    #  value maps to the neutral color.  Additionally park and state borders that
    #  look good with the color ramp are recorded
    #
    # Layer     = the layer to be mapped
    # mapType   %in% AbsTemp (yellow to red scale)
    #                AbsPrecip (white to blue)
    #                TempChng (blue to red)
    #                PrecipChng (brown to blue\green)
    #                PrplBrn    (Purple to brown)
    #                GrnPnk     (Green to pink scale)
    
    #choose the color ramp
      color<-switch(mapType,
              Temp    = brewer.pal(brewer.pal.info["YlOrRd",]$maxcolors,"YlOrRd"),
              Tmin    = brewer.pal(brewer.pal.info["YlOrRd",]$maxcolors,"YlOrRd"),
              Tmax    = brewer.pal(brewer.pal.info["YlOrRd",]$maxcolors,"YlOrRd"),
              Precip  = brewer.pal(brewer.pal.info["GnBu",]$maxcolors,"GnBu"),
              GrnPnk  = rev(brewer.pal(brewer.pal.info["PiYG",]$maxcolors,"PiYG")),
              PurOrn  = brewer.pal(brewer.pal.info["PuOr",]$maxcolors,"PuOr"),
              PrecipChng = brewer.pal(brewer.pal.info["BrBG",]$maxcolors,"BrBG"),
              TempChng   = rev(brewer.pal(brewer.pal.info["RdBu",]$maxcolors,"RdBu"))
            )
     color
}
SetBreaks<-function(layer,mapType,color){    
    #set middle color if looking at change 
     if(!mapType%in%c("AbsTemp","AbsPrecip")){
      r<-max(abs(range(layer,na.rm=TRUE)))
      Breaks<-seq(from=-r,to=r,length=length(color)+1)
     } else Breaks=NULL
     
     Breaks  
}          