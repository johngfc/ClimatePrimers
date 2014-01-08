GenerateColors<-function(layer,mapType){
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
              AbsTemp    = brewer.pal(40,"YlOrRd"),
              AbsPrecip  = brewer.pal(40,"GnBu"),
              GrnPnk     = rev(brewer.pal(40,"PiYG")),
              PrecipChng = rev(brewer.pal(40,"BrBG")),
              TempChng   = rev(brewer.pal(40,"RdBu"))
            )
     color
}
SetBreaks<-function(layer,mapType){    
    #set middle color if looking at change 
     if(!mapType%in%c("AbsTemp","AbsPrecip")){
      r<-max(abs(range(layer,na.rm=TRUE)))
      Breaks<-seq(from=-r,to=r,length=length(color)+1)
     } else Breaks=NULL
     
     Breaks  
}          