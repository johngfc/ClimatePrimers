Convert<-function(FromUnits,ToUnits,x){
#Just some standard unit conversions 
    result<-switch(paste(FromUnits,ToUnits,sep="to"),
      FtoC = (x - 32)/1.8,
      FtoK = (x + 459.67)/1.8,
      CtoF =  x*1.8 + 32,
      CtoK = x  + 273.15,
      KtoC =  x - 273.15,
      KtoF =   x*1.8 - 459.67,
      INtoMM =  x*25.4,
      MMtoIN  = x*.03937007874
      )
    
    if(is.null(result)){
      warning("Conversion was not defined")
      return(x)
    }
   return(result) 
 }   