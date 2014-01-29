 setClass("MappedData",
  representation=list(Layer="array",Lat="vector",Lon="vector",Time="POSIXct",Year="vector",Month="vector",UnitIn="character",PlotUnits="character",Var="character",
          SourcePath="character",ColorScale="vector",TimeOrigin="character"))
          
MappedData = function(SourcePath,Time,UnitMap,PlotUnits,Var,LatRng=NA,LonRng=NA){
	Map = new("MappedData",SourcePath,Time,UnitMap,PlotUnits,Var,LatRng,LonRng)
	return(Map)
}

setMethod(f="initialize",signature="MappedData",
	definition=function(.Object,SourcePath,Time,UnitMap,PlotUnits,Var,LatRng,LonRng){
	
		.Object@SourcePath = SourcePath
	     temp<-open.ncdf(.Object@SourcePath)
		.Object@TimeOrigin = UnitMap$TimeOrigin
		.Object@UnitIn = toupper(as.character(UnitMap[names(UnitMap)==Var]))
		.Object@PlotUnits = toupper(PlotUnits)
		.Object@Lat = get.var.ncdf(temp,varid=UnitMap$latName)
		.Object@Lon = get.var.ncdf(temp,varid=UnitMap$lonName)
		.Object@Var = Var
     if(any(.Object@Lon>0)) .Object@Lon<-.Object@Lon-360
		#=======================================
    #    Setting up temporal aspects
    #=======================================   
       #Getting the time dimension and converting from Julian to POSIXct
     
       ncdfTime = get.var.ncdf(temp,varid=UnitMap$timeName)
	     ncdfTime = as.POSIXct(ncdfTime*86400, origin = UnitMap$TimeOrigin) #as.POSIXct expects seconds not days
	     Time     = as.POSIXct(Time)
	    
       StartIndx = which(format(ncdfTime,"%Y-%m")==format(Time[1],"%Y-%m"),arr.ind=TRUE)
	   
     #Time can have a start and a stop or just one specify time to pull
     if(length(Time)==2){ 
         Count = which(format(ncdfTime,"%Y-%m")==format(Time[2],"%Y-%m"),arr.ind=TRUE)-StartIndx+1
        .Object@Time = seq(from=Time[1],to=Time[2], by = "month")
     }else{ 
         Count = 1
         .Object@Time = Time[1]
     }
     
     .Object@Year = as.numeric(format(.Object@Time,"%Y"))
     .Object@Month = as.numeric(format(.Object@Time,"%m"))
     
      #=====================================
      #     Determine how to restrict lat and lon
      #======================================
      
      if(!is.na(LatRng)){
          LatIndx<-which(.Object@Lat>=min(LatRng) & .Object@Lat<=max(LatRng),arr.ind=TRUE)
           if(length(LatIndx)<=1) stop(paste("Valid LatRng is",range(.Object@Lat)))
          .Object@Lat<-.Object@Lat[LatIndx]
          LatIndx<-range(LatIndx)
      } else LatIndx<-c(1,length(.Object@Lat))
      
     if(!is.na(LonRng)){
          LonIndx<-which(.Object@Lon>=min(LonRng) & .Object@Lon<=max(LonRng),arr.ind=TRUE)
           if(length(LonIndx)<=1) stop(paste("Valid LonRng is",range(.Object@Lon)))
          .Object@Lon<-.Object@Lon[LonIndx]
          LonIndx<-range(LonIndx)
      } else LonIndx<-c(1,length(.Object@Lon)) 
     #=======================================
     #     Retrieve spatial data
     #=======================================
     
      .Object@Layer = get.var.ncdf(temp,start=c(LonIndx[1],LatIndx[1],StartIndx),count=c((LonIndx[2]-LonIndx[1]+1),(LatIndx[2]-LatIndx[1]+1),Count))
      close.ncdf(temp)
      
      #=====================================
      #     Convert units if necessary and possible
      #=====================================
       
        if (.Object@PlotUnits != .Object@UnitIn)
          .Object@Layer<-Convert(.Object@UnitIn,.Object@PlotUnits,.Object@Layer)
        return(.Object)
		})

 
setMethod("+", signature(e1='MappedData', e2='MappedData'),
    function(e1, e2){ 
    if(e1@PlotUnits==e2@PlotUnits & all(e1@Lat==e2@Lat) & all(e1@Lon==e2@Lon)){
     NewObject<-e1
     NewObject@Layer <- e1@Layer + e2@Layer
    } else stop("Dimensions or units do not match")
   return(NewObject) 
})

setMethod("-", signature(e1='MappedData', e2='MappedData'),
    function(e1, e2){ 
    if(e1@PlotUnits==e2@PlotUnits & all(e1@Lat==e2@Lat) & all(e1@Lon==e2@Lon)){
     NewObject<-e1
     NewObject@Layer <- e1@Layer - e2@Layer
     NewObject@Var<-ifelse(tolower(substr(NewObject@Var,1,1))=="t","TempChng","PrecipChng")
    } else stop("Dimensions or units do not match")
   return(NewObject) 
})

setMethod("*", signature(e1='MappedData', e2='MappedData'),
    function(e1, e2){ 
    if(e1@PlotUnits==e2@PlotUnits & all(e1@Lat==e2@Lat) & all(e1@Lon==e2@Lon)){
     NewObject<-e1
     NewObject@Layer <- e1@Layer * e2@Layer
    } else stop("Dimensions or units do not match")
   return(NewObject) 
})

setMethod("/", signature(e1='MappedData', e2='MappedData'),
    function(e1, e2){ 
    if(e1@PlotUnits==e2@PlotUnits & all(e1@Lat==e2@Lat) & all(e1@Lon==e2@Lon)){
     NewObject<-e1
     NewObject@Layer <- e1@Layer / e2@Layer
    } else stop("Dimensions or units do not match")
   return(NewObject) 
})

setMethod("+", signature(e1='MappedData', e2='numeric'),
    function(e1, e2){ 
     NewObject<-e1
     NewObject@Layer <- e1@Layer + e2
   return(NewObject) 
})

setMethod("-", signature(e1='MappedData', e2='numeric'),
    function(e1, e2){ 
     NewObject<-e1
     NewObject@Layer <- e1@Layer - e2
   return(NewObject) 
})

setMethod("*", signature(e1='MappedData', e2='numeric'),
    function(e1, e2){ 
     NewObject<-e1
     NewObject@Layer <- e1@Layer * e2
   return(NewObject) 
})

setMethod("/", signature(e1='MappedData', e2='numeric'),
    function(e1, e2){ 
     NewObject<-e1
     NewObject@Layer <- e1@Layer / e2
   return(NewObject) 
})

            