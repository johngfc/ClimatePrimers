setClass("ClimateTS",
  representation=list(Ts="matrix",Time="POSIXct",Year="vector",Month="vector",PlotUnits="character",Var="vector",
          SourcePath="character",Proj="vector",Rcp="vector"))

ClimateTS = function(InputNCDF,Var,Boundary,PlotUnits,UnitMap,WholeYear=TRUE,ModelKeyPath=NA,Clip=TRUE){
	Ts = new("ClimateTS",InputNCDF,Var,Boundary,PlotUnits,UnitMap,WholeYear,ModelKeyPath,Clip=TRUE)
	return(Ts)
}

setMethod(f="initialize",signature="ClimateTS",
	definition=function(.Object,InputNCDF,Var,Boundary,PlotUnits,UnitMap,WholeYear,ModelKeyPath,Clip=TRUE){
	
        Clim<-open.ncdf(InputNCDF)

        Lat<-get.var.ncdf(Clim,UnitMap$latName)
        Lon<-get.var.ncdf(Clim,UnitMap$lonName)
        tm <- get.var.ncdf(Clim,UnitMap$timeName)
        UnitIn <- toupper(as.character(UnitMap[names(UnitMap)==Var]))

        if(any(Lon>0)) Lon<-Lon-360
        #===============================
        # constructiong the time components
        #===============================
          
          #86400 is the number of seconds in a day POSIXct expects seconds since
          #the origin this might be funky on leap days. tm is in days since origin
         .Object@Time <- as.POSIXct(tm*86400, origin = UnitMap$TimeOrigin)
         .Object@Year <- as.numeric(format(.Object@Time,"%Y"))
         .Object@Month <- as.numeric(format(.Object@Time,"%m"))
         .Object@PlotUnits <- PlotUnits
         .Object@SourcePath <- InputNCDF
         .Object@Var<-StandardUnits(Var)
    #this section should eventually be moved out to something that extends the original object
    if(.Object@SourcePath=="Prism.nc"){
        ProjIndx<-switch(Var,
           ppt=1,
           tmx=2,
           tmn=3) #only for prism
       } else{
          ProjIndx<-get.var.ncdf(Clim,"projection")
       }
    #====================================
    # here we loop through pull in and if desired clip the data
    #===================================
        
    for (j in 1:length(ProjIndx)){
                 #slightly different ncdf formats
              if(InputNCDF=="Prism.nc") RastArray<- get.var.ncdf(Clim,start=c(1,1,1,ProjIndx),count=c((length(Lon)),length(Lat),length(.Object@Time),1))
              else RastArray<-get.var.ncdf(Clim, varid=Var, start=c(1,1,1,j), count=c(length(Lon),length(Lat),length(.Object@Time),1))
                 dimnames(RastArray)[[1]]<-Lon
                 dimnames(RastArray)[[2]]<-Lat
                 dimnames(RastArray)[[3]]<-.Object@Time
                 # 
                if(j==1 & Clip){
                    #I can't always clip because not always are there points inside the boundary
                    #figure out the clipping just once because it's quite time consuming
                    ind<-ClipToPolygon(Lon,Lat,(RastArray[,,j]),Boundary,Indicies=TRUE)
                  }
                  if(!Clip) ind<-expand.grid(1:length(Lon),1:length(Lat))
                  ClippedDat<-apply(RastArray, 3, function(x) x[ind]) #indexing in multidimensional arrays is less than intuitive

             #now average over all of the selected pixels to get a time vector
             ClippedDat<-apply(ClippedDat,2,mean)
            if(j==1) OutputFrame<-ClippedDat
            else OutputFrame<-cbind(OutputFrame,ClippedDat)
    }
    #===========================================
    # remove years for which we only have a portion of the year
    # because they cause funkiness
       
    if(WholeYear){
      if(any(table(.Object@Year)!=12)){

          IncompleteYrs <- which(.Object@Year%in%(names(table(.Object@Year))[table(.Object@Year)!=12]),arr.ind=TRUE)
          if(InputNCDF=="Prism.nc") OutputFrame <- OutputFrame[-c(IncompleteYrs)]
          else  OutputFrame <- OutputFrame[-c(IncompleteYrs),]
          .Object@Time <- .Object@Time[-c(IncompleteYrs)]
          .Object@Year <- .Object@Year[-c(IncompleteYrs)]
          .Object@Month <- .Object@Month[-c(IncompleteYrs)]
          }
     }
      UnitIn <- as.character(UnitMap[names(UnitMap)==Var])
      
     .Object@Ts<-as.matrix(OutputFrame)
     
     if (.Object@PlotUnits != UnitIn)
          .Object@Ts<-Convert(toupper(UnitIn),toupper(.Object@PlotUnits),.Object@Ts)
 
     if(InputNCDF=="Prism.nc"){
         .Object@Proj<-vector()
         .Object@Rcp<-vector() 
         return(.Object)
     }
     #==============================================
     # this last part should probably be put into a new
     # object that extends the current object but not today...
     #==============================================
     ModelKey<-(read.table(ModelKeyPath,stringsAsFactors=FALSE))[,1]
     
     if(Var%in%c("tasmax","tasmin")){
      #not all models were run for tasmin and tasmax so we need to remove those that weren't
      RawMetaData<-scan(ModelKeyPath,what="raw",sep="\n")
      Rm<-grep(Var,RawMetaData)
      if(length(Rm)>0) ModelKey<-ModelKey[-c(Rm)]
     }

    if(ncol(OutputFrame)==length(ModelKey)){ #not the case for tasmin tasmax I need to
      #read in and parse the metadata to figure it out
      
      .Object@Proj<-substr(ModelKey,start=1,stop=(nchar(ModelKey)-6))
      .Object@Rcp<-factor(substr(ModelKey,start=(nchar(ModelKey)-4),stop=nchar(ModelKey)))
    }
    
   return(.Object)
})

YrAgg<-function(InputDat){
#takes a ClimateTs and aggregates it to yearly data from monthly
    
    Ts<-aggregate(InputDat@Ts[,1],FUN=mean,by=list(Year=InputDat@Year))[,2]
    for(i in 2:ncol(InputDat@Ts)){
       Ts<-cbind(Ts,aggregate(InputDat@Ts[,i],FUN=mean,by=list(Year=InputDat@Year))[,2])
    }
    InputDat@Ts<-Ts
    InputDat@Year<-unique(InputDat@Year)
    InputDat@Time<-unique(InputDat@Time)
    InputDat@Month<-vector()
    return(InputDat)
}