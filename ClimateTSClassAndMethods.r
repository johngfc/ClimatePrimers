 setClass("ClimateTS",
  representation=list(Ts="numeric",Time="POSIXct",Year="vector",Month="vector",PlotUnits="character",Var="vector",
          SourcePath="character"))

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
    ProjIndx<-switch(Var,
       ppt=1,
       tmx=2,
       tmn=3) #only for prism
    #====================================
    # here we loop through pull in and if desired clip the data
    #===================================
    for (j in 1:length(Var)){
                 #slightly different ncdf formats
              if(InputNCDF=="Prism.nc") RastArray<- get.var.ncdf(Clim,start=c(1,1,1,ProjIndx),count=c((length(Lon)),length(Lat),length(.Object@Time),1))
              else RastArray<-get.var.ncdf(Clim, varid=varid, start=c(1,1,1,j), count=c(length(Lon),length(Lat),length(.Object@Time),1))
                 dimnames(RastArray)[[1]]<-Lon
                 dimnames(RastArray)[[2]]<-Lat
                 dimnames(RastArray)[[3]]<-.Object@Time

                if(j==1 & Clip){
                    #I can't always clip because not always are there points inside the boundary
                    #figure out the clipping just once because it's quite time consuming
                  browser()
                    a<-ClipToPolygon(Lon,Lat,(RastArray[,,j]),Boundary,Indicies=TRUE)
                    LonLat<-expand.grid(Lon,Lat)
                    o<-order(LonLat)
                    #Clipping reverses the longitude dimension
                    #we can get the indicies by doing a modulus other dimension
                    #on the grid index

                    LonInd<-a@grid.index%%length(Lat)
                    LatInd<-a@grid.index%%length(Lon)
                    indicies<-cbind(LatInd,LonInd)
                    indicies=data.frame()
                       for(k in 1:length(Lat)){ #this is sloppy there should be a better way to do this
                       #I have to get the indicies for the clipping so that I can clip the array
                          for(m in 1:length(Lon)){
                           for(g in 1:nrow(a@coords)){
                             if(Lon[m]==a@coords[g,1] & Lat[k]==a@coords[g,2]){
                             if(nrow(indicies)==0) indicies=cbind(m,k)
                              else indicies<-rbind(indicies,cbind(m,k))
                          }} }}
                  }
                   r<-RastArray[rev(seq(1:length(Lon))),seq(1:length(Lat)),seq(1:length(.Object@Time))]
                  browser()
                  if(!Clip) indicies<-expand.grid(1:length(Lon),1:length(Lat))
                  ClippedDat<-apply(RastArray, 3, function(x) x[indicies]) #indexing in multidimensional arrays is less than intuitive


            #here we always have to collapse over the pixels
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
      UnitIn <- toupper(as.character(UnitMap[names(UnitMap)==Var]))

     if (.Object@PlotUnits != UnitIn)
          .Object@Ts<-Convert(UnitIn,.Object@PlotUnits,.Object@Layer)

     .Object@Ts<-OutputFrame

     if(InputNCDF=="Prism.nc"){
     .Object@Var<-Var
     return(.Object)
     }
     #==============================================
     # this last part should probably be put into a new
     # object that extends the current object but not today...
     #==============================================
     if(Var%in%c("tasmax","tasmin")){
      #not all models were run for tasmin and tasmax so we need to remove those that weren't
      RawMetaData<-scan(ModelKeyPath,what="raw",sep="\n")
      Rm<-grep(Var,RawMetaData)
      if(length(Rm)>0) ModelKey<-ModelKey[-c(Rm)]
     }


    if(ncol(OutputFrame)==length(ModelKey)){ #not the case for tasmin tasmax I need to
      #read in and parse the metadata to figure it out
      colnames(OutputFrame)<-ModelKey
    }
    
     Emissions<-factor(substr(colnames(Yearly),start= nchar(colnames(Yearly))-4,stop= nchar(colnames(Yearly))))
    colnames(ClippedDat)<-Time
    ClippedDat<-apply(ClippedDat,2,mean)
   return(ClippedDat)
})

Prismppt <- ClimateTS(PrismPath,Var="ppt",Boundary=Boundary,PlotUnits="MM",UnitMap=UnitLookup$Prism,ModelKeyPath=ModelKeyPath)