 setClass("ClimateTS",
  representation=list(Ts="matrix",Time="POSIXct",Year="vector",Month="vector",PlotUnits="character",Var="vector",
          SourcePath="character"))

ClimateTS = function(InputNCDF,Model,Var,Boundary,PlotUnits,UnitMap,WholeYear=TRUE){
	Ts = new("ClimateTS",InputNCDF,Model,Var,Boundary,PlotUnits,UnitMap,WholeYear)
	return(Ts)
}

setMethod(f="initialize",signature="ClimateTS",
	definition=function(.Object,InputNCDF,Model,Var,Boundary,PlotUnits,UnitMap,WholeYear){
	
        Clim<-open.ncdf(InputNCDF)

        Lat<-get.var.ncdf(Clim,UnitMap$latName)
        Lon<-get.var.ncdf(Clim,UnitMap$lonName)
        tm<- get.var.ncdf(Clim,UnitMap$lonName)
        UnitIn = toupper(as.character(UnitMap[names(UnitMap)==Var]))

        if(any(Lon>0)) Lon<-Lon-360
        #===============================
        # constructiong the time components
        #===============================
        
          #86400 is the number of seconds in a day POSIXct expects seconds since
          #the origin this might be funky on leap days. tm is in days since origin
         .Object@Time <- as.POSIXct(tm*86400, origin = UnitMap$TimeOrigin)
         .Object@Year = as.numeric(format(.Object@Time,"%Y"))
         .Object@Month = as.numeric(format(.Object@Time,"%m"))

    browser()

    ProjIndx<-1 #only for prism
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

                    a<-ClipToPolygon(Lon,Lat,(RastArray[,,j]),Boundary,Indicies=FALSE)
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
                  if(!Clip) ClippedDat<-apply(RastArray,1,mean) # collapse this to one less dimension to match the next line
                  else ClippedDat<-apply(RastArray, 3, function(x) x[indicies]) #indexing in multidimensional arrays is less than intuitive


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

          IncompleteYrs <- which(Year%in%(names(table(Year))[table(Year)!=12]),arr.ind=TRUE)
          OutputFrame <- OutputFrame[-c(IncompleteYrs),]
          .Object@Time <- .Object@Time[-c(IncompleteYrs)]
          .Object@Year <- .Object@Year[-c(IncompleteYrs)]
          .Object@Month <- .Object@Month[-c(IncompleteYrs)]
          }
     }
     
     if (.Object@PlotUnits != UnitIn)
          .Object@Ts<-Convert(UnitIn,.Object@PlotUnits,.Object@Layer)
          
     if(InputNCDF=="Prism.nc")
      .Object@Ts<-OutputFrame
    
     Emissions<-factor(substr(colnames(Yearly),start= nchar(colnames(Yearly))-4,stop= nchar(colnames(Yearly))))
    colnames(ClippedDat)<-Time
    ClippedDat<-apply(ClippedDat,2,mean)
   return(ClippedDat)
})

Prismppt <- ClimateTS(PrismPath,Model="prism",Var="ppt",Boundary=Boundary,PlotUnits="C",UnitMap=UnitLookup$Prism)