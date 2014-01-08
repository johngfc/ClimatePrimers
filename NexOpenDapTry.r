library(ncdf4)
#get ranges from
#http://dataserver.nccs.nasa.gov/thredds/ncss/grid/bypass/NEX-DCP30/bcsd/rcp85/r1i1p1/inmcm4_pr.ncml/dataset.html
#after running the first 50 lines of GenerateClimatePrimer

> destfile="C:\\GoogleDrive\\Climate\\Nex.nc"
> bbox = "&north=45.1&west=250.1&east=255.1&south=43.1&horizStride=1"
> paste(url,"?var=inmcm4_tasmin",bbox,"&time_start=",GCMTime[1],"&time_end=",GCMTime[2], sep="") -> f
> download.file(f, destfile=destfile,mode="wb")
trying URL 'http://dataserver.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd/rcp85/r1i1p1/inmcm4_tasmin.ncml?var=inmcm4_tasmin&north=45.1&west=250.1&east=255.1&south=43.1&horizStride=1&time_start=2007-01-01T00:00:00Z&time_end=2010-12-31T00:00:00Z'
Content type 'text/html' length unknown



#this bit right here downloads 13 bytes essentially an empty file
url = "http://dataserver.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd/rcp85/r1i1p1/inmcm4_tasmin.ncml"
GCMTime = c('2007-01-01T00:00:00Z','2007-02-01T00:00:00Z')
destfile="C:\\GoogleDrive\\Climate\\Nex.nc"

bbox = "&north=45.1&west=-109.1&east=108&south=43.1&horizStride=1"
paste(url,"?var=inmcm4_tasmin",bbox,"&time_start=",GCMTime[1],"&time_end=",GCMTime[2], sep="") -> f
download.file(f, destfile=destfile,mode="wb")

i=1
while(TRUE){
    bbox = "&north=45.1&west=-110.1&east=-108.1&south=43.1&horizStride=1"
        a<-paste(url,"?var=inmcm4_tasmin",bbox,"&time_start=",GCMTime[1],"&time_end=",GCMTime[2], sep="")
        ty<-try(download.file(a, destfile=destfile,mode="wb"),silent=TRUE)
        if(class(ty)!="try-error") stop("a worked")
    bbox = "&north=45.1&west=-110.1&east=-118.1&south=43.1"
    b<-paste(url,"?var=inmcm4_tasmin",bbox,"&time_start=",GCMTime[1],"&time_end=",GCMTime[2], sep="") -> f
        ty<-try(download.file(b, destfile=destfile,mode="wb"),silent=TRUE)
        if(class(ty)!="try-error") stop("b worked")
    bbox = "&north=45.1&west=250.1&east=249.1&south=43.1"
    d<-paste(url,"?var=inmcm4_tasmin",bbox,"&time_start=",GCMTime[1],"&time_end=",GCMTime[2], sep="") -> f
        ty<-try(download.file(d, destfile=destfile,mode="wb"),silent=TRUE)
        if(class(ty)!="try-error") stop("d worked")    
    Sys.sleep(1)
    i<-i+1
    print(i)
}


bbox = "&north=45.1&west=-110.1&east=-108.1&south=43.1&horizStride=1"
e<-paste(url,"?var=inmcm4_tasmin",bbox,"&time_start=",GCMTime[1],"&time_end=",GCMTime[2], sep="")
bbox = "&north=45.1&west=-110.1&east=-118.1&south=43.1"
f<-paste(url,"?var=inmcm4_tasmin",bbox,"&time_start=",GCMTime[1],"&time_end=",GCMTime[2], sep="") -> f
}
try(download.file(f, destfile=destfile,mode="wb"),silent=TRUE)


f<-"http://www.meteo.unican.es/thredds/ncss/grid/VALUE/Observations/E-OBS_v7/Grid_050deg_reg/tx_0.50deg_reg_v7.0.nc?var=tx&north=44&south=36&east=5&west=-10&time_start=1991-01-01T00:00:00Z&time_end=2000-12-31T00:00:00Z"
open.ncdf(destfile)
http://dataserver.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd/rcp85/r1i1p1/inmcm4_tasmin.ncml?var=inmcm4_tasmin&north=45&south=43&east=-105&west=-110&time_start=2007-01-01T00:00:00Z&time_end=2007-12-31T00:00:00Z"

http://dataserver.nccs.nasa.gov/thredds/ncss/grid/bypass/NEX-DCP30/bcsd/rcp85/r1i1p1/inmcm4_pr.ncml?var=inmcm4_pr&north=45&west=-105&east=-110&south=43&horizStride=1&time_start=2007-01-16T12%3A00%3A00Z&time_end=2010-12-16T12%3A00%3A00Z&timeStride=1&addLatLon=true