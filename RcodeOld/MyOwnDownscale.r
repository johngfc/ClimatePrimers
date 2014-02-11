 INMCM85<-open.ncdf("C:\\Users\\mtalbert\\Downloads\\r1i1p1_inmcm4_tasmin.nc")
     Clim<-open.ncdf("C:\\Users\\mtalbert\\Downloads\\e0c8d36d-1707-4a0f-9e56-231acc78dbd5OUTPUT.d97a2137-85b6-42ee-aad1-bad7c611525a.nc")
#these two are 1/1/2095-12/16/2099
CESM<-open.ncdf("C:\\Users\\mtalbert\\Downloads\\cesmcam5TasMax.nc")
Had<-open.ncdf("C:\\Users\\mtalbert\\Downloads\\HadGEM2TasMax.nc")
#these are 1/1950-1/2099 we want 1/2095 I think that's 1740
Coarse<-open.ncdf("H:\\Desktop\\Climate\\bcsd5\\Extraction_tasmax.nc")  

CoarsHad<-get.var.ncdf(Coarse,start=c(1,1,1740,70),count=c(25,19,49,1))

 ModelKey<-(read.table("H:\\Desktop\\Climate\\bcsd5\\Projections5.txt",stringsAsFactors=FALSE))[,1]
 ModelKey[70]
CE<-get.var.ncdf(CESM,start=c(1,1,1),count=c(299,230,48))
Had<-get.var.ncdf(Had,start=c(1,1,1),count=c(299,230,48))

par(mfrow=c(3,2))
image(CoarsHad[,,1])
image(Had[,,1])

image(CoarsHad[,,2])
image(Had[,,2])

image(CoarsHad[,,1]-CoarsHad[,,2])
image(Had[,,1]-Had[,,2])

     
Clim<-get.var.ncdf(Clim,start=c(1,1,1),count=c(293,177,49))      
 b<-a[,,1]-a[,,13]
downscale<-a[,,1]-b
par(mfrow=c(2,2))
image(a[,,1])
image(a[,,13])
image(a[,,26]-a[,,13]+downscale)
image(a[,,26])
image(a[,,26]-(a[,,26]-a[,,13]+downscale))
image(a[,,38]-(a[,,38]-a[,,13]+downscale))

remainder<-a[,,26]-(a[,,26]-a[,,13]+downscale)