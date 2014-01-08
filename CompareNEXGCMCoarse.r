    
     #the coarse resolution is ncdf4 so I have to use the ncdf4 libarary which is installed on R 3.0.1 x64
     #I used the following site to get 1 degree binlinear interpolation as suggested by mauer
     #ftp://gdo-dcp.ucllnl.org/pub/dcp/archive/cmip5/global_mon/regrid/inmcm4/rcp85/mon/r1i1p1/tasmin/
     #in addition to the Prism they also added a 9 year averge of the bilinear interpolation
     #so I will have to start at year 10
     
library(ncdf4)
library(ncdf) 

 INMCM85<-nc_open("C:\\GoogleDrive\\Climate\\ClimateData\\Yellowstone\\r1i1p1_inmcm4_tasmin.nc") 
 Coarse<-nc_open("C:\\GoogleDrive\\Climate\\ClimateData\\GCMcoarseCompare\\regridded_1deg_tasmin_Amon_inmcm4_rcp85_r1i1p1_200601-210012.nc")
    # unfortunately ncdf4 doesn't have a utility fct to figure out the dimensionality of the array
    # and so we dig...
Cdim<-Coarse$var$tasmin$size
Fdim<-INMCM85$var$inmcm4_tasmin$size

    # get time step 109 and ts121 along with latitude and longitude

ts109<- ncvar_get(INMCM85, varid="inmcm4_tasmin",start=c(1,1,109), count=c(Fdim[1],Fdim[2],1))
ts121<- ncvar_get(INMCM85, varid="inmcm4_tasmin",start=c(1,1,121), count=c(Fdim[1],Fdim[2],1))
image(ts109-ts121)
    #==============================================
    # Difference is the result to compare with the bilinear interpolation
Difference<-ts109-ts121
lon<-ncvar_get(INMCM85, varid="lon",start=1, count=Fdim[1])
lat<-ncvar_get(INMCM85, varid="lat",start=1, count=Fdim[2])
Time<-ncvar_get(INMCM85, varid="time",start=1, count=20)

    # for any given month NEX takes the average of the 9 previous corresponding
    # months and average them I'm not sure if it happens before or after bilinear 
    # interpolation...
     
NineYr<-array(data = NA, dim = c(10, Cdim[1],Cdim[2]))
S<-seq(from=1,by=12,length=10)
for(i in 1:10){
      NineYr[i,,]<- ncvar_get(Coarse, varid="tasmin",start=c(1,1,S[i]), count=c(Cdim[1],Cdim[2],1))
}

Avg9yr109<-apply(NineYr[1:9,,],c(2,3),mean)
Avg9yr121<-apply(NineYr[2:10,,],c(2,3),mean)

Cts109<- ncvar_get(Coarse, varid="tasmin",start=c(1,1,109), count=c(Cdim[1],Cdim[2],1))
Cts121<- ncvar_get(Coarse, varid="tasmin",start=c(1,1,121), count=c(Cdim[1],Cdim[2],1))
CTime<-ncvar_get(Coarse, varid="time",start=1, count=20)
image(Cts109-Cts121)
 
    #Coarse has the same time but not the same lat lon 
Clon<-ncvar_get(Coarse, varid="longitude",start=1, count=Cdim[1])
Clat<-ncvar_get(Coarse, varid="latitude",start=1, count=Cdim[2])
 

    # now try to restrict to the region of analysis for the fine scaled data
    # these two don't match
    # inRegion<-(Cts1-Cts13)[(Clon<=(max(lon)-180) & Clon>=(min(lon)-180)),(Clat<=max(lat) & Clat>=min(lat))]
    # inRegion<-(Cts1-Cts13)[(Clon<=(max(lon)) & Clon>=(min(lon))),(Clat<=max(lat) & Clat>=min(lat))]
Cts109<-Cts109[(Clon<=(max(lon)) & Clon>=(min(lon))),(Clat<=max(lat) & Clat>=min(lat))]
Cts121<-Cts121[(Clon<=(max(lon)) & Clon>=(min(lon))),(Clat<=max(lat) & Clat>=min(lat))]
Avg9yr109<-Avg9yr109[(Clon<=(max(lon)) & Clon>=(min(lon))),(Clat<=max(lat) & Clat>=min(lat))]
Avg9yr121<-Avg9yr121[(Clon<=(max(lon)) & Clon>=(min(lon))),(Clat<=max(lat) & Clat>=min(lat))]     
     # Convert to rasters so I can do a bilinear interpolation 
Cts109<-raster(t(Cts109))
Cts121<-raster(t(Cts121))
Avg9yr109<-raster(t(Avg9yr109))
Avg9yr121<-raster(t(Avg9yr121))
Cts109<-resample(Cts109,z,method="bilinear")
Cts121<-resample(Cts121,z,method="bilinear")
Avg9yr109<-resample(Avg9yr109,z,method="bilinear")
Avg9yr121<-resample(Avg9yr121,z,method="bilinear")
plot(Cts109-Cts121)

CoarseDif<-t(as.matrix(Cts109))-t(as.matrix(Cts121))
image(CoarseDif,main="Coarse Dif")
 
  p<-raster(t(inRegion))
  z<-raster(t(ts1))
  Interpol<-resample(p,z,method="bilinear")
 image(Difference,main="Fine Diff")
 a<-(Difference+t(as.matrix(Avg9yr109))-t(as.matrix(Avg9yr121)))
image(a,main="with addition")  
  
image(t(as.matrix(Avg9yr)),main="9 yr avg")

  
