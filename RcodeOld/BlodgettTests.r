# Define Inputs (will come from external call)
start <- "1961"
end <- "1962"
bbox_in<-"-87,41,-89,43"
bioclims<-"1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19"
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/wicci/cmip3/20c3m"
tmax_var  <- "20c3m-cccma_cgcm3_1-tmax-01"
tmin_var <- "20c3m-cccma_cgcm3_1-tmin-01"
prcp_var <- "20c3m-cccma_cgcm3_1-prcp-01"
tave_var <- "NULL"

# Define Inputs (will come from external call)
start <- "1980"
end <- "1980"
bbox_in<-c(-90,40,-91,41)
bioclims<-c(1)
OPeNDAP_URI<-"http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet"
tmax_var  <- "tmax"
tmin_var <- "tmin"
prcp_var <- "prcp"
tave_var <- "NULL"

start <- "1950"
end <- "1962"
bbox_in<-c(-90,41,-90.5,41.5)
bioclims<-c(1,2)
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/dcp/conus"
tmax_var  <- "ccsm-a1b-tmax-NAm-grid"
tmin_var <- "ccsm-a1b-tmin-NAm-grid"
prcp_var <- "ccsm-a1fi-pr-NAm-grid"
tave_var <- "NULL"

start <- "1950"
end <- "1950"
bbox_in<-"-87,41,-89,43"
bioclims<-"1,2,3,4,5,6,7"
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/prism"
tmax_var  <- "tmx"
tmin_var <- "tmn"
prcp_var <- "ppt"
tave_var <- "NULL"
dods_data <- nc_open(OPeNDAP_URI)
variables<-as.character(sapply(dods_data$var,function(x) x$name))


start <- "1950"
end <- "1950"
bbox_in<-"-87,41,-89,43"
bioclims<-"1,2,3,4,5,6,7"
OPeNDAP_URI<-"http://esgdata1.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd/Emissions/r1i1p1.ncml"
tmax_var  <- "tmx"
tmin_var <- "tmn"
prcp_var <- "ppt"
tave_var <- "NULL"