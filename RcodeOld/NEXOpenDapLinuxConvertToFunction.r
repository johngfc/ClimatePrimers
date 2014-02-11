Start <- "2007"
End <- "2017"
#this works after reading in Blodgett's functions
#and getting the bounding box from
OPenDAP_URI<-list(Prism="http://cida.usgs.gov/thredds/dodsC/prism",
Daymet="http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet"
Nex="http://dataserver.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd"
bbox_in<-as.character(paste(BoundBox@coords[4,1],BoundBox@coords[3,2],BoundBox@coords[1,1],BoundBox@coords[2,2],sep=","))


model<-"inmcm4_tasmin"
OPeNDAP_URI<-"http://dataserver.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd/rcp85/r1i1p1/inmcm4_tasmin.ncml"
dods_data <- nc_open(OPeNDAP_URI)
request_bbox_indices<-request_bbox(dods_data,model,bbox_in)
x1<-request_bbox_indices$x1
y1<-request_bbox_indices$y1
x2<-request_bbox_indices$x2
y2<-request_bbox_indices$y2
x_index<-request_bbox_indices$x_index
y_index<-request_bbox_indices$y_index
prj<-request_bbox_indices$prj
# Check for regular grid.
dif_xs = mean(diff(x_index))
dif_ys = mean(diff(y_index))
if (abs(abs(dif_ys)-abs(dif_xs))>0.00001)
  stop('The data source appears to be an irregular grid, this datatype is not supported.')
# Create x/y points for cells for geotiff files to be written.
coords <- array(dim=c(length(x_index)*length(y_index),2))
coords[,1]<-rep(rev(x_index)+dif_ys/2,length(y_index))
coords[,2]<-rep(rev(y_index)-dif_ys/2,each=length(x_index))
fileNames<-array(dim=(as.numeric(End)-as.numeric(Start))*length(bioclims))
fileStep<-1
for (year in as.numeric(Start):(as.numeric(End)))
{
  request_time_indices<-request_time_bounds(dods_data,year,year+1)
  t_ind1 <- request_time_indices$t_ind1
  t_ind2<-request_time_indices$t_ind2
  time<-request_time_indices$time
  origin<-request_time_indices$origin
  # !!! Make sure this is robust for network failures. !!!
  tmax_data <- ncvar_get(dods_data, model, c(min(x1,x2),min(y1,y2),t_ind1),c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1)))

  cells<-nrow(tmax_data)*ncol(tmax_data)
  tmax_data <- matrix(tmax_data,t_ind2-t_ind1,cells,byrow = TRUE)
  tmin_data <- matrix(tmin_data,t_ind2-t_ind1,cells,byrow = TRUE)
  prcp_data <- matrix(prcp_data,t_ind2-t_ind1,cells,byrow = TRUE)
  tave_data <- matrix(tave_data,t_ind2-t_ind1,cells,byrow = TRUE)

  # Create x/y points for cells for geotiff files to be written.
  coords <- array(dim=c(length(x_index)*length(y_index),2))
  coords[,1]<-rep(rev(x_index)+dif_ys/2,length(y_index))
  coords[,2]<-rep(rev(y_index)-dif_ys/2,each=length(x_index))
  mask<-!is.na(prcp_data[,1])
  coords<-coords[mask,]
  tmax_data<-tmax_data[mask,]
  tmin_data<-tmin_data[mask,]
  prcp_data<-prcp_data[mask,]
  tave_data<-tave_data[mask,]

}