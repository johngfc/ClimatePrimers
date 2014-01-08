# wps.des: id = gridded_bioclim, title = A generalized bioclim algorithm, abstract = TBD; 
# wps.in: start, string, Start Year, Start Year (ie. 1950);
# wps.in: end, string, End Year, End Year (ie. 2000);
# wps.in: bbox_in, string, BBOX, Format, comma seperated min lat/lon max lat/lon;
# wps.in: bioclims, string, bioclims, list of bioclims of interest.;
# wps.in: OPeNDAP_URI, string, OPeNDAP URI, An OPeNDAP (dods) url for the climate dataset of interest.;
# wps.in: tmax_var, string, Tmax Variable, The variable from the OPeNDAP dataset to use as tmax.;
# wps.in: tmin_var, string, Tmin Variable, The variable from the OPeNDAP dataset to use as tmin.;
# wps.in: tave_var, string, Tave Variable, The variable from the OPeNDAP dataset to use as tave, can be "NULL".;
# wps.in: prcp_var, string, Prcp Variable, The variable from the OPeNDAP dataset to use as prcp.;
library("ncdf4")
library("climates")
library("rgdal")
library("stats")
library("chron")
library("zoo")
request_bbox<-function(ncdf4_handle,rep_var,bbox)
{
  if (!is.null(ncatt_get(ncdf4_handle, rep_var,'grid_mapping')) && !is.null(ncdf4_handle$dim$x$vals))
  {
    if (ncatt_get(ncdf4_handle, ncatt_get(ncdf4_handle, rep_var,'grid_mapping')$value, 'grid_mapping_name')$value=='lambert_conformal_conic')
    {
      grid_mapping_name<-ncatt_get(ncdf4_handle, rep_var,'grid_mapping')$value
      longitude_of_central_meridian<-ncatt_get(ncdf4_handle, grid_mapping_name, 'longitude_of_central_meridian')$value
      latitude_of_projection_origin<-ncatt_get(ncdf4_handle, grid_mapping_name, 'latitude_of_projection_origin')$value
      standard_parallel<-ncatt_get(ncdf4_handle, grid_mapping_name, 'standard_parallel')$value
      false_easting<-ncatt_get(ncdf4_handle, grid_mapping_name, 'false_easting')$value
      false_northing<-ncatt_get(ncdf4_handle, grid_mapping_name, 'false_northing')$value
      longitude_of_prime_meridian<-ncatt_get(ncdf4_handle, grid_mapping_name, 'longitude_of_prime_meridian')$value
      semi_major_axis<-ncatt_get(ncdf4_handle, grid_mapping_name, 'semi_major_axis')$value
      inverse_flattening<-ncatt_get(ncdf4_handle, grid_mapping_name, 'inverse_flattening')$value
      if (length(standard_parallel==2))
      {
        prj <- paste("+proj=lcc +lat_1=", standard_parallel[1],
                     " +lat_2=", standard_parallel[2],
                     " +lat_0=", latitude_of_projection_origin,
                     " +lon_0=", longitude_of_central_meridian,
                     " +x_0=", false_easting,
                     " +y_0=", false_northing,
                     " +a=", semi_major_axis,
                     " +f=", (1/inverse_flattening),
                     sep='')
      }
      else
      {
        prj <- paste("+proj=lcc +lat_1=", standard_parallel[1],
                     " +lat_2=", standard_parallel[1],
                     " +lat_0=", latitude_of_projection_origin,
                     " +lon_0=", longitude_of_central_meridian,
                     " +x_0=", false_easting,
                     " +y_0=", false_northing,
                     " +a=", semi_major_axis,
                     " +f=", (1/inverse_flattening),
                     sep='') 
      }
      # Project bbox and unproject data-source range to check intersection.
      # preparing bbox for projection.
      bbox_unproj<-data.frame(matrix(c(bbox, bbox[1],bbox[4],bbox[3],bbox[2]),ncol=2,byrow=TRUE))
      colnames(bbox_unproj)<-c("x","y")
      coordinates(bbox_unproj)<-c("x","y")
      proj4string(bbox_unproj) <- CRS("+init=epsg:4326")
      bbox_proj<-spTransform(bbox_unproj,CRS(prj)) # Project bbox.
      bbox_proj_coords<-coordinates(bbox_proj)
      # Get projected bounds
      min_dods_x<-min(ncdf4_handle$dim$x$vals)
      max_dods_x<-max(ncdf4_handle$dim$x$vals)
      min_dods_y<-min(ncdf4_handle$dim$y$vals)
      max_dods_y<-max(ncdf4_handle$dim$y$vals)
      # Prepare projected bounds to be unprojected.
      ncdf4_handle_range<-data.frame(matrix(c(min_dods_x,min_dods_y,max_dods_x,max_dods_y,min_dods_x,max_dods_y,max_dods_x,min_dods_y),ncol=2,byrow=TRUE))
      colnames(ncdf4_handle_range)<-c("x","y")
      coordinates(ncdf4_handle_range)<-c("x","y")
      proj4string(ncdf4_handle_range) <- CRS(prj)
      ncdf4_handle_range_unproj<-spTransform(ncdf4_handle_range,CRS("+init=epsg:4326"))
      ncdf4_handle_range_unproj_coords<-coordinates(ncdf4_handle_range_unproj)
      # Coding against daymet for now, need to find a way to identify the coordinate variable for requested variables and use that name rather than the hardcoded x and y.
      # Check lower left.
      if (bbox_proj_coords[1]<min_dods_x || bbox_proj_coords[1]>max_dods_x) stop(paste("Submitted minimum longitude",bbox[1], "is outside the dataset's minimum",ncdf4_handle_range_unproj_coords[1]))
      if (bbox_proj_coords[3]<min_dods_y || bbox_proj_coords[3]>max_dods_y) stop(paste("Submitted minimum latitude",bbox[2], "is outside the dataset's minimum",ncdf4_handle_range_unproj_coords[2]))
      # Check upper right.
      if (bbox_proj_coords[2]<min_dods_x || bbox_proj_coords[2]>max_dods_x) stop(paste("Submitted maximum longitude",bbox[3], "is outside the dataset's maximum",ncdf4_handle_range_unproj_coords[3]))
      if (bbox_proj_coords[4]<min_dods_y || bbox_proj_coords[4]>max_dods_y) stop(paste("Submitted maximum latitude",bbox[4], "is outside the dataset's maximum",ncdf4_handle_range_unproj_coords[4]))
      # Check upper left.
      if (bbox_proj_coords[5]<min_dods_x || bbox_proj_coords[5]>max_dods_x) stop(paste("Submitted minimum longitude",bbox[1], "is outside the dataset's minimum",ncdf4_handle_range_unproj_coords[1]))
      if (bbox_proj_coords[6]<min_dods_y || bbox_proj_coords[6]>max_dods_y) stop(paste("Submitted minimum latitude",bbox[2], "is outside the dataset's minimum",ncdf4_handle_range_unproj_coords[2]))
      # Check lower right.
      if (bbox_proj_coords[7]<min_dods_x || bbox_proj_coords[7]>max_dods_x) stop(paste("Submitted maximum longitude",bbox[3], "is outside the dataset's maximum",ncdf4_handle_range_unproj_coords[3]))
      if (bbox_proj_coords[8]<min_dods_y || bbox_proj_coords[8]>max_dods_y) stop(paste("Submitted maximum latitude",bbox[4], "is outside the dataset's maximum",ncdf4_handle_range_unproj_coords[4]))
      bbox<-bbox_proj_coords
      x1 <- which(abs(ncdf4_handle$dim$x$vals-bbox[1])==min(abs(ncdf4_handle$dim$x$vals-bbox[1])))
      y1 <- which(abs(ncdf4_handle$dim$y$vals-bbox[2])==min(abs(ncdf4_handle$dim$y$vals-bbox[2])))
      x2 <- which(abs(ncdf4_handle$dim$x$vals-bbox[3])==min(abs(ncdf4_handle$dim$x$vals-bbox[3])))                 
      y2 <- which(abs(ncdf4_handle$dim$y$vals-bbox[4])==min(abs(ncdf4_handle$dim$y$vals-bbox[4])))
      # Check to see if multiple indices were found and buffer out if they were.
      if(length(x1)==2) if((bbox[1]-ncdf4_handle$dim$x$vals[x1[1]])>(bbox[1]-ncdf4_handle$dim$x$vals[x1[2]])) x1<-x1[1] else x1<-x1[2]  
      if(length(y1)==2) if((bbox[2]-ncdf4_handle$dim$y$vals[y1[1]])>(bbox[2]-ncdf4_handle$dim$y$vals[y1[2]])) y1<-y1[1] else y1<-y1[2]
      if(length(x2)==2) if((bbox[3]-ncdf4_handle$dim$x$vals[x2[1]])>(bbox[3]-ncdf4_handle$dim$x$vals[x2[2]])) x2<-x2[1] else x2<-x2[2]
      if(length(y2)==2) if((bbox[4]-ncdf4_handle$dim$y$vals[y2[1]])>(bbox[4]-ncdf4_handle$dim$y$vals[y2[2]])) y2<-y2[1] else y2<-y2[2]
      x_index<-dods_data$dim$x$vals[x1:x2]
      y_index<-dods_data$dim$y$vals[y1:y2]
    }
    else
    {
      stop('Unsupported Projection Found in Source Data.')
    }
  }
  if (!is.null(ncdf4_handle$dim$lat$vals) && length(dim(ncdf4_handle$dim$lat$vals)==1))
  {
    prj<-"+init=epsg:4326"
    if (max(ncdf4_handle$dim$lon$vals)>180 || max(ncdf4_handle$dim$lat$vals)>180) 
    {
      bbox[1]=bbox[1]+360
      bbox[3]=bbox[3]+360
    }
    if (bbox[1]<min(ncdf4_handle$dim$lon$vals)) stop(paste("Submitted minimum longitude",bbox[1], "is outside the dataset's minimum",min(ncdf4_handle$dim$lon$vals)))
    if (bbox[2]<min(ncdf4_handle$dim$lat$vals)) stop(paste("Submitted minimum latitude",bbox[2], "is outside the dataset's minimum",min(ncdf4_handle$dim$lat$vals)))
    if (bbox[3]>max(ncdf4_handle$dim$lon$vals)) stop(paste("Submitted maximum longitude",bbox[3], "is outside the dataset's maximum",max(ncdf4_handle$dim$lon$vals)))
    if (bbox[4]>max(ncdf4_handle$dim$lat$vals)) stop(paste("Submitted maximum latitude",bbox[4], "is outside the dataset's maximum",max(ncdf4_handle$dim$lat$vals)))
    # Search for x/y indices cooresponding to start and end dates.
    lon1_index <- which(abs(ncdf4_handle$dim$lon$vals-bbox[1])==min(abs(ncdf4_handle$dim$lon$vals-bbox[1])))
    lat1_index <- which(abs(ncdf4_handle$dim$lat$vals-bbox[2])==min(abs(ncdf4_handle$dim$lat$vals-bbox[2])))
    lon2_index <- which(abs(ncdf4_handle$dim$lon$vals-bbox[3])==min(abs(ncdf4_handle$dim$lon$vals-bbox[3])))                 
    lat2_index <- which(abs(ncdf4_handle$dim$lat$vals-bbox[4])==min(abs(ncdf4_handle$dim$lat$vals-bbox[4])))
    # Check to see if multiple indices were found and buffer out if they were.
    if(length(lon1_index)==2) if((bbox[1]-ncdf4_handle$dim$lon$vals[lon1_index[1]])>(bbox[1]-ncdf4_handle$dim$lon$vals[lon1_index[2]])) lon1_index<-lon1_index[1] else lon1_index<-lon1_index[2]  
    if(length(lat1_index)==2) if((bbox[2]-ncdf4_handle$dim$lat$vals[lat1_index[1]])>(bbox[2]-ncdf4_handle$dim$lat$vals[lat1_index[2]])) lat1_index<-lat1_index[1] else lat1_index<-lat1_index[2]
    if(length(lon2_index)==2) if((bbox[3]-ncdf4_handle$dim$lon$vals[lon2_index[1]])>(bbox[3]-ncdf4_handle$dim$lon$vals[lon2_index[2]])) lon2_index<-lon2_index[1] else lon2_index<-lon2_index[2]
    if(length(lat2_index)==2) if((bbox[4]-ncdf4_handle$dim$lat$vals[lat2_index[1]])>(bbox[4]-ncdf4_handle$dim$lat$vals[lat2_index[2]])) lat2_index<-lat2_index[1] else lat2_index<-lat2_index[2]
    x_index<-dods_data$dim$lon$vals[lon1_index:lon2_index]
    y_index<-dods_data$dim$lat$vals[lat1_index:lat2_index]
    x1<-lon1_index
    y1<-lat1_index
    x2<-lon2_index
    y2<-lat2_index
  }
  return(list(x1=x1,y1=y1,x2=x2,y2=y2,x_index=x_index,y_index=y_index,prj=prj))
}

dailyToMonthly<-function(daily_data, time, origin, cells)
{
  daily_data<-zoo(daily_data,chron(time,out.format=c(dates="year-m-day"), origin=origin))
  daily_data<-aggregate(daily_data, as.yearmon, mean)
  daily_data<-t(data.matrix(fortify.zoo(daily_data),cells)[1:12,2:(cells+1)])
  return(daily_data)
}

request_time_bounds<-function(ncdf4_handle, start, end)
{
  time_units<-strsplit(ncdf4_handle$dim$time$units, " ")[[1]]
  time_step<-time_units[1]
  date_origin<-time_units[3]
  time_origin<-"00:00:00"
  if(length(time_units)==4) time_origin<-time_units[4]
  cal_origin <- paste(date_origin, time_origin)
  # Convert to posixlt
  year_origin<-as.numeric(strsplit(date_origin,'-')[[1]][1])
  month_origin<-as.numeric(strsplit(date_origin,'-')[[1]][2])
  day_origin<-as.numeric(strsplit(date_origin,'-')[[1]][3])
  chron_origin<-c(month=month_origin, day=day_origin, year=year_origin)
  t_1 <- julian(strptime(paste(start,'-01-01 12:00',sep=''), '%Y-%m-%d %H:%M'), origin<-strptime(cal_origin, '%Y-%m-%d %H:%M:%S'))
  t_2 <- julian(strptime(paste(end, '-01-01 00:00', sep=''), '%Y-%m-%d %H:%M'), origin<-strptime(cal_origin, '%Y-%m-%d %H:%M:%S'))
  # Some simple time and bbox validation.
  if (t_1<head(ncdf4_handle$dim$time$vals,1)) stop(paste("Submitted start date,",start, "is before the dataset's start date,",chron(floor(head(ncdf4_handle$dim$time$vals,1)),out.format=c(dates="year-m-day"), origin=chron_origin)))
  if (t_2>tail(ncdf4_handle$dim$time$vals,1)) stop(paste("Submitted end date,",end, "is after the dataset's end date,",chron(floor(tail(ncdf4_handle$dim$time$vals,1)),out.format=c(dates="year-m-day"), origin=chron_origin)))
  if (t_1>t_2) stop('Start date must be before end date.')
  t_ind1 <- min(which(abs(ncdf4_handle$dim$time$vals-t_1)==min(abs(ncdf4_handle$dim$time$vals-t_1))))
  t_ind2 <- max(which(abs(ncdf4_handle$dim$time$vals-t_2)==min(abs(ncdf4_handle$dim$time$vals-t_2))))
  time<-dods_data$dim$time$vals[t_ind1:(t_ind2-1)]
  return(list(t_ind1=t_ind1, t_ind2=t_ind2, time=time, origin=chron_origin))
}
bbox_in <- as.double(read.csv(header=F,colClasses=c("character"),text=bbox_in))
bioclims <- as.double(read.csv(header=F,colClasses=c("character"),text=bioclims))
# Define Inputs (will come from external call)
tryCatch(dods_data <- nc_open(OPeNDAP_URI), error = function(e) 
  {
  cat("An error was encountered trying to open the OPeNDAP resource."); print(e)
  })
variables<-as.character(sapply(dods_data$var,function(x) x$name))
if (!tmax_var %in% variables) stop(paste("The given tmax variable wasn't found in the OPeNDAP dataset"))
if (!tmin_var %in% variables) stop(paste("The given tmin variable wasn't found in the OPeNDAP dataset"))
if (!prcp_var %in% variables) stop(paste("The given prcp variable wasn't found in the OPeNDAP dataset"))
if (tave_var!="NULL") if (!tmax_var %in% variables) stop(paste("The given tave variable wasn't found in the OPeNDAP dataset"))
valid_bioclims<-c(1:19)
if (any(!bioclims %in% valid_bioclims)) stop("Invalid Bioclim ids were submitted.")
# Bioclims in allowable set
# lat in dataset check in bbox function
# time in dataset check in time function
# check is tmax, tmin, and prcp variables exist in dataset
request_bbox_indices<-request_bbox(dods_data,tmax_var,bbox_in)
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
fileNames<-array(dim=(as.numeric(end)-as.numeric(start))*length(bioclims))
fileStep<-1
for (year in as.numeric(start):(as.numeric(end)))
{
  request_time_indices<-request_time_bounds(dods_data,year,year+1)
  t_ind1 <- request_time_indices$t_ind1
  t_ind2<-request_time_indices$t_ind2
  time<-request_time_indices$time
  origin<-request_time_indices$origin
  # !!! Make sure this is robust for network failures. !!!
  tmax_data <- ncvar_get(dods_data, tmax_var, c(min(x1,x2),min(y1,y2),t_ind1),c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1)))
  tmin_data <- ncvar_get(dods_data, tmin_var, c(min(x1,x2),min(y1,y2),t_ind1),c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1)))
  prcp_data <- ncvar_get(dods_data, prcp_var, c(min(x1,x2),min(y1,y2),t_ind1),c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1)))
  if (tave_var!="NULL") tave_data <- ncvar_get(dods_data, tave_var, c(min(x1,x2),min(y1,y2),t_ind1),c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1))) else tave_data <- (tmax_data+tmin_data)/2
  cells<-nrow(tmax_data)*ncol(tmax_data)
  tmax_data <- matrix(tmax_data,t_ind2-t_ind1,cells,byrow = TRUE)
  tmin_data <- matrix(tmin_data,t_ind2-t_ind1,cells,byrow = TRUE)
  prcp_data <- matrix(prcp_data,t_ind2-t_ind1,cells,byrow = TRUE)
  tave_data <- matrix(tave_data,t_ind2-t_ind1,cells,byrow = TRUE)
  if (dim(time)>12)
  {
    # Convert daily data to monthly in preperation for bioclim functions.
    time<-floor(time)
    tmax_data<-dailyToMonthly(tmax_data, time, origin, cells)
    tmin_data<-dailyToMonthly(tmin_data, time, origin, cells)
    prcp_data<-dailyToMonthly(prcp_data, time, origin, cells)
    tave_data<-dailyToMonthly(tave_data, time, origin, cells)
  }
  else
  {
    tmax_data<-t(tmax_data)
    tmin_data<-t(tmin_data)
    prcp_data<-t(prcp_data)
    tave_data<-t(tave_data)
  }
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
  bioclim<-data.frame(bioclim(tmin=tmin_data, tmax=tmax_data, prec=prcp_data, tmean=tave_data, bioclims))
  colnames(bioclim)<-paste('bioclim_',bioclims, sep='')
  for (bclim in names(bioclim))
  {
    data_to_write <- SpatialPixelsDataFrame(SpatialPoints(coords, proj4string = CRS(prj)), bioclim[bclim], tolerance=0.0001)
    file_name<-paste(bclim,'_',as.character(year),'.tif',sep='')
    fileNames[fileStep]<-file_name
    fileStep<-fileStep+1
    writeGDAL(data_to_write,file_name)
  }
}
name<-'bioclim.zip'
bioclim_zip<-zip(name,fileNames)
#wps.out: name, zip, bioclim_zip, A zip pf the resulting bioclim getiffs..;