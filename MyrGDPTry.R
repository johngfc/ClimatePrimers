library("rGDP")

# ---- variables -----
WFS <-  'https://www.sciencebase.gov/catalogMaps/mapping/ows/51b0f5e5e4b030b51983cda1' #somewhere my data is stored upload:
feature_collection  <-	'sb:WBIC_190900' #field name in the shape file
attribute  <-	'YELL' #I think this is the specific park (value in the field above
datasetURI	<-	'http://esgdata1.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd/rcp45/r1i1p1.ncml'  #rasters I'm trying to query NEX CMIP5
var	<-	'1'
# ---- variables -----

# instantiate rGDP object	
rGDP	<-	rGDP()
# changing this to number 2 since I wan't netCDF
rGDP	<-	setAlgorithm(rGDP,getAlgorithms(rGDP)[2])

# show what 'cha got
print(rGDP)

# set the web feature service
rGDP	<-	setWFS(rGDP,WFS)

# set the feature collection for the element in the service that you want to use
rGDP	<-	setFeature(rGDP,list('FEATURE_COLLECTION'=feature_collection,
	'ATTRIBUTE'=attribute))
	
# set the process inputs that you want to use
rGDP	<-	setPostInputs(rGDP,list('DATASET_ID'=var,
								'DATASET_URI'=datasetURI))

# show what 'cha got
print(rGDP)

# execute what you have
rGDP	<-	executePost(rGDP)

status.rGDP  <-  checkProcess(rGDP)

cat('checking status of GDP request. Large complex requests take longer to process.\n')
repeat{
	if (!is.null(status.rGDP$URL) | status.rGDP$status!=""){
	    break
	  }
  cat('checking process...\n')
  Sys.sleep(10)
  if (is.null(status.rGDP$URL)){
    status.rGDP  <-  checkProcess(rGDP)
  }
}

if (status.rGDP$status=='Process successful'){
	cat(paste(status.rGDP$status,'\nDownload available at: ',status.rGDP$URL,sep=''))
} else {
	cat(status.rGDP$status)
}
