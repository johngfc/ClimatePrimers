 require(rGDP)

# create rGDP object w/ defaults
rGDP <- rGDP()
# give this rGDP object a linear ring as the feature of interest (will be adding multiple rings in the future, but...)
rGDP <- setFeature(rGDP,list(LinearRing=c(-111.48,36.95, -111.48, 36.92, -111.47, 36.93,-111.47, 36.95,-111.48,36.95)))

# get a list of available processing algorithms
getAlgorithms(rGDP)

# set processing algorithm to feature weighted grid statistics (unweighted will likely fail, because the ring won't intersect the centroids)
rGDP <- setAlgorithm(rGDP,getAlgorithms(rGDP)[2]) # OPeNDAP Subset

# set the post inputs for the processing dataset
rGDP <-  setPostInputs(rGDP,list('DATASET_URI'='http://esgdata1.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd/rcp45/r1i1p1.ncml',
                                        'TIME_START'='2010-01-01T00:00:00Z',
                                        'TIME_END'='2010-05-01T00:00:00Z'))

# print it out data IDs
getDataIDs(rGDP)

# set DATASET_ID to one of the list items that was returned
rGDP <-  setPostInputs(rGDP,list('DATASET_ID'=getDataIDs(rGDP)[1]))

#print rGDP object contents:
rGDP

# kick off your request
rGDP <- executePost(rGDP)

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
