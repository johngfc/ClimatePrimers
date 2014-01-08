url_grid <- "http://opendap.deltares.nl/thredds/fileServer/opendap/rijkswaterstaat/vaklodingen_remapped/vaklodingenKB116_4544.nc" 
# note: netcdf4 does not work on windows R   
url_time <- "http://opendap.deltares.nl/thredds/fileServer/opendap/rijkswaterstaat/waterbase/concentration_of_suspended_matter_in_sea_water/id410-DELFZBTHVN.nc"  
download.file(url_grid, "vaklodingenKB116_4544.nc", method = "auto", quiet = FALSE, mode="wb", cacheOK = TRUE)   
download.file(url_time, "id410-DELFZBTHVN.nc", method = "auto", quiet = FALSE, mode="wb", cacheOK = TRUE)  
