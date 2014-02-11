#===============================================================
#                      end of input here    

#=============================================
# reading in my helper functions 
#  and all required libraries
#  installing missing libraries
#=============================================
    setwd("/data/nccsc/Private/TalbertM/MariansProjects/Climate/Rcode")
    #setwd("C:\\Users\\mallen\\Desktop\\Climate\\Rcode")
    sourceList<-list("ChkLibs.r","RequestNetCDF.R","GetParkBoundary.r",
    "GetParkBox.r","InputConstants.r")
    unlist(lapply(sourceList,source))
    
    ChkLibs(list("ncdf4","shapefiles","maptools","raster"))
    
   
#============================================
# Inputting all data that is required to produce
# the primer 
#============================================
#Park Information
    ParkCode = "Pipe"
    ParkName = "Pipestone National Park"
    NpsShapes ="/data/nccsc/Private/TalbertM/MariansProjects/Climate/InputLayers/NPS_boundaries/nps_boundary.shp"
       
# Setting up and pointing to output directory structure
         
    OutputParent=file.path("/data/nccsc/Private/TalbertM/MariansProjects/Climate/ParkOutput",ParkCode)
       if(!file.exists(OutputParent)) dir.create(OutputParent)
    OutputNCDF = file.path(OutputParent,"ClimateData")
    OutputGraphics = file.path(OutputParent,"graphics")
       if(!file.exists(OutputGraphics))dir.create(OutputGraphics)
       if(!file.exists(OutputNCDF)) dir.create(OutputNCDF)
       setwd(OutputNCDF)

# GCM and Prism pull info     

    PrismURI="http://cida.usgs.gov/thredds/dodsC/prism"
    Daymet="http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet",
    NexURI="http://dataserver.nccs.nasa.gov/thredds/dodsC/bypass/NEX-DCP30/bcsd" 

    Present=2013
    ES = c("rcp26","rcp45","rcp60","rcp85","historical")
           #for nex pull we do groups of years
    GCMStart <- c(2015,2055,2095)
    GCMEnd <- c(2018,2058,2098)
             #Nex is only 1/14/1950-1/1/2100 best to end on the last day of the year
             #then I don't have to worry about partial years 
    HistoricTime = c(1895,2013)  
              #Prism is only 1/1/1895-11/1/2011  
    #List of models to use this might often be all
   NexLst<-list(rcp45 = ModelList[[1]][c(1,2,3,52,53,54,94,95,96,70,71,72)],
          rcp85=ModelList[[2]][c(1,2,3,46,47,48,88,89,90,70,71,72)])
 
   PrsmLst<-list(prism=ModelList[[3]])
   
#==========================================
# moving the latex and the menu into our directory
#==========================================
file.copy(from="/data/nccsc/Private/TalbertM/MariansProjects/Climate/MenuDefaults/GraphicsCatalog.tex", 
          to=file.path(OutputGraphics,"GraphicsCatalog.tex"))
file.copy(from="/data/nccsc/Private/TalbertM/MariansProjects/Climate/MenuDefaults/GraphicsMenuWorksheet.docx", 
          to=file.path(OutputGraphics,"GraphicsMenuWorksheet.docx"))               
#===========================================    
#get a bounding box for the park for now later we can just grab the shape 
#============================================ 

  BoundBox <- GetParkBox(Bndry=NpsShapes,ParkCode,Buffer="50km")
#read in PRISM and NEX CMIP5 for the desired park by submitting the shape file 
#===========================================
# this section only works on linux

RequestNetCDF(BoundBox,URI=NexURI,ModelList=NexLst,GCMStart,GCMEnd,OutputNCDF,FileName="Nex.nc",ParkCode)
RequestNetCDF(BoundBox,URI=PrismURI,ModelList=PrsmLst,Start=HistoricTime[1],End=HistoricTime[2],
   OutputNcdfDir=OutputNCDF,FileName="Prism.nc",ParkCode=ParkCode)

                               