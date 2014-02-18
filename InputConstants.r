 # a constant list of models associated with emissions scenarios stored as a constant 
 # so I don't mistype the names
 # to generate this list I used
 # temp <- getDataIDs(rGDP)
 # cat(temp,sep="','")

 #Model List is only used for specifying which CMIP5 are desired and constructing the NEX call
ModelList<-list(rcp45 = c('ACCESS1-0_pr','ACCESS1-0_tasmax','ACCESS1-0_tasmin','BNU-ESM_pr','BNU-ESM_tasmax','BNU-ESM_tasmin','CCSM4_pr','CCSM4_tasmax','CCSM4_tasmin','CESM1-BGC_pr','CESM1-BGC_tasmax','CESM1-BGC_tasmin','CESM1-CAM5_pr','CESM1-CAM5_tasmax','CESM1-CAM5_tasmin','CMCC-CM_pr','CMCC-CM_tasmax','CMCC-CM_tasmin','CNRM-CM5_pr','CNRM-CM5_tasmax','CNRM-CM5_tasmin','CSIRO-Mk3-6-0_pr','CSIRO-Mk3-6-0_tasmax','CSIRO-Mk3-6-0_tasmin','CanESM2_pr','CanESM2_tasmax','CanESM2_tasmin','FGOALS-g2_pr','FGOALS-g2_tasmax','FGOALS-g2_tasmin','FIO-ESM_pr','FIO-ESM_tasmax','FIO-ESM_tasmin','GFDL-CM3_pr','GFDL-CM3_tasmax','GFDL-CM3_tasmin','GFDL-ESM2G_pr','GFDL-ESM2G_tasmax','GFDL-ESM2G_tasmin','GFDL-ESM2M_pr','GFDL-ESM2M_tasmax','GFDL-ESM2M_tasmin','GISS-E2-H-CC_pr','GISS-E2-H-CC_tasmax','GISS-E2-H-CC_tasmin','GISS-E2-R-CC_pr','GISS-E2-R-CC_tasmax','GISS-E2-R-CC_tasmin','GISS-E2-R_pr','GISS-E2-R_tasmax','GISS-E2-R_tasmin','HadGEM2-AO_pr','HadGEM2-AO_tasmax','HadGEM2-AO_tasmin','HadGEM2-CC_pr','HadGEM2-CC_tasmax','HadGEM2-CC_tasmin','HadGEM2-ES_pr','HadGEM2-ES_tasmax','HadGEM2-ES_tasmin','IPSL-CM5A-LR_pr','IPSL-CM5A-LR_tasmax','IPSL-CM5A-LR_tasmin','IPSL-CM5A-MR_pr','IPSL-CM5A-MR_tasmax','IPSL-CM5A-MR_tasmin','IPSL-CM5B-LR_pr','IPSL-CM5B-LR_tasmax','IPSL-CM5B-LR_tasmin','MIROC-ESM-CHEM_pr','MIROC-ESM-CHEM_tasmax','MIROC-ESM-CHEM_tasmin','MIROC-ESM_pr','MIROC-ESM_tasmax','MIROC-ESM_tasmin','MIROC5_pr','MIROC5_tasmax','MIROC5_tasmin','MPI-ESM-LR_pr','MPI-ESM-LR_tasmax','MPI-ESM-LR_tasmin','MPI-ESM-MR_pr','MPI-ESM-MR_tasmax','MPI-ESM-MR_tasmin','MRI-CGCM3_pr','MRI-CGCM3_tasmax','MRI-CGCM3_tasmin','NorESM1-M_pr','NorESM1-M_tasmax','NorESM1-M_tasmin','bcc-csm1-1-m_pr','bcc-csm1-1-m_tasmax','bcc-csm1-1-m_tasmin','bcc-csm1-1_pr','bcc-csm1-1_tasmax','bcc-csm1-1_tasmin','inmcm4_pr','inmcm4_tasmax','inmcm4_tasmin'),

rcp85 = c('ACCESS1-0_pr','ACCESS1-0_tasmax','ACCESS1-0_tasmin','BNU-ESM_pr','BNU-ESM_tasmax','BNU-ESM_tasmin','CCSM4_pr','CCSM4_tasmax','CCSM4_tasmin','CESM1-BGC_pr','CESM1-BGC_tasmax','CESM1-BGC_tasmin','CESM1-CAM5_pr','CESM1-CAM5_tasmax','CESM1-CAM5_tasmin','CMCC-CM_pr','CMCC-CM_tasmax','CMCC-CM_tasmin','CNRM-CM5_pr','CNRM-CM5_tasmax','CNRM-CM5_tasmin','CSIRO-Mk3-6-0_pr','CSIRO-Mk3-6-0_tasmax','CSIRO-Mk3-6-0_tasmin','CanESM2_pr','CanESM2_tasmax','CanESM2_tasmin','FGOALS-g2_pr','FGOALS-g2_tasmax','FGOALS-g2_tasmin','FIO-ESM_pr','FIO-ESM_tasmax','FIO-ESM_tasmin','GFDL-CM3_pr','GFDL-CM3_tasmax','GFDL-CM3_tasmin','GFDL-ESM2G_pr','GFDL-ESM2G_tasmax','GFDL-ESM2G_tasmin','GFDL-ESM2M_pr','GFDL-ESM2M_tasmax','GFDL-ESM2M_tasmin','GISS-E2-R_pr','GISS-E2-R_tasmax','GISS-E2-R_tasmin','HadGEM2-AO_pr','HadGEM2-AO_tasmax','HadGEM2-AO_tasmin','HadGEM2-CC_pr','HadGEM2-CC_tasmax','HadGEM2-CC_tasmin','HadGEM2-ES_pr','HadGEM2-ES_tasmax','HadGEM2-ES_tasmin','IPSL-CM5A-LR_pr','IPSL-CM5A-LR_tasmax','IPSL-CM5A-LR_tasmin','IPSL-CM5A-MR_pr','IPSL-CM5A-MR_tasmax','IPSL-CM5A-MR_tasmin','IPSL-CM5B-LR_pr','IPSL-CM5B-LR_tasmax','IPSL-CM5B-LR_tasmin','MIROC-ESM-CHEM_pr','MIROC-ESM-CHEM_tasmax','MIROC-ESM-CHEM_tasmin','MIROC-ESM_pr','MIROC-ESM_tasmax','MIROC-ESM_tasmin','MIROC5_pr','MIROC5_tasmax','MIROC5_tasmin','MPI-ESM-LR_pr','MPI-ESM-LR_tasmax','MPI-ESM-LR_tasmin','MPI-ESM-MR_pr','MPI-ESM-MR_tasmax','MPI-ESM-MR_tasmin','MRI-CGCM3_pr','MRI-CGCM3_tasmax','MRI-CGCM3_tasmin','NorESM1-M_pr','NorESM1-M_tasmax','NorESM1-M_tasmin','bcc-csm1-1-m_pr','bcc-csm1-1-m_tasmax','bcc-csm1-1-m_tasmin','bcc-csm1-1_pr','bcc-csm1-1_tasmax','bcc-csm1-1_tasmin','inmcm4_pr','inmcm4_tasmax','inmcm4_tasmin'),
prism =c('ppt','tmx','tmn'))  

#The units for each netcdf is stored in the metadata I save it here so I can convert units as need
#these have many aliases
#Time origin is the origin for the julian date (days since x)
UnitLookup<-list(Nex=list(TimeOrigin = "1950-01-01T00:00:00Z",
                          Temp = "K",
                          Tas = "K",
                          Tmin = "K",
                          Tmax = "K",
                          Precip = "kgm2s1",
                          latName =  "lat",
                          lonName =  "lon",
                          timeName = "time"),
                 Prism=list(TimeOrigin ='1858-11-17T00:00:00Z',
                            Temp ="C",
                            Tmin ="C",
                            tmx ="C",
                            tmn ="C",
                            Tmax ="C",
                            Precip ="mm",
                            ppt ="mm",
                            latName = "lat",
                            lonName = "lon",
                            timeName = "time"
                            ),
                 GDO=list(TimeOrigin ='1950-01-01T00:00:00Z',
                            Temp ="C",
                            Tmin ="C",
                            Tmax ="C",
                            tas ="C",
                            tasmin ="C",
                            tastmax="C",
                            Precip ="mm",
                            pr ="mm",
                            latName = "latitude",
                            lonName = "longitude",
                            timeName = "time"
                            ),
                 Mauer=list(TimeOrigin ='1950-01-01T00:00:00Z',
                            Temp ="C",
                            Tmin ="C",
                            Tmax ="C",
                            Precip ="mm",
                            latName = "latitude",
                            lonName = "longitude",
                            timeName = "time"
                            )                                         
                 )                    
                          
GenerateLab<-function(Var,PlotUnits,addTime=FALSE,Month,Year){
    
 quant<-switch(Var,
              Temp    = "Temperature",
               Tas     = "Average~Temperature",
              Tmin    = "Minimum~Temperature",
              Tmax    = "Maximum~Temperature",
              tmx     ="Maximum~Temperature",
              Precip  = "Precipitation",
              GrnPnk     = "",
              PurOrn  ="",
              PrecipChng = "Precipitation~Change",
              TempChng   = "Temperature~Change")
           
           #setting a default label and changing it in a few special cases 
            Main = paste(quant, PlotUnits,sep="~")
           if(toupper(PlotUnits)%in%c("C","F","K")){
               Main<-paste(quant, "~({}^o*",PlotUnits,")",sep="")
            } 
            if(toupper(PlotUnits)=="MM"){
               Main<-paste(quant, "(mm / month)",sep="")
            } 
            if(PlotUnits=="kgm2s1"){
            Main = paste(quant,(kg*m^2*s^1),sep="~")
            } 
            if(addTime){ Main = paste(
                     ifelse(length(x@Month==1),paste(month.name[Month],"~",sep=""),""),
                     ifelse(length(x@Year==1),Year,paste(min(Year),"~to~",max(Year),sep="")),
                     "~",Main,
                     sep="")
                 }
                
           Main = parse(text=Main)
           return(Main) 
}
 