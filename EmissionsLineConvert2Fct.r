
#===============================
# Extracting info
GCMTime = c('1950-01-01T00:00:00Z','2099-01-01T00:00:00Z') #Year-Month-Day and then time 
ToPlot<-ReadGDOClimate("H:\\Desktop\\Climate\\bcsd5\\Extraction_tas.nc",Time=GCMTime,Boundary,WholeYear=TRUE,LongitudeOffset=LongitudeOffset,varid="tas",ModelKeyPath=ModelKeyPath)
EmissionSDPlot(ToPlot,ParkName,Present)
EmissionLinePlot(ToPlot,ParkName,Present)


EmissionLinePlot<-function(Yearly,ParkName,Present){
#Yearly is a dataframe 
#   rows = years for observations
#   columns = to model output for an emissions scenario and climate model
#   rownames = year
#   colnames = model.emissions emissioncs should be 5 characters long "rcp85" for example
#ParkName = The name of the park for the title
#Present = The current year for determining color break 
#Written by Marian Talbert 10/2013
 
    Years<-as.numeric(rownames(Yearly))
    Emissions<-factor(substr(colnames(Yearly),start= nchar(colnames(Yearly))-4,stop= nchar(colnames(Yearly))))
    EmissionsCol<-c("seagreen3","lemonchiffon","orange","red","royalblue1")
    color.box<-col2rgb(EmissionsCol,alpha=TRUE)
                           color.box[4,]<-150
                           temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
                           EmissionsCol<-apply(color.box/255,2,temp.fct)
    PastCol<-EmissionsCol[5]
    EmissionsCol<-EmissionsCol[1:4]                        
    ylabel<-as.expression(expression( paste("Celcius Temperature ( ", degree*C, ")") ))
    
     par(mar=c(5,5,4,2))                        
    MyPlot(Years,range(Yearly),drawGrid=TRUE,cexMult=1,bgCol="grey38",
        main=paste("Average Surface Air Temperature for ",ParkName,"\nUnder Four Emissions Scenarios",sep=""),xlab="Year",ylab=ylabel)
    
    for (i in 1:ncol(Yearly)){
      lines(Years[Years>=Present],Yearly[Years>=Present,i],col=EmissionsCol[Emissions[i]],lwd=1.9)
      lines(Years[Years<=Present],Yearly[Years<=Present,i],col=PastCol,lwd=.75)
      }
    #plotting the avg of everything before the present  
    lines(Years[Years<=Present],apply(Yearly[Years<=Present,],1,mean),lwd=4,col="royalblue4") 
      #Now plotting avgs by emissions scenario
    EmissionsCol<-c("green4","yellow","darkorange3","red4")
   
    for(i in 1:4){
        a<-apply(Yearly[Years>=Present,Emissions==levels(Emissions)[i]],1,mean)
        lines(Years[Years>=Present],a,lwd=4,col=EmissionsCol[i])  
  } 
}



EmissionSDPlot<-function(Yearly,ParkName,Present){
#Yearly is a dataframe 
#   rows = years for observations
#   columns = to model output for an emissions scenario and climate model
#   rownames = year
#   colnames = model.emissions emissioncs should be 5 characters long "rcp85" for example
#ParkName = The name of the park for the title
#Present = The current year for determining color break 
#Written by Marian Talbert 10/2013
 
    Years<-as.numeric(rownames(Yearly))
    Emissions<-factor(substr(colnames(Yearly),start= nchar(colnames(Yearly))-4,stop= nchar(colnames(Yearly))))
    EmissionsBgCol<-c("seagreen3","lemonchiffon","orange","red","royalblue1")
    
    PastCol<-EmissionsCol[5]
    EmissionsCol<-EmissionsCol[1:4]                        
    ylabel<-as.expression(expression( paste("Celcius Temperature ( ", degree*C, ")") ))
    
  
    
    #Now plotting avgs by emissions scenario
   SDandAvg<-data.frame()
   EmissionsBgCol<-c("seagreen3","lemonchiffon","orange","red","royalblue1")
    EmissionsCol<-c("green4","yellow","darkorange3","red4")
    
     EmissionsBgCol<-c("green","yellow","orange","red","slateblue1")
    EmissionsCol<-c(rcp26="green3",rcp45="yellow",rcp60="orange",rcp85="red",past="slateblue3")
   #getting the avg and sd for each emissions scenario and future time
    for(i in 1:4){
        Avg<-apply(Yearly[Years>=Present,Emissions==levels(Emissions)[i]],1,mean)
        SD<-apply(Yearly[Years>=Present,Emissions==levels(Emissions)[i]],1,sd)
        if(ncol(SDandAvg)==0) SDandAvg<-cbind(Avg,SD)
        else SDandAvg<-cbind(SDandAvg,Avg,SD)
        colnames(SDandAvg)[(ncol(SDandAvg)-1):ncol(SDandAvg)]<-c(paste(levels(Emissions)[i],"Avg",sep=""),paste(levels(Emissions)[i],"SD",sep=""))
        }
  #sd and avg for past and present time
   Avg<-apply(Yearly[Years<=Present,],1,mean)
        SD<-apply(Yearly[Years<=Present,],1,sd)
        PastAvg<-data.frame(Years=Years[Years<=Present],Avg=Avg,SD=SD)
    rng<-range(SDandAvg)
            
    SDandAvg<-cbind(Years[Years>=Present],SDandAvg)
    colnames(SDandAvg)[1]<-"Years"
     #names(SDandAvg)[2:3]<-c("Avg","SD")
    SDandAvg<-as.data.frame(SDandAvg)
    AllAvgs<-merge(SDandAvg,PastAvg,by=1,all=TRUE)
    
    Alpha<-.2
g<- ggplot(AllAvgs,aes(x=Years,ymin=0,ymax=10)) +
    geom_ribbon(aes(ymin=rcp26Avg-rcp26SD, ymax=rcp26Avg+rcp26SD,colour="rcp26"),
    alpha=Alpha,colour=EmissionsCol[1],fill=EmissionsBgCol[1]) +
    geom_line(aes(y=rcp26Avg),colour=EmissionsCol[1],size=2)+
    
    geom_ribbon(aes(ymin=rcp45Avg-rcp45SD, ymax=rcp45Avg+rcp45SD,colour="rcp45"),
    alpha=Alpha,colour=EmissionsCol[2],fill=EmissionsBgCol[2]) +
    geom_line(aes(y=rcp45Avg),colour=EmissionsCol[2],size=2)+
    
    geom_ribbon(aes(ymin=rcp60Avg-rcp60SD, ymax=rcp60Avg+rcp60SD,colour="rcp60"),
    alpha=Alpha,colour=EmissionsCol[3],fill=EmissionsBgCol[3]) +
    geom_line(aes(y=rcp60Avg),colour=EmissionsCol[3],size=2)+
    
    geom_ribbon(aes(ymin=rcp85Avg-rcp85SD, ymax=rcp85Avg+rcp85SD,colour="rcp85"),
    alpha=Alpha,colour=EmissionsCol[4],fill=EmissionsBgCol[4]) +
    geom_line(aes(y=rcp85Avg),colour=EmissionsCol[4],size=2)+
    
    geom_ribbon(aes(ymin=Avg-SD, ymax=Avg+SD,colour="past"),
    alpha=Alpha,colour=EmissionsCol[5],fill=EmissionsBgCol[5]) +
    geom_line(aes(y=Avg),colour=EmissionsCol[5],size=2)+scale_fill_manual(name="Bar",values=EmissionsCol)+
    scale_colour_manual(name="Error Bars",values=EmissionsCol)+theme(legend.position=c(.5,.5))+
    ylab("Celcius Temperature")+labs(title = "4 Emisson Scenarios")
return(g)
}


 