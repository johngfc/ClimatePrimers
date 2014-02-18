EmissionLinePlot<-function(InputDat,ParkName,Present,DisplayOutput,OutputGraphics){
#InputDat is a dataframe 
#   rows = years for observations
#   columns = to model output for an emissions scenario and climate model
#   rownames = year
#   colnames = model.emissions emissioncs should be 5 characters long "rcp85" for example
#ParkName = The name of the park for the title
#Present = The current year for determining color break 
#Written by Marian Talbert 10/2013
    
    InputDat<-YrAgg(InputDat)
  
    if(!DisplayOutput) jpeg(file.path(OutputGraphics,
       paste(InputDat@Var,min(InputDat@Year),"to",max(InputDat@Year),"EmissionsLine.jpeg",sep="_")),height=1000,width=1000)
    EmissionsCol<-c("seagreen3","lemonchiffon","orange","red","royalblue1")
    color.box<-col2rgb(EmissionsCol,alpha=TRUE)
                           color.box[4,]<-150
                           temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
                           EmissionsCol<-apply(color.box/255,2,temp.fct)
    PastCol<-EmissionsCol[5]
    EmissionsCol<-EmissionsCol[1:4]                        
    ylabel<-as.expression(expression( paste("Celcius Temperature ( ", degree*C, ")") ))
    
     par(mar=c(5,5,4,2))                        
    MyPlot(InputDat@Year,range(InputDat@Ts),drawGrid=TRUE,cexMult=1,bgCol="grey38",
        main=paste("Average Surface Air Temperature for ",ParkName,"\nUnder Four Emissions Scenarios",sep=""),xlab="Year",ylab=ylabel)
    
    for (i in 1:ncol(InputDat@Ts)){
      lines(InputDat@Year[InputDat@Year>=Present],InputDat@Ts[InputDat@Year>=Present,i],col=EmissionsCol[InputDat@Rcp[i]],lwd=1.9)
      lines(InputDat@Year[InputDat@Year<=Present],InputDat@Ts[InputDat@Year<=Present,i],col=PastCol,lwd=.75)
      }
    #plotting the avg of everything before the present  
    lines(InputDat@Year[InputDat@Year<=Present],apply(InputDat@Ts[InputDat@Year<=Present,],1,mean),lwd=4,col="royalblue4") 
      #Now plotting avgs by emissions scenario
    EmissionsCol<-c("green4","yellow","darkorange3","red4")
       
    for(i in 1:4){
        a<-apply(InputDat@Ts[InputDat@Year>=Present,InputDat@Rcp==levels(InputDat@Rcp)[i]],1,mean)
        lines(InputDat@Year[InputDat@Year>=Present],a,lwd=4,col=EmissionsCol[i])  
  }
   if(!DisplayOutput) dev.off()   
}



EmissionSDPlot<-function(InputDat,ParkName,Present,Main="",DisplayOutput,OutputGraphics){
#InputDat is a dataframe 
#   rows = years for observations
#   columns = to model output for an emissions scenario and climate model
#   rownames = year
#   colnames = model.emissions emissioncs should be 5 characters long "rcp85" for example
#ParkName = The name of the park for the title
#Present = The current year for determining color break 
#Written by Marian Talbert 10/2013
        #=======================
        # aggregate to yearly
   InputDat<-YrAgg(InputDat)
        
   if(!DisplayOutput) jpeg(file.path(OutputGraphics,
      paste(InputDat@Var,min(InputDat@Year),"to",max(InputDat@Year),"EmissionsSD.jpeg",sep="_")),height=1000,width=1000)
               
   ylabel<-as.expression(expression( paste("Celcius Temperature ( ", degree*C, ")") ))
      
    #Now plotting avgs by emissions scenario
   SDandAvg<-data.frame()
    
   EmissionsBgCol<-c("green","yellow","orange","red","slateblue1")
   EmissionsCol<-c(rcp26="green3",rcp45="yellow",rcp60="orange",rcp85="red",past="slateblue3")
   PastCol<-EmissionsBgCol[5]
  
   #getting the avg and sd for each emissions scenario and future time
    for(i in 1:4){
        Avg<-apply(InputDat@Ts[InputDat@Year>=Present,InputDat@Rcp==levels(InputDat@Rcp)[i]],1,mean)
        SD<-apply(InputDat@Ts[InputDat@Year>=Present,InputDat@Rcp==levels(InputDat@Rcp)[i]],1,sd)
        if(ncol(SDandAvg)==0) SDandAvg<-cbind(Avg,SD)
        else SDandAvg<-cbind(SDandAvg,Avg,SD)
        colnames(SDandAvg)[(ncol(SDandAvg)-1):ncol(SDandAvg)]<-c(paste(levels(InputDat@Rcp)[i],"Avg",sep=""),paste(levels(InputDat@Rcp)[i],"SD",sep=""))
        }
        
  #sd and avg for past and present time
   Avg<-apply(InputDat@Ts[InputDat@Year<=Present,],1,mean)
        SD<-apply(InputDat@Ts[InputDat@Year<=Present,],1,sd)
        PastAvg<-data.frame(Years=InputDat@Year[InputDat@Year<=Present],Avg=Avg,SD=SD)
    rng<-range(SDandAvg)
            
    SDandAvg<-cbind(InputDat@Year[InputDat@Year>=Present],SDandAvg)
    colnames(SDandAvg)[1]<-"Years"
    
    SDandAvg<-as.data.frame(SDandAvg)
    AllAvgs<-merge(SDandAvg,PastAvg,by=1,all=TRUE)
   
    Alpha<-.2
    
g<- ggplot(AllAvgs,aes(x=Years)) +
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
    ylab("Celcius Temperature")+labs(title = "4 Emisson Scenarios")+ ggtitle(Main)
plot(g) 
     if(!DisplayOutput) dev.off()  
return(g)
}


 