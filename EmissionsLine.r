EmissionLinePlot<-function(InputDat,ParkName,Present,Main,yLab,DisplayOutput,OutputGraphics,cexMult){
#InputDat is a dataframe 
#   rows = years for observations
#   columns = to model output for an emissions scenario and climate model
#   rownames = year
#   colnames = model.emissions emissioncs should be 5 characters long "rcp85" for example
#ParkName = The name of the park for the title
#Present = The current year for determining color break 
#Written by Marian Talbert 10/2013
    
    InputDat<-YrAgg(InputDat)
  
    if(!DisplayOutput){ png(file.path(OutputGraphics,
       paste(InputDat@Var,min(InputDat@Year),"to",max(InputDat@Year),"EmissionsLine.png",sep="_")),height=1000,width=1000)
     on.exit(dev.off())
    }
     if(missing(Main)) Main=paste("Model Projections for", LongName(InputDat@Var),
     "\nUnder Future Emissions Scenarios for",ParkName)                  
   if(missing(yLab)) yLab=GenerateLab(InputDat)
    EmissionsCol<-c("seagreen3","lemonchiffon","orange","red","royalblue1")
    color.box<-col2rgb(EmissionsCol,alpha=TRUE)
                           color.box[4,]<-60
                           temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
                           EmissionsCol<-apply(color.box/255,2,temp.fct)
    PastCol<-EmissionsCol[5]
    EmissionsCol<-EmissionsCol[1:4]                        
    ylabel<-as.expression(expression( paste("Celcius Temperature ( ", degree*C, ")") ))
    
     par(mar=c(5,5,4,2))                        
    MyPlot(InputDat@Year,range(InputDat@Ts),drawGrid=TRUE,cexMult=1,bgCol="grey88",
        main=Main,xlab="Year",ylab=ylabel,cex.main=cexMult,cex.lab=cexMult)
    
    for (i in 1:ncol(InputDat@Ts)){
      lines(InputDat@Year[InputDat@Year>=Present],InputDat@Ts[InputDat@Year>=Present,i],
      col=EmissionsCol[InputDat@Rcp[i]],lwd=1.9)
      lines(InputDat@Year[InputDat@Year<=Present],InputDat@Ts[InputDat@Year<=Present,i],col=PastCol,lwd=.75)
      }
    #plotting the avg of everything before the present  
    lines(InputDat@Year[InputDat@Year<=Present],apply(InputDat@Ts[InputDat@Year<=Present,],1,mean),lwd=4,col="royalblue4") 
      #Now plotting avgs by emissions scenario
    LineCol<-c("green4","yellow","darkorange3","red4")
       
    for(i in 1:4){
        a<-apply(InputDat@Ts[InputDat@Year>=Present,InputDat@Rcp==levels(InputDat@Rcp)[i]],1,mean)
        lines(InputDat@Year[InputDat@Year>=Present],a,lwd=4,col=LineCol[i])  
  }
  EmissionsCol<-c("seagreen3","lemonchiffon","orange","red","royalblue1")
  l<-as.character(levels(InputDat@Rcp))
  legend("topleft",legend=c("All",rev(l)),fill=rev(EmissionsCol),cex=cexMult,bty="n")
}


