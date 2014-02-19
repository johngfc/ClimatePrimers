EmissionSDPlot<-function(InputDat,ParkName,Present,Main,yLab,DisplayOutput,OutputGraphics,cexMult){
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
    if(!DisplayOutput){ png(file.path(OutputGraphics,
       paste(InputDat@Var,min(InputDat@Year),"to",max(InputDat@Year),"EmissionsSD.png",sep="_")),height=1000,width=1000)
        on.exit(dev.off())
       }
   InputDat<-YrAgg(InputDat)
   if(missing(Main)) Main=paste("Mean and Standard deviations of Model Projections for", LongName(InputDat@Var),"\nUnder Future Emissions Scenarios for",ParkName)                  
   if(missing(yLab)) yLab=GenerateLab(InputDat)
      
    #Now plotting avgs by emissions scenario
   SDandAvg<-data.frame()
    
   EmissionsBgCol<-c("green","yellow","orange","red","slateblue1")
   EmissionsCol<-c("green3","yellow","orange","red","slateblue3")
   PastCol<-EmissionsBgCol[5]
  
   #getting the avg and sd for each emissions scenario and future time
    for(i in 1:4){
        Avg<-apply(InputDat@Ts[InputDat@Year>=Present,InputDat@Rcp==levels(InputDat@Rcp)[i]],1,mean)
        SD<-apply(InputDat@Ts[InputDat@Year>=Present,InputDat@Rcp==levels(InputDat@Rcp)[i]],1,sd)
        if(i==1) SDandAvg<-data.frame(Year=InputDat@Year[InputDat@Year>=Present],
                  Avg=Avg,SD=SD,Emissions=rep(levels(InputDat@Rcp)[i],times=length(Avg)))
        else SDandAvg<-rbind(SDandAvg,data.frame(Year=InputDat@Year[InputDat@Year>=Present],
                  Avg=Avg,SD=SD,Emissions=rep(levels(InputDat@Rcp)[i],times=length(Avg))))
        }
      
  #sd and avg for past and present time
       Avg<-apply(InputDat@Ts[InputDat@Year<=Present,],1,mean)
        SD<-apply(InputDat@Ts[InputDat@Year<=Present,],1,sd)
   SDandAvg<-rbind(SDandAvg,data.frame(Year=InputDat@Year[InputDat@Year<=Present],
        Avg=Avg,SD=SD,Emissions=rep("All",times=length(Avg))))
      
      SDandAvg$Emissions<-as.factor(SDandAvg$Emissions)
      
g<-ggplot(SDandAvg, aes(x = Year, y = Avg)) + geom_ribbon(aes(ymin = Avg-SD,
    ymax = Avg+SD, fill = Emissions,colour=Emissions), alpha = 0.3) + geom_line(aes(colour = Emissions),
    size = 1)+scale_colour_manual(values=EmissionsCol)+
    scale_fill_manual(values=EmissionsBgCol)+ylab(yLab)+ ggtitle(Main)     
plot(g) 
     
return(g)
}


 