EmissionSDPlot<-function(InputDat,PastClim,ParkName,Present,Main,yLab,DisplayOutput,
  OutputGraphics,cexMult,f=1/30,rcp=c("rcp26","rcp45","rcp60","rcp85")){
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
   if(missing(Main)) Main=paste("Mean and Standard deviations of Model Projections for", 
        LongName(InputDat@Var),"\nUnder Future Emissions Scenarios for",ParkName)                  
   if(missing(yLab)) yLab=GenerateLab(InputDat)
      
    #Now plotting avgs by emissions scenario
   SDandAvg<-data.frame()
    
   EmissionsBgCol<-c("green","yellow","orange","red","slateblue1")
   EmissionsCol<-c("green3","yellow","orange","red","slateblue3")
  
   #getting the avg and sd for each emissions scenario and future time
    for(i in 1:4){
        Avg<-apply(InputDat@Ts[InputDat@Year>=Present,InputDat@Rcp==levels(InputDat@Rcp)[i]],1,mean)
        SD<-apply(InputDat@Ts[InputDat@Year>=Present,InputDat@Rcp==levels(InputDat@Rcp)[i]],1,sd)
        if(i==1) SDandAvg<-data.frame(
                  Year=InputDat@Year[InputDat@Year>=Present],
                  Avg=Avg,SD=SD,
                  Emissions=rep(levels(InputDat@Rcp)[i],times=length(Avg)))
        else SDandAvg<-rbind(SDandAvg,data.frame(Year=InputDat@Year[InputDat@Year>=Present],
                  Avg=Avg,SD=SD,Emissions=rep(levels(InputDat@Rcp)[i],times=length(Avg))))
        }
      
  #sd and avg for past and present time
       Avg<-apply(InputDat@Ts[InputDat@Year<=Present,],1,mean)
        SD<-apply(InputDat@Ts[InputDat@Year<=Present,],1,sd)
   SDandAvg<-rbind(SDandAvg,data.frame(Year=InputDat@Year[InputDat@Year<=Present],
        Avg=Avg,SD=SD,Emissions=rep("All",times=length(Avg))))
  
  #preparing past climate data for the plot
   if(!missing(PastClim)){
       #aggregate past clim to year
      if(PastClim@Var!=InputDat@Var) stop("Input variables are not the same")
       YearMean<-aggregate(PastClim@Ts,FUN=mean,list(PastClim@Year))
       YearMean[,2]<-lowess(YearMean[,1],y=YearMean[,2],f=f)$y
       YearMean<-YearMean[YearMean[,1]>=min(InputDat@Year),]
        SDandAvg<-rbind(SDandAvg,data.frame(Year=YearMean[,1],
            Avg=YearMean[,2],
            SD=rep(0,times=nrow(YearMean)),
            Emissions=rep(gsub(".nc","",PastClim@SourcePath),times=nrow(YearMean)))
            )
          EmissionsBgCol<-c(EmissionsBgCol,"black")
         EmissionsCol<-c(EmissionsCol,"black")   
   } 
 # if(length(rcp)!=4) for later
  
  SDandAvg$Emissions<-as.factor(SDandAvg$Emissions)
      
    
g<-ggplot(SDandAvg, aes(x = Year, y = Avg)) + geom_ribbon(aes(ymin = Avg-SD,
    ymax = Avg+SD, fill = Emissions,colour=Emissions), alpha = 0.4) + geom_line(aes(colour = Emissions),
    size = 1)+scale_colour_manual(values=EmissionsCol)+
    scale_fill_manual(values=EmissionsBgCol)+ylab(yLab)+ ggtitle(Main)+geom_line(aes(colour = Emissions),
    size = 1)+theme(axis.text.y = element_text(size = rel(cexMult))) +
    		theme(axis.title = element_text(size = rel(cexMult))) +	
    		theme(plot.title =element_text(size=rel(1.2*cexMult)))+
        theme(axis.text.x = element_text(size = rel(cexMult)))  
plot(g)
}


 