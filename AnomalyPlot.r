AnomalyPlot <- function(InputDat,Baseline,Ylab,Months,Main,ParkName,DisplayOutput,OutputGraphics,cexMult){
    #barplot is going to take a baseline period and optionally a time of year and
     
     #making appropriate labels
     if(missing(Main)) Main=paste(ParkName,"Difference from", min(Baseline),"to",max(Baseline),"Baseline")
     if(missing(Ylab)) Ylab=GenerateLab(InputDat)
     
     if(!DisplayOutput){ png(file.path(OutputGraphics,
       paste(InputDat@Var,"Baseline",min(Baseline),"to",max(Baseline),"Anomaly.png",sep="_")),height=1000,width=1000)
       on.exit(dev.off())
       }
     #get the fraction portion and determine which to keep based on desired months
    
     if(missing(Months)) Months=seq(1:12)    
     Keep <- InputDat@Month%in%Months

     #now aggregate the desired months to years
        
     cb <- aggregate(InputDat@Ts[Keep],FUN=mean,by=list(Year=InputDat@Year[Keep]))
      names(cb)[2]<-"Anomaly"
     BaselineYr<-(cb$Year<Baseline[2] & cb$Year>=Baseline[1])
     Baseline<-mean(cb[BaselineYr,2])
     cb$Anomaly<-cb$Anomaly-Baseline
     cb$valence[cb$Anomaly >= 0] <- "pos"
     cb$valence[cb$Anomaly < 0]  <- "neg"
    
     interp <- approx(cb$Year, cb$Anomaly, n=20000)
              # Put in a data frame and recalculate valence
              cbi <- data.frame(Year=interp$x, Anomaly=interp$y)
              cbi$valence[cbi$Anomaly >= 0] <- "pos"
              cbi$valence[cbi$Anomaly < 0]  <- "neg"
              cb <- cbi                   
    cb$pos<-cb$Anomaly>=0

   p<-ggplot(cb, aes(x=Year, y=Anomaly, fill=pos)) +
     geom_bar(stat="identity", position="identity",width=.1,alpha=.05) +
     scale_fill_manual(values=c("slateblue1","red"), guide=FALSE)+geom_hline(yintercept=0)+
     ggtitle(Main)+ylab(Ylab)+theme(axis.text = element_text(size = rel(cexMult))) +
    		theme(axis.title.y = element_text(size = rel(cexMult), angle = 90)) +
    		theme(axis.title.x = element_text(size = rel(cexMult)))+
    		theme(plot.title =element_text(size=rel(1.2*cexMult)))+
    		theme(plot.margin =unit(c(8,8,8,8),"mm"))
     plot(p)
      
}        
