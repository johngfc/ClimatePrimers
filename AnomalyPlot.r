AnomalyPlot <- function(InputDat,Baseline,Ylab,Months){
    #barplot is going to take a baseline period and optionally a time of year and
     
     #get the fraction portion and determine which to keep based on desired months
    
     if(missing(Months)) Months=seq(1:12)
     Month <- as.numeric(names(InputDat))%%1 
     Keep <- round(Month,digits=3)%in%round(c((Months-1)/12),digits=3)
     InputDat <- InputDat[Keep]
     
     #now aggregate the desired months to years
     Year <- floor(as.numeric(names(InputDat)))
     cb <- aggregate(InputDat,FUN=mean,by=list(Year=Year))
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

   ggplot(cb, aes(x=Year, y=Anomaly, fill=pos)) +
     geom_bar(stat="identity", position="identity",width=.1,alpha=.05) +
     scale_fill_manual(values=c("slateblue1","red"), guide=FALSE)+geom_hline(yintercept=0)
}        
