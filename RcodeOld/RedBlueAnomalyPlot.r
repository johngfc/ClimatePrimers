library(gcookbook)
 cb <- subset(climate, Source=="Berkeley")

cb$valence[cb$Anomaly10y >= 0] <- "pos"
cb$valence[cb$Anomaly10y < 0]  <- "neg"

interp <- approx(cb$Year, cb$Anomaly10y, n=1000)

# Put in a data frame and recalculate valence
cbi <- data.frame(Year=interp$x, Anomaly10y=interp$y)
cbi$valence[cbi$Anomaly10y >= 0] <- "pos"
cbi$valence[cbi$Anomaly10y < 0]  <- "neg"
    
    
====================================

jpeg("H:\\Desktop\\MonthlyTemporalDatPlot\\AnomalyPlot.jpg",height=1200,width=1200,quality=100)
ggplot(cbi, aes(x=Year, y=Anomaly10y)) +
    geom_area(aes(fill=valence), alpha = .6) +
    geom_line() +
    geom_hline(yintercept=0) +
    scale_fill_manual(values=c("blue", "red"), guide=FALSE) +
    scale_x_continuous(expand=c(0, 0))
dev.off()