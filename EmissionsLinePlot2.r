library(ncdf)
library(fields)
library(maps)
library(RNCEP) #for some nice color scales
#reading in the data this is avgerage temperature I believe but I also have max, min and precip
# this data I down'loaded from a ling on barsuglis page it's cmip 3 projections
#These are cmip3 data from the site:
#http://gdo-dcp.ucllnl.org/downscaled_cmip_projections/dcpInterface.html
#these are cmip5 projections downloaded BY HAND from the above
setwd("H:\\Desktop\\Climate")
#setwd("C:\\Users\\mallen\\Desktop\\VisualizingClimateData\\ClimateData\\SWCo1950To2100")
Clim<-open.ncdf("bcsd5\\Extraction_tas.nc")
LongitudeOffset<-360 #in the last dataset 150 worked I'm not sure why
Region=c("wyoming","montana")
#===============================
# Extracting info
Lat<-Clim$dim$latitude$vals
Lon<-(Clim$dim$longitude$vals-LongitudeOffset) # I really have no idea how the longitudes got changed...
Time<-Clim$dim$time$vals
Year<-1950+floor(Time/365)
Month<-rep(c("Jan","Feb","March","April","May","June",
   "July","Aug","Sep","Oct","Nov","Dec"),times=length(unique(Year)))

#clip off the extra if the last year wasn't complete
Month<-Month[1:length(Year)]

#these are projections from 43 climate models 
TempAvg<-get.var.ncdf(nc=Clim,varid="tas")

#for any of these we can plot the spatial projection I'm doing the first and last
image.plot(TempAvg[,,1,1],col=tim.colors())
image.plot(TempAvg[,,dim(TempAvg)[3],dim(TempAvg)[4]],col=tim.colors())

#===================================
# with regional baseline calculated we can plot

#this is four dimensional lat, lon, time, projection
#we can average over the area and produce line plots for each projection
AreaAvg<-apply(TempAvg,c(3,4),mean)
#then aggregate to year
Yearly<-aggregate(AreaAvg,by=list(Year),FUN=mean)
#remove last year because it's incomplete
Yearly<-Yearly[-c(nrow(Yearly)),]

#================================================================
#Emissions Line plot by color

#================================================================
#Ok well let's just assume we could calculate the baseline from weather data and move on...

ModelKey<-(read.table(ModelKeyPath,stringsAsFactors=FALSE))[,1]
Years<-Yearly[,1]
Yearly<-Yearly[,-1]

rownames(Yearly)<-Years
colnames(Yearly)<-ModleKey

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
      lines(Years[Year>=Present],Yearly[Year>=Present,i],col=EmissionsCol[Emissions[i]],lwd=1.9)
      lines(Years[Year<=Present],Yearly[Year<=Present,i],col=PastCol,lwd=.75)
      }
    #plotting the avg of everything before the present  
    lines(Years[Year<=Present],apply(Yearly[Year<=Present,],1,mean),lwd=4,col="royalblue4")  
    
    #Now plotting avgs by emissions scenario
    EmissionsCol<-c("green4","yellow","darkorange3","red4")
    for(i in 1:4){
        a<-apply(Yearly[Year>=Present,Emissions==levels(Emissions)[i]],1,mean)
        lines(Years[Year>=Present],a,lwd=4,col=EmissionsCol[i])  
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
ggplot(AllAvgs,aes(x=Years,ymin=0,ymax=10)) +
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
ylab("Celcius Temperature")+labs(title = "4 Emisson Scenarios")+

 z <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
z
z + theme(legend.position = "none")
z + theme(legend.position = "bottom")
# Or use relative coordinates between 0 and 1
z + theme(legend.position = c(.5, .5))
z + theme(legend.background = element_rect(colour = "black"))
# Legend margin controls extra space around outside of legend:
z + theme(legend.background = element_rect(), legend.margin = unit(1, "cm"))
z + theme(legend.background = element_rect(), legend.margin = unit(0, "cm"))


theme(
legend.background = element_rect(fill="grey85", colour="red", size=1),
legend.title = element_text(colour="blue", face="bold", size=14)



scale_colour_manual(values = cols)

scale_colour_manual(values = EmissionsCol,breaks=c("rcp 26","rcp 45","rcp 60","rcp 85","past"))



a<- as.data.frame(cbind(Years[Years>=Present],Avg,SD))
names(a)<-c("Years","Avg","SD")

ggplot(a, aes(x=Years, ymin=min(Yearly),ymax=max(Yearly))) +
geom_ribbon(aes(ymin=Avg-SD, ymax=Avg+SD),
alpha=0.2,colour="blue",fill="blue") +
geom_line(aes(y=Avg))


ggplot(SDandAvg, aes(x=Years, ymin=min(SDandAvg),ymax=max(SDandAvg))) +
geom_ribbon(aes(ymin=Avg-SD, ymax=Avg+SD),
alpha=0.2,colour="blue",fill="blue") +
geom_line(aes(y=Avg))



ggplot() + 
  geom_ribbon(data = a, aes(x = Years, ymin = min(Yearly), ymax = max(Yearly), 
                 fill = 'Target'), alpha = 0.4) + 
  geom_ribbon(data = b, aes(x = Years, ymin = min(Yearly), ymax = max(Yearly) 
                 fill = 'Observed'), alpha = 0.4) + 
  #scale_fill_manual("", c('Target' = 'blue', 'Observed' = 'orange')) + 
  theme(legend.position = c(0.88, 0.85), 
       legend.background = theme_rect(colour = 'transparent'), 
       legend.text = theme_text(size = 12))




huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
library(plyr) # to access round_any
huron$decade <- round_any(huron$year, 10, floor)

h <- ggplot(huron, aes(x=year))

h + geom_ribbon(aes(ymin=0, ymax=level))
h + geom_area(aes(y = level))

# Add aesthetic mappings
h + geom_ribbon(aes(ymin=level-1, ymax=level+1))
h + geom_ribbon(aes(ymin=level-1, ymax=level+1)) + geom_line(aes(y=level))

# Take out some values in the middle for an example of NA handling
huron[huron$year > 1900 & huron$year < 1910, "level"] <- NA
h <- ggplot(huron, aes(x=year))
h + geom_ribbon(aes(ymin=level-1, ymax=level+1)) + geom_line(aes(y=level))

# Another data set, with multiple y's for each x
m <- ggplot(movies, aes(y=votes, x=year))
(m <- m + geom_point())

# The default summary isn't that useful
m + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max")
m + stat_summary(geom="ribbon", fun.data="median_hilow")

# Use qplot instead
qplot(year, level, data=huron, geom=c("area", "line"))



 
    ggplot(a, aes(ymin=Year, y=access1-0.1.rcp45))+
     + geom_line() +
    geom_hline(yintercept=0)
    
    geom_area(aes(fill=valence), alpha = .6) +
    geom_line() +
    geom_hline(yintercept=0) +
    scale_fill_manual(values=c("blue", "red"), guide=FALSE) +
    scale_x_continuous(expand=c(0, 0))
}
}