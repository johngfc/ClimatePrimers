 Year <- floor(as.numeric(names(PrismTmin)))
InputDat<-PrismTmin
AllOnePlot=TRUE
 Ylab<-(expression(paste(Tmin, ~({}^o*C)))) 

MyBoxplot<-function(InputDat,GroupBy = "season",BarAvg=10,HeatBy="med",AllOnePlot=TRUE,Col=NA,Main="",Ylab){
#  InputDat =
#  GroupBy = season, month or year 
#  BarAvg = length of time to represent each bar 10 years by default but can be none
#  HeatBy = "med"(median) or "recent" how the color scale is organized

#Bar avg will use this cut of bins but not yet...
# bins<-cut(Year,breaks=seq(from=min(Year),to=max(Year),by=BarAvg),labels=seq(from=min(Year),to=max(Year),by=BarAvg))
 Year <- floor(as.numeric(names(InputDat)))
 Yr10 <- signif(Year,digits=3) 
 par(mar=c(5,5,3,1),oma=c(0,0,4,0))
if(GroupBy=="season"){
      Groups = list(Winter=c(12,1,2),Spring=c(3,4,5),Summer=c(6,7,8),Autumn=c(9,10,11))
      Col =  c("royalblue4","springgreen4","red","chocolate4")
       Dim<-c(2,2)
       }
if(GroupBy=="month"){
       Groups = split(seq(1:12),factor(month.abb,ordered=TRUE,levels=month.abb))
       if(is.na(Col) = Col= rep("red",times=12)
       Dim<-c(4,3)
       }  
 if(GroupBy=="year"){
       Groups = list(Yearly=seq(1:12))
       if(is.na(Col)) Col="red"
       Dim<-c(1,1)
       }   

 if(AllOnePlot) par(mfrow=c(Dim[1],Dim[2]))
 
 for(i in 1:length(Groups)){
     Month <- as.numeric(names(InputDat))%%1 #gives the decimal part or month of the number
     
     Keep <- round(Month,digits=3)%in%round(c((Groups[[i]]-1)/12),digits=3)
     InputDat2 <- InputDat[Keep]
     YrKeep<-Yr10[Keep]
     
     if(HeatBy=="med"){
       Med<-aggregate(InputDat2,by=list(YrKeep),median)
       r<-rank(Med$x)
     } else r<-c(1:length(unique(YrKeep)))

      jet.colors=colorRampPalette( c("white", Col[i]) )
      Color<-jet.colors(length(table(Yr10)))
      MyPlot(YrKeep,InputDat2,bgCol="gray85",xlab="",ylab="") 
  boxplot(InputDat2~YrKeep,col=Color[r],main=ifelse(GroupBy=="year","",names(Groups)[i]),ylab=Ylab,add=TRUE,at=unique(YrKeep),boxwex=8,xlab="Year")
  mtext(Main,side=3,outer=TRUE,cex=2)
  }
}


