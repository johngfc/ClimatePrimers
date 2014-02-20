 

MyBoxplot<-function(InputDat,GroupBy = "season",BarAvg=10,HeatBy="med",AllOnePlot=TRUE,Col=NA,Main="",Ylab,DisplayOutput,OutputGraphics,cexMult){
#  InputDat =
#  GroupBy = season, month or year 
#  BarAvg = length of time to represent each bar 10 InputDat@Years by default but can be none
#  HeatBy = "med"(median) or "recent" how the color scale is organized
if(missing(Ylab)) Ylab=GenerateLab(InputDat)
if(!DisplayOutput){ png(file.path(OutputGraphics,
       paste(InputDat@Var,min(InputDat@Year),"to",max(InputDat@Year),"HeatBy",HeatBy,"Box.png",sep="_")),height=1000,width=1000)
       on.exit(dev.off()) 
        } 

 Yr10 <- signif(InputDat@Year,digits=3) 
 
par(mar=c(5,5,3,1),oma=c(0,0,4,0))
if(GroupBy=="season"){
      Groups = list(Winter=c(12,1,2),Spring=c(3,4,5),Summer=c(6,7,8),Autumn=c(9,10,11))
      Col =  c("royalblue4","springgreen4","red","chocolate4")
       Dim<-c(2,2)
       }
if(GroupBy=="month"){
       Groups = split(seq(1:12),factor(month.abb,ordered=TRUE,levels=month.abb))
       if(is.na(Col)) Col= brewer.pal(12,"Set3")
       Dim<-c(4,3)
       }  
 if(GroupBy=="year"){
       Groups = list(Yearly=seq(1:12))
       if(is.na(Col)) Col="red"
       Dim<-c(1,1)
       }   

if(AllOnePlot) par(mfrow=c(Dim[1],Dim[2]))
    
 for(i in 1:length(Groups)){
    
     Keep <- InputDat@Month%in%Groups[i][[1]]
     
     InputDat2 <- InputDat@Ts[Keep]
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


