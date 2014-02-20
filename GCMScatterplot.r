 

GCMScatterplot<-function(GCMx,GCMy,timeBreak=Present,season="Summer",XLab,YLab,Col=c("mediumorchid3","seagreen3"),Main,DisplayOutput,OutputGraphics,rcp=c("rcp26","rcp45","rcp60","rcp85"),cexMult){
 Call<-match.call()
#this doesn't look great for my current dataset but maybe eventually 
      
 if(!DisplayOutput){png(file.path(OutputGraphics,
       paste(Call$InputDat,"time cuttoff",timeBreak,season,"TwoTimeScatter.png",sep="_")),height=700,width=700)
        on.exit(dev.off())
}
if(missing(XLab))  
   XLab=paste(LongName(GCMx@Var)," for ",season,sep="")
   
if(missing(YLab)) 
 YLab=paste(LongName(GCMy@Var), " for ",season,sep="")

if(missing(Main))
 Main=paste(season,"Climate projections using the Green Data Oasis\n CMIP5 Projections for",ParkName,"for\n",paste(rcp,collapse=", "),sep=" ")       
Months<-switch(season,
           Summer=c(6,7,8),
           Winter=c(12,1,2),
           Fall=c(9,10,11),
           Spring=c(3,4,5))
 Months<-GCMx@Month%in%Months   
#incrasing the transparance
color.box<-col2rgb(Col,alpha=TRUE)
                           color.box[4,]<-50
                           temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
                           Color<-apply(color.box/255,2,temp.fct)
par(mar=c(5,6,6,2))                           
MyPlot(GCMx@Ts[Months,],GCMy@Ts[Months,],drawGrid=TRUE,cexMult=1,bgCol="grey92",
        main=Main,xlab=XLab,ylab=YLab,cex.lab=cexMult,cex.main=cexMult)
for(i in 1:ncol(GCMx@Ts)){#for every model if more than 1
  points(GCMx@Ts[Months,i],GCMy@Ts[Months,i],col=Color[as.factor(GCMx@Year[Months]<=timeBreak)],pch=19,cex=.05)
}
legend("bottomright",fill=Col,legend=c(paste("post",timeBreak),paste("pre",timeBreak)),cex=cexMult)
}
