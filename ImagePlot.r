
ImagePlot<-function(InputDat,Ylab="",Xlab="",Plot=TRUE,Months,Colors,Baseline=c(1950,1980),Main,ColVar="TempChng",cexMult=1,
DisplayOutput=DisplayOutput,OutputGraphics=OutputGraphics){
     Call<-match.call()
#this doesn't look great for my current dataset but maybe eventually 
      
    if(!DisplayOutput){ png(file.path(OutputGraphics,
       paste(Call$InputDat,"Baseline",min(Baseline),"to",max(Baseline),"Image.png",sep="_")),height=1000,width=500)
        on.exit(dev.off())
        }
    if(missing(Main)) Main= paste(Call$InputDat,"with Baseline\n",min(Baseline),"to",max(Baseline),sep=" ")
    Dat<-matrix(data=InputDat@Ts,nrow=12,byrow=FALSE)    
    #This should also work for the GDO with a specified model 
    BsLnDat<-InputDat@Ts[InputDat@Year>=Baseline[1] &InputDat@Year>Baseline[2]]
    BsLnDat<-apply(matrix(data=BsLnDat,nrow=12,byrow=FALSE),1,mean) #we calculate the mean this way so it's monthly  
    
    Dat<-matrix(data=InputDat@Ts,nrow=12,byrow=FALSE)
    ID<-(Dat-BsLnDat) #/apply(InputDat,1,sd)
    if(missing(Colors)) Colors<-GenerateColors(ColVar)
    Colors=two.colors(n=256, start=Colors[1], end=Colors[length(Colors)], middle="gray89",alpha=1.0)
    Breaks<-SetBreaks(ID,"diff",Colors)
    image.plot(z=ID,x=seq(1:12),y=unique(InputDat@Year),xlab="Month",ylab=Ylab,col=Colors,main=Main,cex.lab=cex.mult,cex.axis=cex.mult,legend.mar=7.1,breaks=Breaks)    
}

