CurvesColorForTime<-function(InputDat,ColVar,Main,Ylab,DisplayOutput,OutputGraphics){
#This function draws one curve for each year using a yellow to red color scale
#I need to add more color scale options at some point
#also possibly a circular plot option
#I might also want to let this take a list of ClimateTsObjects
 if(!DisplayOutput){ jpeg(file.path(OutputGraphics,
             paste(gsub(".nc","",InputDat@SourcePath),InputDat@Var,"Curves.jpeg",sep="_")),height=500,width=1000)
              on.exit(dev.off()) 
 }             
Yrs<-unique(InputDat@Year) 

if(missing(Main)) Main=paste(InputDat@SourcePath,InputDat@Var,"curve in", InputDat@PlotUnits, "\nfor",min(Yrs),"to",max(Yrs),sep=" ")
if(missing(Ylab)) Ylab=GenerateLab(InputDat@Var,InputDat@PlotUnits)

ColorScale<-rev(heat.colors(length(Yrs)))
YearlyDat<-split(InputDat@Ts,InputDat@Year)

#setting up a plot region
 MyPlot(c(1,12),range(InputDat@Ts),bgCol="grey35",ylab=Ylab,xlab="Month",xaxt="n",main=Main,drawGrid=FALSE)
 mtext(month.abb,side=1,line=1,at=seq(1:12),las=2)

#adding lines
for(i in 1:length(Yrs))
  lines(seq(1:12),YearlyDat[[i]],col=ColorScale[i])

#adding the color legend
boxSeq<-seq(from=max(InputDat@Ts)-.2*diff(range(InputDat@Ts)),to=max(InputDat@Ts),length=length(ColorScale))
for(k in 1:(length(ColorScale)-1)){
          rect(11.5,boxSeq[k],12,boxSeq[k+1],col=ColorScale[k],lty="blank")
          if(Yrs[k]==min(Yrs)) text(11.15,boxSeq[k]+.5*(boxSeq[2]-boxSeq[1]),as.character(Yrs[k]),col="white",cex=cexMult)
          }
        text(11.15,boxSeq[k+1]+.5*(boxSeq[2]-boxSeq[1]),as.character(Yrs[k+1]),col="white",cex=cexMult)
}






