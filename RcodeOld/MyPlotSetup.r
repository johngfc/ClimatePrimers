MyPlot<-function(x,y,drawGrid=TRUE,cexMult=1,bgCol="grey73",...){
    plot(x=range(x,na.rm=TRUE),y=range(y,na.rm=TRUE),type="n",cex.lab=cexMult,cex.axis=cexMult,...)
    Xext<-extendrange(x)
    Yext<-extendrange(y)
    rect(Xext[1],Yext[1],Xext[2],Yext[2],col=bgCol)
    if(drawGrid){
        grid(nx=10,ny=10,col="grey89",lty="solid",lwd=.2)
        grid(nx=5,ny=5,col="white",lty="solid",lwd=2)
    }
}