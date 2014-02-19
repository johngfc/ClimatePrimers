GenerateLab<-function(x,addTime=FALSE,addNCDF=FALSE){
 #this generates some nice labels for either a ClimateTS object
 #or the mapped data object (x)
 
 quant<-switch(x@Var,
              Temp    = "Temperature",
              Tmax     = "Average~Temperature",
              Tmin    = "Minimum~Temperature",
              Precip  = "Precipitation",
              GrnPnk     = "",
              PurOrn  ="",
              PrecipChng = "Precipitation~Change",
              TempChng   = "Temperature~Change")

           #setting a default label and changing it in a few special cases
            Main = paste(quant, x@PlotUnits,sep="~")
           if(toupper(x@PlotUnits)%in%c("C","F","K")){
               Main<-paste(quant, "~({}^o*",x@PlotUnits,")",sep="")
            }
            if(toupper(x@PlotUnits)=="MM"){
               Main<-paste(quant, " (mm / month)",sep="")
            }
            if(x@PlotUnits=="kgm2s1"){
            Main = paste(quant,(kg*m^2*s^1),sep="~")
            }
            if(addTime){ Main = paste(
                     ifelse(length(x@Month==1),paste(month.name[x@Month],"~",sep=""),""),
                     ifelse(length(x@Year==1),x@Year,paste(min(x@Year),"~to~",max(x@Year),sep="")),
                     "~",Main,
                     sep="")
                 }
            if(addNCDF) Main=paste(gsub(".nc","",x@SourcePath),Main,sep="~")
            

           Main = parse(text=Main)
           return(Main)
}
LongName<-function(Var){
     switch(Var,
              Temp="Average Temperature",
              Tmax="Monthly Maximum Temperature",
              Tmin="Monthly Minimum Temperature",
              Precip="Monthly Precipitation")
              }