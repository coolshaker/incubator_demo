setwd("C:/Users/Kevin/Dropbox/Job/applied/Insight/insight analysis demo") #choose the working directory
#save.image("safety_demo.RData") #save data
#load("safety_demo.RData") #load data

library("foreign")
library(extrafont)
loadfonts(device = "win")
library(ggplot2)

totCrash<-read.dbf("crash_events.csv", as.is = FALSE)

totCrash$SEVERITY<-5  #no injury
totCrash$SEVERITY[grepl('C',totCrash$EXT_OF_INJ)]<-4 #possible injury
totCrash$SEVERITY[grepl('B',totCrash$EXT_OF_INJ)]<-3 #non-incapacitating injury
totCrash$SEVERITY[grepl('A',totCrash$EXT_OF_INJ)]<-2 #incapacitating injury
totCrash$SEVERITY[grepl('K',totCrash$EXT_OF_INJ)]<-1 #fatality injury
totCrash$MONTH<-as.numeric(format(totCrash$ACCD_DTE,"%m"))
totCrash$YEAR<-as.numeric(format(totCrash$ACCD_DTE,"%Y"))
totCrash$MONTHINDEX<-totCrash$MONTH+(totCrash$YEAR-2005)*12

###accident frequency and severity by year in the whole study area
png(file="Overall accident tendency.png",width=8,height=5,units='in',res=500,family="Times New Roman")
d<-aggregate(CASE_NUM~CASE_YR+SEVERITY,data=totCrash,FUN=length)
names(d)<-c("Year","Severity","Frequency")
p<-ggplot(d, aes(x=Year,y=Frequency,group=Severity,fill=Severity)) + geom_area(aes(colour = Severity, fill= Severity), position = 'stack')
p+theme(text=element_text(size=14),aspect.ratio=0.5)
dev.off()

###accident frequency and severity by year in the whole study area
library(forecast)
png(file="Accident tendency by region.png",width=6,height=8,units='in',res=500,family="Times New Roman")
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE))

##ARIMA auto fit
```{r}
for (i in 1:6)
{
      crashByMonth<-aggregate(CASE_NUM~MONTHINDEX,data=subset(totCrash,NTAName==NTANameList[i]),FUN=length)
      #crashSeries<-ts(crashByMonth$CASE_NUM,frequency=12)
      
      #auot fit ARIMA model
      fit <- auto.arima(crashByMonth$CASE_NUM)
      
      #predict
      plot(forecast(fit,h=30),xaxt ="n",xlab="Month",ylab="Crash Frequency",font.lab=2,main=NTANameList[i])
      abline(v=(seq(0,120,4)), col="lightgray", lty="dotted")
      xDate<-seq(0,120,12)
      ticks<-seq(0,120,12)
      axis(side =1, at = ticks,labels=F)
      axis(1, xDate, ticks, cex.axis = 1,lwd.ticks =0)
}

dev.off()
