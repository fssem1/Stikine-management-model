#notes----
#June 2017
#Trigger run size code for the cpue-catch Stikine management model (smm) 
#When last updated, these triggers included data from 1994-2011
#the current data contains years 1994-2016
#SPA data drom 1994-2011; GSI data from 2012-2016
rm(list=ls(all=TRUE))
#load----
library(MuMIn)
library(dplyr)
library(readxl)
library(tidyverse)
library(stargazer)
library(tidyverse)
library(extrafont)
library(car)
#data----
Prop.D108.Data <- read_excel('data/Trigger_Data.xlsx', sheet="Prop.D108")
#data clean----
Prop.D108.Data %>% 
  mutate(Year = factor(Year),
         Stock = factor(Stock),
         RunSize = as.numeric(RunSize),
         log.StatWeek = as.numeric(log(StatWeek)),
         log.Prop.D108 = as.numeric(logit(Prop.D108+0.00001)),
         Size = ifelse(RunSize > 175000, 'High',
                        ifelse(RunSize < 46000, 'Low','Average')))->Prop.D108
#analysis-----
#Determine if the data is normally distributed (p should be >0.05)
eda.norm <- function(x, ...)
{
  par(mfrow=c(2,2))
  if(sum(is.na(x)) > 0)
    warning("NA's were removed before plotting")
  x <- x[!is.na(x)]
  hist(x, main = "Histogram and non-\nparametric density estimate", prob = T)
  iqd <- summary(x)[5] - summary(x)[2]
  lines(density(x, width = 2 * iqd))
  boxplot(x, main = "Boxplot", ...)
  qqnorm(x)
  qqline(x)
  plot.ecdf(x, main="Empirical and normal cdf")
  LIM <- par("usr")
  y <- seq(LIM[1],LIM[2],length=100)
  lines(y, pnorm(y, mean(x), sqrt(var(x))))
  shapiro.test(x)
}

eda.norm(Prop.D108$log.Prop.D108)

#results----
#Run Models
#low run size
A1 <- lm(formula = log.Prop.D108~ (log.StatWeek),data=Prop.D108, subset=Size=="Low") 
A2 <- lm(formula = log.Prop.D108~ poly(log.StatWeek,2),data=Prop.D108, subset=Size=="Low") 
A3 <- lm(formula = log.Prop.D108 ~ poly(log.StatWeek,3),data=Prop.D108, subset=Size=="Low") 
#high run size
A4 <- lm(formula = log.Prop.D108~ (log.StatWeek),data=Prop.D108, subset=Size=="High") 
A5 <- lm(formula = log.Prop.D108~ poly(log.StatWeek,2),data=Prop.D108, subset=Size=="High") 
A6 <- lm(formula = log.Prop.D108~ poly(log.StatWeek,3),data=Prop.D108, subset=Size=="High") 
#average run size (all data)
A7 <- lm(formula = log.Prop.D108~ (log.StatWeek),data=Prop.D108) 
A8 <- lm(formula = log.Prop.D108~ poly(log.StatWeek,2),data=Prop.D108) 
A9 <- lm(formula = log.Prop.D108~ poly(log.StatWeek,3),data=Prop.D108)

AICcResults<-data.frame (AICc(A1,A2,A3,A4,A5,A6,A7,A8,A9))
write.csv(AICcResults, file="results/AICc_D108.csv") #save AICc results
stargazer(A1, A2, A3, type="text", out="results/Prop.D108.models.Low.htm")#save results to folder
stargazer(A4, A5, A6, type="text", out="results/Prop.D108.models.High.htm")#save results to folder
stargazer(A7, A8, A9, type="text", out="results/Prop.D108.models.htm")#save results to folder
#determine best models based on output of results (AICc and regression results)

#figures----
#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

new.data<-data.frame(log.StatWeek=seq(24,35,1))
new.data<-log(new.data)
#low run size data (prediction)
prediction.low<-predict(A3, newdata=new.data, interval="prediction")
trans.prediction.low<-1/(1+(1/exp(prediction.low)))
trans.prediction.low<-trans.prediction.low[,1]
#high run size data (prediction)
prediction.high<-predict(A5, newdata=new.data, interval="prediction")
trans.prediction.high<-1/(1+(1/exp(prediction.high)))
trans.prediction.high<-trans.prediction.high[,1]
#average run size data (prediction)
prediction<-predict(A8, newdata=new.data, interval="prediction")
trans.prediction<-1/(1+(1/exp(prediction)))
trans.prediction<-trans.prediction[,1]
#prediction dataset for figures
new.data.fig<-cbind(trans.prediction.low, trans.prediction,trans.prediction.high,exp(new.data))

#create figures by run size
png("figures/Fitted_D108.png", res=600, height=4.5, width=6.5, units="in")
par(mfrow=c(1,3)) 
plot(Prop.D108~StatWeek,data=Prop.D108, subset=Size=="Low",  las=1, type="p", pch=16, cex=0.8,
     col=1,xlab="Statistical Week", ylab="Tah Proportion", main="Low (<46,000) run size D108 fishery",
     font.lab=1, xlim=c(24,35), ylim=c(0,1),
     cex.lab=1, cex.main=1, font.axis=1)
lines(trans.prediction.low~log.StatWeek,data=new.data.fig,col=1, pch=8, cex=0.8)
plot(Prop.D108~StatWeek,data=Prop.D108, subset=Size=="High",  las=1, type="p", pch=16, cex=0.8,
     col=1,xlab="Statistical Week", ylab="Tah Proportion", main="High (>175,000) run size D108 fishery",
     font.lab=1, xlim=c(24,35), ylim=c(0,1),
     cex.lab=1, cex.main=1, font.axis=1)
lines(trans.prediction.high~log.StatWeek,data=new.data.fig,col=1, pch=8, cex=0.8)
plot(Prop.D108~StatWeek,data=Prop.D108, subset=Size=="Average",  las=1, type="p", pch=16, cex=0.8,
     col=1,xlab="Statistical Week", ylab="Tah Proportion", main="Average run size D108 fishery",
     font.lab=1, xlim=c(24,35), ylim=c(0,1),
     cex.lab=1, cex.main=1, font.axis=1)
lines(trans.prediction~log.StatWeek,data=new.data.fig,col=1, pch=8, cex=0.8)
dev.off()

png("figures/Combined_D108.png", res=600, height=4.5, width=6.5, units="in")
par(mfrow=c(1,1)) 
plot(trans.prediction~log.StatWeek,data=new.data.fig,las=1, type="l", pch=16, cex=0.8,
     col=1,xlab="Statistical Week", ylab="Tah Proportion",main="D108 fishery",
     font.lab=1, ylim=c(0,1), xlim=c(24,35),
     cex.lab=1, cex.main=1, font.axis=1)
lines(trans.prediction.low~log.StatWeek,data=new.data.fig,col=2, pch=16, cex=0.8)
lines(trans.prediction.high~log.StatWeek,data=new.data.fig,col=3, pch=16, cex=0.8)
legend (32,0.9, legend=c("Low; <46,000", "High; >175,000", "Average;All Data"),
        cex=0.75,pch=16,col=c(2,3,1), bty="n", lwd=1)
dev.off()
new.data.fig$StatWeek<-new.data.fig$log.StatWeek
new.data.fig<-new.data.fig[,-4]
write.csv(new.data.fig, file="results/Prop.D108.Tah.Results.csv") #save prop results for smm model


#figures of confidence and prediction intervals
#par(mfrow=c(1,2)) 
#matplot(new.data$log.StatWeek, cbind(prediction[-1],confidence),
#        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
#matplot(exp(new.data$log.StatWeek), cbind(trans.prediction[-1],trans.confidence),
#        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

