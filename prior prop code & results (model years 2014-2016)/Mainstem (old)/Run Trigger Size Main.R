rm(list=ls(all=TRUE))
#load packages
library(MuMIn)

#STEP #1: Import DATA (Run Trigger Size by Stock & District)
Data1<- read.table("clipboard", header=T, sep="\t") #108 MAIN
Data2<- read.table("clipboard", header=T, sep="\t") #106-41 MAIN

#STEP #2: Determine if data is normally distributed (p-value should be >0.05)
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
attach(Data1)
eda.norm(logitProp)
attach(Data2)
eda.norm(logitProp)

#STEP #3: RUN MODELS
A1 <- lm(formula = logitProp~ (lnStatWeek),data=Data1, subset=Size=="Low") 
A2 <- lm(formula = logitProp~ poly(lnStatWeek,2),data=Data1, subset=Size=="Low") 
A3 <- lm(formula = logitProp ~ poly(lnStatWeek,3),data=Data1, subset=Size=="Low") 
A4 <- lm(formula = logitProp~ (lnStatWeek),data=Data1, subset=Size=="High") 
A5 <- lm(formula = logitProp~ poly(lnStatWeek,2),data=Data1, subset=Size=="High") 
A6 <- lm(formula = logitProp~ poly(lnStatWeek,3),data=Data1, subset=Size=="High") 
A7 <- lm(formula = logitProp~ (lnStatWeek),data=Data1) 
A8 <- lm(formula = logitProp~ poly(lnStatWeek,2),data=Data1) 
A9 <- lm(formula = logitProp~ poly(lnStatWeek,3),data=Data1)

B1 <- lm(formula = logitProp~ (lnStatWeek),data=Data2, subset=Size=="Low") 
B2 <- lm(formula = logitProp~ poly(lnStatWeek,2),data=Data2, subset=Size=="Low") 
B3 <- lm(formula = logitProp ~ poly(lnStatWeek,3),data=Data2, subset=Size=="Low") 
B4 <- lm(formula = logitProp~ (lnStatWeek),data=Data2, subset=Size=="High") 
B5 <- lm(formula = logitProp~ poly(lnStatWeek,2),data=Data2, subset=Size=="High") 
B6 <- lm(formula = logitProp~ poly(lnStatWeek,3),data=Data2, subset=Size=="High") 
B7 <- lm(formula = logitProp~ (lnStatWeek),data=Data2) 
B8 <- lm(formula = logitProp~ poly(lnStatWeek,2),data=Data2) 
B9 <- lm(formula = logitProp~ poly(lnStatWeek,3),data=Data2)

#STEP #4: OUTPUT OF MODELS
data.frame (AICc(A1,A2,A3,A4,A5,A6,A7,A8,A9))
data.frame (AICc(B1,B2,B3,B4,B5,B6,B7,B8,B9))
summary(A1)#Go through each model summary to get R squared and coefficient values for tables

nd<-data.frame(lnStatWeek=c(3.17805,
                            3.21888,
                            3.25810,
                            3.29584,
                            3.33220,
                            3.36730,
                            3.40120,
                            3.43399,
                            3.46574,
                            3.49651,
                            3.52636,
                            3.55535,
                            3.58352,
                            3.61092))
prediction<-predict(A9, newdata=nd, interval="prediction")
exp(prediction)


#STEP #5: OUTPUT PREDICTION FRAME FOR ALL MODELS
pred.frame<-data.frame(lnStatWeek=seq(3.178054,3.8,0.01))
pc_A1<-predict(A1,newdata=pred.frame,interval="confidence", level=0.95)
pc_A2<-predict(A2,newdata=pred.frame,interval="confidence", level=0.95)
pc_A3<-predict(A3,newdata=pred.frame,interval="confidence", level=0.95)
pc_A4<-predict(A4,newdata=pred.frame,interval="confidence", level=0.95)
pc_A5<-predict(A5,newdata=pred.frame,interval="confidence", level=0.95)
pc_A6<-predict(A6,newdata=pred.frame,interval="confidence", level=0.95)
pc_A7<-predict(A7,newdata=pred.frame,interval="confidence", level=0.95)
pc_A8<-predict(A8,newdata=pred.frame,interval="confidence", level=0.95)
pc_A9<-predict(A9,newdata=pred.frame,interval="confidence", level=0.95)

pc_B1<-predict(B1,newdata=pred.frame,interval="confidence", level=0.95)
pc_B2<-predict(B2,newdata=pred.frame,interval="confidence", level=0.95)
pc_B3<-predict(B3,newdata=pred.frame,interval="confidence", level=0.95)
pc_B4<-predict(B4,newdata=pred.frame,interval="confidence", level=0.95)
pc_B5<-predict(B5,newdata=pred.frame,interval="confidence", level=0.95)
pc_B6<-predict(B6,newdata=pred.frame,interval="confidence", level=0.95)
pc_B7<-predict(B7,newdata=pred.frame,interval="confidence", level=0.95)
pc_B8<-predict(B8,newdata=pred.frame,interval="confidence", level=0.95)
pc_B9<-predict(B9,newdata=pred.frame,interval="confidence", level=0.95)

pc_A1
pc_A2
pc_A3
pc_A4
pc_A5
pc_A6
pc_A7
pc_A8
pc_A9

pc_B1
pc_B2
pc_B3
pc_B4
pc_B5
pc_B6
pc_B7
pc_B8
pc_B9

#STEP #6: GRAPH BEST MODELS (Change model number in plots for low, medium, high, & combined models)
#D108 Tahltan
Data3<- read.table("clipboard", header=T, sep="\t") #Fitted Values
par(mfrow=c(1,3)) 
plot(Prop~StatWeek,data=Data1, subset=Size=="Low",  las=1, type="p", pch=16, cex=0.8,
     col=1,xlab="Stat Week", ylab="Prop", main="Low Run Size D108 Main",
     font.lab=1, 
     cex.lab=1, cex.main=1, font.axis=1)
lines (A3~StatWeek,data=Data3,col=1, pch=8, cex=0.8)


plot(Prop~StatWeek,data=Data1, subset=Size=="High",  las=1, type="p", pch=16, cex=0.8,
     col=1,xlab="Stat Week", ylab="Prop", main="High Run Size D108 Main",
     font.lab=1, 
     cex.lab=1, cex.main=1, font.axis=1)
lines (A6~StatWeek,data=Data3,col=1, pch=8, cex=1.8)


plot(Prop~StatWeek,data=Data1, las=1, type="p", pch=16, cex=0.8,
     col=1,xlab="Stat Week", ylab="Prop", main="D108 Main",
     font.lab=1, 
     cex.lab=1, cex.main=1, font.axis=1)
lines (A9~StatWeek,data=Data3,col=1, pch=8, cex=1.8)


plot(Prop~StatWeek,data=Data2, subset=Size=="Low",  las=1, type="p", pch=16, cex=0.8,
     col=1,xlab="Stat Week", ylab="Prop", main="Low Run Size D106-41 Main",
     font.lab=1, 
     cex.lab=1, cex.main=1, font.axis=1)
lines (B1~StatWeek,data=Data3,col=2, pch=8, cex=0.8)
lines (B2~StatWeek,data=Data3,col=3, pch=8, cex=0.8)
lines (B3~StatWeek,data=Data3,col=4, pch=8, cex=0.8)
legend (32,0.4, legend=c("B1", "B2", "B3"),
        cex=0.75,pch=16,col=c(2,3,4), bty="n", lwd=1)


plot(Prop~StatWeek,data=Data2, subset=Size=="High",  las=1, type="p", pch=16, cex=0.8,
     col=1,xlab="Stat Week", ylab="Prop", main="High Run Size D106-41 Main",
     font.lab=1, 
     cex.lab=1, cex.main=1, font.axis=1)
lines (B4~StatWeek,data=Data3,col=2, pch=8, cex=0.8)
lines (B5~StatWeek,data=Data3,col=3, pch=8, cex=0.8)
lines (B6~StatWeek,data=Data3,col=4, pch=8, cex=0.8)
legend (32,0.4, legend=c("B4", "B5", "B6"),
        cex=0.75,pch=16,col=c(2,3,4), bty="n", lwd=1)

plot(Prop~StatWeek,data=Data2, las=1, type="p", pch=16, cex=0.8,
     col=1,xlab="Stat Week", ylab="Prop", main="D106-41 Main",
     font.lab=1, 
     cex.lab=1, cex.main=1, font.axis=1)
lines (B7~StatWeek,data=Data3,col=2, pch=8, cex=0.8)
lines (B8~StatWeek,data=Data3,col=3, pch=8, cex=0.8)
lines(B9~StatWeek,data=Data3,col=4, pch=8, cex=0.8)
legend (32,0.4, legend=c("B7", "B8", "B9"),
        cex=0.75,pch=16,col=c(2,3,4), bty="n", lwd=1)


#STEP #7: DIAGNOSTICS (BEST MODEL); Change model number based on model that figures pertain to
par(mfrow=c(2,2)) 
plot(A9, which=2, main="Figure A")
plot(A9, which=2, main="Figure B")
plot(A9, which=3, main="Figure C")
plot(A9, which=4, main="Figure D")
plot(A9, which=5, main="Figure E")
plot(A9, which=6, main="Figure F")

#step #8: SHOW FITTED VALUES IN A FIGURE FOR LOW, HIGH
Data3<- read.table("clipboard", header=T, sep="\t") #Fitted Values
par(mfrow=c(1,1)) 
plot(A3~StatWeek,data=Data3,las=1, type="l", pch=16, cex=0.8,
     col=1,xlab="Stat Week", ylab="Prop", main="D108 Prop. Mainstem",
     font.lab=1, ylim=c(0,0.7), xlim=c(24,41),
     cex.lab=1, cex.main=1, font.axis=1)
lines(A6~StatWeek,data=Data3,col=2, pch=16, cex=0.8)
lines(A9~StatWeek,data=Data3,col=3, pch=16, cex=0.8)
legend (35.4,0.7, legend=c("Low (A3)", "High (A6)", "All Data (A9)"),
        cex=0.75,pch=16,col=c(1,2,3), bty="n", lwd=1)

library(lattice)
Data1$Year <- factor(Data1$Year)
xyplot(Prop ~ StatWeek|Year, data=Data1,font.lab=1, font.axis=1, type="l",las=1, family="A",
       xlab=list("Stat Week", font=1, cex=1.2), ylab=list("Prop. Mainstem D108",font=1,cex=1.2),
       col=1, pch=16, cex=0.75, layout=c(5,5), ylim=c(0.0,1.0), xlim=c(24,35))
strip.background <- trellis.par.get("strip.background") 
trellis.par.set(strip.background = list(col = 0)) 
update(trellis.last.object(),scales=list(cex=0.75, font=1))

Data2$Year <- factor(Data2$Year)
xyplot(Prop~ StatWeek|Year, data=Data2,font.lab=1, font.axis=1, type="l",las=1, family="A",
       xlab=list("Stat Week", font=1, cex=1.2), ylab=list("Prop. Mainstem D106-41",font=1,cex=1.2),
       col=1, pch=16, cex=0.75, layout=c(6,6), ylim=c(0.0,1.0), xlim=c(24,35))
strip.background <- trellis.par.get("strip.background") 
trellis.par.set(strip.background = list(col = 0)) 
update(trellis.last.object(),scales=list(cex=0.75, font=1))

