RESIT JULY 2024
# Question 2 -------------------------------------------------------------------
grades2<-c(6, rep(9,2), rep(14,3), rep(15,5), rep(16,4), rep(17,5), rep(18,8), rep(19,4),
          20,21)
hist(grades2, breaks=seq(-0.5,22.5), xlim=c(0,22), xlab="Grade Points", main=NULL)
axis(side=1, at=seq(0,22), labels=seq(0,22))
length(grades2)

harrier<-read.table("harrier.txt", header=T)
summary(harrier)
#plot of mean values with 95%CI for two sexs of harriers
library(gplots)
plotmeans(tarsus ~ sex, data = harrier, connect = FALSE, main="mean +/- 95%CI")

#boxplot with sex as grouping variable
boxplot(harrier$logWeight~harrier$sex,
        ylab = "LogWeight", xlab = "sex")
model <- lm(harrier$logWeight ~ harrier$sex)
anova(model)
summary(model)
length(harrier$logWeight)
1.9679/(15.1286+1.9679)

prawn<-read.table("prawn.txt", header=T)
mean(prawn[prawn$Diet=="Artificial",]$GrowthR) # same as in quiz
sd(prawn[prawn$Diet=="Artificial",]$GrowthR)
mean(prawn[prawn$Diet=="Natural",]$GrowthR) # same as in quiz
sd(prawn[prawn$Diet=="Natural",]$GrowthR)
hist(prawn$GrowthR)
boxplot(prawn$GrowthR~prawn$Diet)
1-pnorm(12, mean(prawn[prawn$Diet=="Artificial",]$GrowthR), sd(prawn[prawn$Diet=="Artificial",]$GrowthR))
pnorm(9, mean(prawn[prawn$Diet=="Artificial",]$GrowthR), sd(prawn[prawn$Diet=="Artificial",]$GrowthR))
t.test(GrowthR ~ Diet, data=prawn, 
       alternative = "two.sided", var.equal = FALSE)
9.794-10.45

green<-read.table("green.txt", header=T)
str(green)
summary(green)
plot(green$TOTMASS~green$BURROWS)
boxplot(green$TOTMASS~green$SITE)
modelgreen<-lm(TOTMASS~SITE + BURROWS, data=green)
anova(modelgreen)
summary(modelgreen)

hist(green$TOTMASS) #produces a frequency histogram of the response variable
plot(green$TOTMASS) #produces a scatterplot of the response against order in dataset
plot(green$BURROWS) #produces a scatterplot of the predictor against order in dataset
plot(green$TOTMASSs~green$BURROWS)#produces a scatterplot of response against predictor

hist(modelgreen$residuals)
qqnorm(modelgreen$residuals)
qqline(modelgreen$residuals)
plot(modelgreen$residuals ~ modelgreen$fitted.values)
abline(1,0)
abline(0,0)
abline(-1,0)
plot(cooks.distance(modelgreen)~hatvalues(modelgreen))

answer<-(0.93+1.7499)+70*0.03424

limpet<-read.table("Limpet_dimensions.txt", header=T)
str(limpet)
summary(limpet)
plot(limpet$Length~limpet$Height)
boxplot(limpet$Mass~limpet$Position_on_shore)
modellimpet<-lm(Mass~Position_on_shore + Height + Length, data=limpet)
anova(modellimpet)
summary(modellimpet)


hist(modellimpet$residuals)
qqnorm(modellimpet$residuals)
qqline(modellimpet$residuals)
plot(modellimpet$residuals ~ modellimpet$fitted.values)
abline(1,0)
abline(0,0)
abline(-1,0)
plot(cooks.distance(modellimpet)~hatvalues(modellimpet))
boxplot(limpet$Mass~limpet$Position_on_shore)

