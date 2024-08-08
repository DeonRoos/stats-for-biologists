#leprosy - question 1
leprosy<-read.table("leprosy_datasets.txt", header=TRUE)
str(leprosy)
head(leprosy)
leprosy$time<-as.factor(leprosy$time)
str(leprosy)
leprosy<-within(leprosy, time<-relevel(time, ref="before"))
model_leprosy<-lm(leprosy$muscle_weakness~leprosy$time*leprosy$treatment_group)
anova(model_leprosy)
summary(model_leprosy)
library(car)
library(ggplot2)
par(mfrow = c(2, 2))
hist(model_leprosy$residuals)
qqPlot(model_leprosy$residuals)
plot(model_leprosy$residuals~model_leprosy$fitted.values)
abline(5,0)
abline(0,0)
abline(-5,0)
plot(cooks.distance(model_leprosy) ~ hatvalues(model_leprosy))
par(mfrow = c(1, 1))
boxplot(leprosy$muscle_weakness~leprosy$time*leprosy$treatment_group, 
        xlab="time x treatment", ylab="muscle weakness", col=c("wheat1","wheat3","slategray1","slategray3"))

#blooms
blooms<-read.table("blooms.txt", header=TRUE)
str(blooms)
head(blooms)
length(blooms$SQBLOOMS)
blooms$BED<-as.factor(blooms$BED)
blooms$WATER<-as.factor(blooms$WATER)
blooms$SHADE<-as.factor(blooms$SHADE)
str(blooms)
model_blooms<-lm(blooms$SQBLOOMS~blooms$BED+blooms$WATER+blooms$SHADE)
anova(model_blooms)
summary(model_blooms)
par(mfrow = c(2, 2))
hist(model_blooms$residuals)
qqPlot(model_blooms$residuals)
plot(model_blooms$residuals~model_blooms$fitted.values)
abline(0.5,0)
abline(0,0)
abline(-0.5,0)
plot(cooks.distance(model_blooms) ~ hatvalues(model_blooms))
boxplot(model_blooms$residuals~blooms$BED)
boxplot(model_blooms$residuals~blooms$WATER)
boxplot(model_blooms$residuals~blooms$SHADE)
par(mfrow = c(1, 1))
summary(blooms$SQBLOOMS)
max(blooms$SQBLOOMS)
median(blooms$SQBLOOMS)
blooms$blooms<-blooms$SQBLOOMS^2
summary(blooms$blooms)
hist(blooms$blooms)
hist(blooms$SQBLOOMS)

model_blooms2<-lm(blooms$blooms~blooms$BED+blooms$WATER+blooms$SHADE)
anova(model_blooms2)
summary(model_blooms2)
par(mfrow = c(2, 2))
hist(model_blooms2$residuals)
qqPlot(model_blooms2$residuals)
plot(model_blooms2$residuals~model_blooms2$fitted.values)
abline(5,0)
abline(0,0)
abline(-5,0)
plot(cooks.distance(model_blooms2) ~ hatvalues(model_blooms2))

model_blooms3<-lm(blooms$SQBLOOMS~blooms$BED+blooms$WATER+blooms$SHADE+blooms$BED*blooms$SHADE)
anova(model_blooms3)
summary(model_blooms3)
par(mfrow = c(2, 2))
hist(model_blooms3$residuals)
qqPlot(model_blooms3$residuals)
plot(model_blooms3$residuals~model_blooms3$fitted.values)
abline(5,0)
abline(0,0)
abline(-5,0)
plot(cooks.distance(model_blooms3) ~ hatvalues(model_blooms3))

#bodyfat
bodyfat<-read.table("bodyfat.txt", header=TRUE)
str(bodyfat)
# test for unequal variances
t.test(WEIGHT ~ SEX, data=bodyfat, 
       alternative = "two.sided", var.equal = FALSE)

#plantlets
plants<-read.table("plantlets.txt", header=TRUE)
str(plants)
plants$SPECIES<-as.factor(plants$SPECIES)
plot(plants$LEAFSIZE~plants$LONGEV)
pairs(plants[,1:3])
table(plants$SPECIES)
plot(plants$LONGEV~plants$HGHT)

#partridge
partridge<-read.table("partridge.txt", header=TRUE)
str(partridge)
par(mfrow = c(2, 2))
boxplot(partridge$LLONGEV~partridge$TYPE, xlab="type of partner", ylab="longevity", col=c(1:3))
boxplot(partridge$LLONGEV~partridge$PARTNERS, xlab="number of partners", ylab="longevity", col=c(5:7))
col=(partridge$TYPE)
plot(partridge$LLONGEV~partridge$THORAX, col=c(1:3), pch=c(16:18), legend="TRUE")
legend("bottomright", title = "Type of partners", legend = c("0", "1", "9"), fill = (1:3))
partridge$TYPE<-as.factor(partridge$TYPE)
col=(partridge$PARTNERS)
plot(partridge$LLONGEV~partridge$THORAX, col=c(5:7), pch=c(16:18))
legend("bottomright", title = "Number of partners", legend = c("0", "1", "8"), fill = (5:7))


model1<-lm(partridge$LLONGEV~partridge$PARTNERS+partridge$TYPE+partridge$THORAX)
anova(model1)

model2<-lm(partridge$LLONGEV~partridge$PARTNERS+partridge$TYPE)
anova(model2)


par(mfrow = c(2, 2))
hist(model1$residuals)
qqPlot(model1$residuals)
plot(model1$residuals~model1$fitted.values)
abline(0.15,0)
abline(0,0)
abline(-0.15,0)
plot(cooks.distance(model1) ~ hatvalues(model1))

par(mfrow = c(2, 2))
hist(model2$residuals)
qqPlot(model2$residuals)
plot(model2$residuals~model2$fitted.values)
abline(0.15,0)
abline(0,0)
abline(-0.15,0)
plot(cooks.distance(model2) ~ hatvalues(model2))


#test_scores
scores<-read.table("test_scores.txt", header=TRUE)
str(scores)
summary(scores$MATHS)#gives me the Q1 and Q3 values which allow determination of 50% data range limits
summary(scores$ENGLISH)
scores$diff<-scores$MATHS-scores$ENGLISH #calculates the difference between the scores for each student
hist(scores$diff)
summary(scores$diff)#generates the mean value for the difference between scores
scores$diff2<-scores$ENGLISH-scores$MATHS#just a check for the values if I reverse the order in the subtraction
summary(scores$diff2)
#
mean_E<-mean(scores$ENGLISH)
t_E<-qt(0.975,length(scores$ENGLISH))
se_E<-sd(scores$ENGLISH)/sqrt(length(scores$ENGLISH))
upr95ENGLISH<-mean_E+t_E*se_E
lwr95ENGLISH<-mean_E-t_E*se_E

upr95ENGLISH2<-mean_E+2*se_E
lwr95ENGLISH2<-mean_E-2*se_E

qqnorm(scores$MATHS, plot.it=TRUE, pch=16, cex=3, col=c(4,1,1,1,2,1,1,1,1,1))#produces a normal QQ plot of sample values
qqline(scores$MATHS, distribution=qnorm)#adds a line to the theoretical quantiles
abline(60,0,lty=2)
abline(65,0, lty=2)
abline(70,0,lty=2)
abline(75,0,lty=2)
abline(80,0, lty=2)
abline(85,0,lty=2)
abline(v=0, lty=3)
abline(v=-0.5,lty=3)
abline(v=0.5, lty=3)
abline(v=1.0,lty=3)
abline(v=-1, lty=3)

library(car)
qqPlot(scores$MATHS)
quantile(scores$MATHS, 0.1) # returns the 10% quantile
quantile(scores$MATHS, 0.95)
1-pnorm(95,mean(scores$MATHS), sd(scores$MATHS))
1-pnorm(95,mean(scores$ENGLISH), sd(scores$ENGLISH))



#caterpillars
cat<-read.table("caterpillars.txt", header=TRUE)
str(cat)
par(mfrow = c(2, 2))
hist(cat$north_barley,breaks=12,xlim=c(-50,150),ylim=c(0,12))
hist(cat$north_meadow,breaks=12,xlim=c(-50,150), ylim=c(0,12))
hist(cat$south_barley,breaks=20,xlim=c(-50,150), ylim=c(0,12))
hist(cat$south_meadow,breaks=20,xlim=c(-50,150), ylim=c(0,12))

#question 9 - Luna
1-pnorm(22, 17.5,1.75)
pnorm(22,21,3.5)

#bodyfat - with weight as the response
fat<-read.table("bodyfat.txt", header=TRUE)
str(fat)
fat$SEX<-as.factor(fat$SEX)
str(fat)
clrs<-c("red","green")[factor(fat$SEX)]
plot(fat$WEIGHT~fat$FAT,pch=16,cex=2, col=c("red","green")[factor(fat$SEX)])
legend("topright", title = "sex", legend = c("1", "2"),fill=(2:3))
boxplot(fat$WEIGHT~fat$SEX, col=(2:3))
boxplot(fat$FAT~fat$SEX, col=(2:3))
model_fat<-lm(fat$WEIGHT~fat$FAT*fat$SEX)
anova(model_fat)
summary(model_fat)
par(mfrow = c(2, 2))
hist(model_fat$residuals)
qqPlot(model_fat$residuals)
plot(model_fat$residuals~model_fat$fitted.values)
abline(5,0)
abline(0,0)
abline(-5,0)
plot(cooks.distance(model_fat) ~ hatvalues(model_fat))
par(mfrow = c(1, 1))
plot(fat$WEIGHT~fat$FAT,pch=16,cex=2,col=clrs, xlim=c(22,36),ylim=c(40,100))
abline(a=12.77, b=1.64, h=NULL, v=NULL, col=(2),lwd=3)
abline(a=-44.61, b=4.71,h=NULL, v=NULL, col=(3), lwd=3)
legend("topleft", title = "sex", legend = c("1", "2"),fill=(2:3))
#removing the interaction
model_fat2<-lm(fat$WEIGHT~fat$FAT+fat$SEX)
anova(model_fat2)
summary(model_fat2)
par(mfrow = c(2, 2))
hist(model_fat2$residuals)
qqPlot(model_fat2$residuals)
plot(model_fat2$residuals~model_fat2$fitted.values)
abline(5,0)
abline(0,0)
abline(-5,0)
plot(cooks.distance(model_fat2) ~ hatvalues(model_fat2))
par(mfrow = c(1, 1))
plot(fat$WEIGHT~fat$FAT,pch=16,cex=2,col=clrs, xlim=c(22,36),ylim=c(40,100))
abline(a=-32.92, b=3.31, h=NULL, v=NULL, col=(2),lwd=3)
abline(a=-2.96, b=3.31,h=NULL, v=NULL, col=(3), lwd=3)
legend("topleft", title = "sex", legend = c("1", "2"),fill=(2:3))

par(mfrow = c(1, 2))
plot(fat$WEIGHT~fat$FAT,pch=16,cex=2,col=clrs, xlim=c(22,36),ylim=c(40,100))
abline(a=-32.92, b=3.31, h=NULL, v=NULL, col=(2),lwd=3)
abline(a=-2.96, b=3.31,h=NULL, v=NULL, col=(3), lwd=3)
legend("topleft", title = "sex", legend = c("1", "2"),fill=(2:3))
plot(fat$WEIGHT~fat$FAT,pch=16,cex=2,col=clrs, xlim=c(22,36),ylim=c(40,100))
abline(a=12.77, b=1.64, h=NULL, v=NULL, col=(2),lwd=3)
abline(a=-44.61, b=4.71,h=NULL, v=NULL, col=(3), lwd=3)
legend("topleft", title = "sex", legend = c("1", "2"),fill=(2:3))
boxplot(fat$WEIGHT~fat$SEX, col=(2:3))
boxplot(fat$FAT~fat$SEX, col=(2:3))

#bodyfat - with fat as the response
fat<-read.table("bodyfat.txt", header=TRUE)
str(fat)
fat$SEX<-as.factor(fat$SEX)
str(fat)
clrs<-c(7,8)[factor(fat$SEX)]
par(mfrow = c(1, 3))
plot(fat$FAT~fat$WEIGHT,pch=16,cex=2, col=c(7,8)[factor(fat$SEX)])
legend("topright", title = "sex", legend = c("1", "2"),fill=(7:8))
boxplot(fat$WEIGHT~fat$SEX, col=(7:8))
boxplot(fat$FAT~fat$SEX, col=(7:8))
model_fat1<-lm(fat$FAT~fat$WEIGHT*fat$SEX)
anova(model_fat1)
summary(model_fat1)
par(mfrow = c(2, 2))
hist(model_fat1$residuals)
qqPlot(model_fat1$residuals)
plot(model_fat1$residuals~model_fat1$fitted.values)
abline(1,0)
abline(0,0)
abline(-1,0)
plot(cooks.distance(model_fat1) ~ hatvalues(model_fat1))
par(mfrow = c(1, 1))
plot(fat$FAT~fat$WEIGHT,pch=16,cex=2,col=clrs, xlim=c(50,100),ylim=c(20,40))
abline(a=5.24, b=0.40, h=NULL, v=NULL, col=(7),lwd=3)
abline(a=11.56, b=0.18,h=NULL, v=NULL, col=(8), lwd=3)
legend("topleft", title = "sex", legend = c("1", "2"),fill=(7:8))
#removing the interaction
model_fat2<-lm(fat$FAT~fat$WEIGHT+fat$SEX)
anova(model_fat2)
summary(model_fat2)
par(mfrow = c(2, 2))
hist(model_fat2$residuals)
qqPlot(model_fat2$residuals)
plot(model_fat2$residuals~model_fat2$fitted.values)
abline(2,0)
abline(0,0)
abline(-2,0)
plot(cooks.distance(model_fat2) ~ hatvalues(model_fat2))
par(mfrow = c(1, 1))
plot(fat$FAT~fat$WEIGHT,pch=16,cex=2,col=clrs, xlim=c(50,100),ylim=c(20,40))
abline(a=16.96, b=0.22, h=NULL, v=NULL, col=(7),lwd=3)
abline(a=9.06, b=0.22, h=NULL, v=NULL, col=(8), lwd=3)
legend("topleft", title = "sex", legend = c("1", "2"),fill=(7:8))

par(mfrow = c(1, 2))
plot(fat$FAT~fat$WEIGHT,pch=16,cex=2,col=clrs, xlim=c(50,100),ylim=c(20,40))
abline(a=5.24, b=0.40, h=NULL, v=NULL, col=(7),lwd=3)
abline(a=11.56, b=0.18,h=NULL, v=NULL, col=(8), lwd=3)
legend("topleft", title = "sex", legend = c("1", "2"),fill=(7:8))
plot(fat$FAT~fat$WEIGHT,pch=16,cex=2,col=clrs, xlim=c(50,100),ylim=c(20,40))
abline(a=16.96, b=0.22, h=NULL, v=NULL, col=(7),lwd=3)
abline(a=9.06, b=0.22, h=NULL, v=NULL, col=(8), lwd=3)
legend("topleft", title = "sex", legend = c("1", "2"),fill=(7:8))

#plantlets
plantlets<-read.table("plantlets.txt", header=TRUE)
str(plantlets)
plantlets$SPECIES<-as.factor(plantlets$SPECIES)
pairs(plantlets[,1:3])
boxplot(plantlets$HGHT~plantlets$SPECIES)
model_plantlets<-lm(plantlets$HGHT~plantlets$LONGEV+plantlets$LEAFSIZE+
                      plantlets$SPECIES+plantlets$LONGEV*plantlets$LEAFSIZE+plantlets$LONGEV*plantlets$SPECIES+
                      plantlets$LEAFSIZE*plantlets$SPECIES)
anova(model_plantlets)
model_plantlets2<-lm(plantlets$HGHT~plantlets$LONGEV+plantlets$LEAFSIZE+
                      plantlets$SPECIES+plantlets$LONGEV*plantlets$LEAFSIZE+plantlets$LONGEV*plantlets$SPECIES)
anova(model_plantlets2)

model_plantlets3<-lm(plantlets$HGHT~plantlets$LONGEV+plantlets$LEAFSIZE+
                       plantlets$SPECIES+plantlets$LONGEV*plantlets$SPECIES)
anova(model_plantlets3)

model_plantlets4<-lm(plantlets$HGHT~plantlets$LONGEV+
                       plantlets$SPECIES+plantlets$LONGEV*plantlets$SPECIES)
anova(model_plantlets4)
summary(model_plantlets4)

#KH
kh<-read.table("KH.txt", header=TRUE)
str(kh)
kh$cond<-as.factor(kh$cond)
kh$cond<-relevel(kh$cond, ref="Placebo")
kh$rat<-as.factor(kh$rat)
col=(kh$cond)
boxplot(kh$values~kh$cond)
par(mfrow = c(1, 2))
plot(kh$values~kh$time, pch=16, col=c(9,10)[factor(kh$cond)])
legend("topright", title = "treatment", legend = c("Placebo", "Pinacidil"),fill=(9:10))
plot(kh$values~kh$time, pch=16, col=c(1:7)[factor(kh$rat)])
legend("topright", title = "rat", legend = c("1", "2", "3", "4", "5", "6", "7"),fill=(1:7))
model_kh<-lm(kh$values~kh$rat+kh$cond+kh$time+kh$rat*kh$cond)
anova(model_kh)
summary(model_kh)
#without rat in the model
model_kh0<-lm(kh$values~kh$cond+kh$time)
anova(model_kh0)
summary(model_kh0)
par(mfrow = c(1, 1))
#creates a plot with one categorical predictor on the x-axis, a second categorical predictor overlaid with different colours and the response variable on the y-axis
interaction.plot(x.factor     = kh$rat,
                 trace.factor = kh$cond,
                 response     = kh$values,
                 fun = mean,
                 type="b",
                 col=c(1:2),#Colors for levels of trace var.
                 pch=c(16:17), #Symbols for levels of trace var.
                 fixed=TRUE, #Order by factor order in data
                 leg.bty = "o")
plot(kh$values~kh$time)
