
# Question 1 -------------------------------------------------------------------

# Question 2

xa<-seq(10,90,10)
ya<-seq(0,80,10)

xb<-seq(0,90,10)
yb<-rep(10,10)

xc<-seq(10,90,10)
yc<-seq(10,90,10)

xd<-seq(0,80,10)
yd<-seq(10,90,10)

par(mfrow=c(2,2))
plot(xa,ya, type="l", main="Graph A", xlim=c(0,90), ylim=c(0,90), xlab="x", ylab="y")
axis(side = 1, at = xa,labels = T)
axis(side = 2, at = ya,labels = T)
plot(xb,yb, type="l", main="Graph B", xlim=c(0,90), ylim=c(0,90), xlab="x", ylab="y")
axis(side = 1, at = xb,labels = T)
axis(side = 2, at = ya,labels = T)
plot(xc,yc, type="l", main="Graph C", xlim=c(10,90), ylim=c(0,90), xlab="x", ylab="y")
axis(side = 1, at = xc,labels = T)
axis(side = 2, at = yc,labels = T)
plot(xd,yd, type="l", main="Graph D", xlim=c(0,90), ylim=c(0,90), xlab="x", ylab="y")
axis(side = 1, at = xd,labels = T)
axis(side = 2, at = yd,labels = T)
#Question 3

# Question 4 
#Question 5 
cuckoo<-read.table("cuckoodata.txt", header=TRUE)
str(cuckoo)
hist(cuckoo$LayDate)
plot(cuckoo$LayDate~cuckoo$Lat)
boxplot(cuckoo$LayDate~cuckoo$Migr)
col=(cuckoo$Migr)
plot(cuckoo$LayDate~cuckoo$Lat, col=c(1:3))
model_cuckoo<-lm(cuckoo$LayDate~cuckoo$Lat+cuckoo$Migr)
hist(model_cuckoo$residuals)
qqnorm(model_cuckoo$residuals)
qqline(model_cuckoo$residuals)
plot(model_cuckoo$residuals ~ model_cuckoo$fitted.values)
abline(10,0)
abline(0,0)
abline(-10,0)
plot(cooks.distance(model_cuckoo) ~ hatvalues(model_cuckoo))
anova(model_cuckoo)
summary(model_cuckoo)

summary(cuckoo$LayDate)
p_220<-(1-pnorm(220,mean(cuckoo$LayDate),sd(cuckoo$LayDate)))
quantile(cuckoo$LayDate, probs=c(0.2))







# Question 6
# Question 7
# Question 8
# Question 9
# Question 10
# Question 11
# Question 12
harrier<-read.table("harrier.txt", header=TRUE)
str(harrier)
plot(harrier$tarsus~harrier$weight)
boxplot(harrier$weight~harrier$EarlyLate)
model_harrier<-lm(harrier$logWeight~harrier$tarsus+harrier$EarlyLate)
summary(model_harrier)
anova(model_harrier)
-------------------------------------------------------------------
survey<-read.table("BI3010Survey2021.txt")
survey<-survey[survey$Year==2019,]
model1<-aov(survey$DreamPartner~survey$Height+survey$Gender+survey$ShoeSize)
summary(model1) # DIFFERENT from quiz

# Question 5 -------------------------------------------------------------------
survey<-read.csv("BI3010Survey.csv")
survey<-survey[survey$Year==2021,]
model2<-aov(survey$DreamPartner~survey$Height+survey$Upbringing)
model2$coefficients # DIFFERENT from quiz (not sure which year or subset was used)


# Question 6 -------------------------------------------------------------------
survey<-read.csv("BI3010Survey2021_v1.csv")
#survey<-survey[survey$Gender!="NotSaying",]
model3<-glm(survey$ShoeSize~survey$Height)
summary(model3) # DIFFERENT from quiz (0.18 instead of 0.12)

model4<-glm(survey$ShoeSize~survey$Height+survey$Gender)
summary(model4) # DIFFERENT from quiz (1.77 instead of 1.91)

# TO DO: blanks 4 and 5


# Question 7 -------------------------------------------------------------------


# Question 8 -------------------------------------------------------------------



# Question 9 -------------------------------------------------------------------



# Question 10 ------------------------------------------------------------------



# Question 11 ------------------------------------------------------------------
# Same as Question 4


# Question 12 ------------------------------------------------------------------
survey<-survey[survey$Year==2019 & survey$Gender=="Female",]
model5<-glm(survey$DreamPartner~survey$Height+survey$DietType)
summary(model5) # DIFFERENT from quiz

# Question 13 ------------------------------------------------------------------



# Question 14 ------------------------------------------------------------------



# Question 15 ------------------------------------------------------------------