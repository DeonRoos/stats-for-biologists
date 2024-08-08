
# FROGS DATASET ----------------------------------------------------------------

frogs<-read.table("frogs.txt", header=T)

boxplot(CALLS~YEAR, data=frogs,
        ylim=c(-20,20))
boxplot(CALLS~BLOCK_1, data=frogs,
        ylim=c(-20,20))
# plot(CALLS~BLOCK_2, data = frogs, col=YEAR, pch=20, cex=1.5, type="b", lty=2) # doesn't seem to make the right lines
library(lattice)
xyplot(CALLS~BLOCK_2, data = frogs, group=YEAR, pch=20, cex=1.5,type="b", lty=2)

model1<-aov(frogs$CALLS~frogs$YEAR)
summary(model1) # Results are not the same as in the lecture slides. Not sure why?

model2<-aov(frogs$CALLS~frogs$YEAR+frogs$BLOCK_1)
summary(model2) # Results are not the same as in the lecture slides. Not sure why?


# MITES DATASET ----------------------------------------------------------------

mites<-read.table("mites.txt", header=T)

boxplot(LMITE~TREAT, data=mites,
        ylim=c(-2,6))
boxplot(LMITE~BLOCK, data=mites,
        ylim=c(-2,6))

model3<-aov(mites$LMITE~mites$TREAT+mites$BLOCK)
summary(model3)

interaction.plot(x.factor     = mites$TREAT,
                 trace.factor = mites$BLOCK,
                 response     = mites$LMITE,
                 fun = mean,
                 type="b",
                 col=c(1:14),#Colors for levels of trace var.
                 pch=c(1:14), #Symbols for levels of trace var.
                 fixed=TRUE, #Order by factor order in data
                 leg.bty = "o")

# DUCKS DATASET ----------------------------------------------------------------

ducks<-read.table("ducks.txt", header=T)

ducks$ln.obs.count<-log(ducks$obs.count)
ducks[ducks$ln.obs.count==-Inf,]$ln.obs.count<-NA

sub.ducks<-ducks[ducks$size=="Medium",]

library(gplots)
plotmeans(ln.obs.count ~ time, data = sub.ducks, connect = FALSE, n.label = FALSE)
plotmeans(ln.obs.count ~ treat, data = sub.ducks, connect = FALSE, n.label = FALSE)
plotmeans(ln.obs.count ~ interaction(treat, time), data = sub.ducks, connect = FALSE, n.label = FALSE)


model4<-aov(sub.ducks$ln.obs.count~sub.ducks$time*sub.ducks$treat)
summary(model4)

model4$coefficients

treat<-c("with eiders","with eiders", "without eiders", "without eiders")
time<-c("t0", "t60","t0", "t60")
model4.mean<-as.data.frame(cbind(treat, time))
model4.mean$ln.obs<-NA
model4.mean$ln.obs[1]<-model4$coefficients[1]
model4.mean$ln.obs[2]<-model4$coefficients[1]+model4$coefficients[2]
model4.mean$ln.obs[3]<-model4$coefficients[1]+model4$coefficients[3]
model4.mean$ln.obs[4]<-model4$coefficients[1]+model4$coefficients[2]+model4$coefficients[3]+model4$coefficients[4]

barplot.matrix<-reshape(model4.mean,idvar = "time",
        timevar = "treat",
        direction = "wide")
row.names(barplot.matrix) <- barplot.matrix$time
barplot.matrix <- barplot.matrix[ , 2:ncol(barplot.matrix)]
colnames(barplot.matrix) <- c("with eiders", "without eiders")
barplot.matrix <- as.matrix(barplot.matrix)

barplot(height=barplot.matrix,beside = TRUE, legend = c("t0","t60"),args.legend=list(title="time"),
        xlab = "treat", ylab= "Mean of LN obs count")
abline(h=model4.mean$ln.obs,col="red",  lty=2)

barplot.matrix.exp<-exp(barplot.matrix)
barplot(height=barplot.matrix.exp,beside = TRUE, legend = c("t0","t60"), xlab = "treat",args.legend=list(title="time"),
        ylab= "Mean number per plot")
abline(h=exp(model4.mean$ln.obs),col="red",  lty=2)

#rats dataset
mice<-read.table("rats_diets2.txt", header=TRUE)
str(mice)
boxplot(mice$Rand_weight~mice$Rand_trt)
col=(mice$Rand_trt)
plot(mice$Rand_weight~mice$i.weight, col=c(1:2), pch=16)
boxplot(mice$i.weight~mice$Rand_trt)
model1<-lm(mice$Rand_weight~mice$Rand_trt)
anova(model1)
summary(model1)

hist(model1$residuals)
qqnorm(model1$residuals)
qqline(model1$residuals)
plot(model1$residuals ~ model1$fitted.values)
abline(20,0)
abline(0,0)
abline(-20,0)
plot(cooks.distance(model1) ~ hatvalues(model1))

model2<-lm(mice$Rand_weight~mice$i.weight+mice$Rand_trt)
anova(model2)
summary(model2)

hist(model2$residuals)
qqnorm(model2$residuals)
qqline(model2$residuals)
plot(model2$residuals ~ model2$fitted.values)
abline(20,0)
abline(0,0)
abline(-20,0)
plot(cooks.distance(model2) ~ hatvalues(model2))

#second experiment
str(mice)
boxplot(mice$BL_weight~mice$BL_trt)
col=(mice$BL_trt)
plot(mice$BL_weight~mice$i.weight, col=c(1:2), pch=16)
boxplot(mice$i.weight~mice$BL_trt)

model3<-lm(mice$Rand_weight~mice$i.weight+mice$Rand_trt)
anova(model3)
summary(model3)
hist(model3$residuals)
qqnorm(model3$residuals)
qqline(model3$residuals)
plot(model3$residuals ~ model3$fitted.values)
abline(20,0)
abline(0,0)
abline(-20,0)
plot(cooks.distance(model3) ~ hatvalues(model3))

#biostatch9.txt
seedlings<-read.table("seedling.txt", header=TRUE)
str(seedlings)
hist(seedlings$SeedlSum, breaks =10)
boxplot(seedlings$SeedlSum~seedlings$Treatment)
boxplot(seedlings$SeedlSum~seedlings$Block)
interaction.plot(x.factor     = seedlings$Treatment,
                 trace.factor = seedlings$Block,
                 response     = seedlings$SeedlSum,
                 fun = mean,
                 type="b",
                 col=c(1:14),#Colors for levels of trace var.
                 pch=c(1:14), #Symbols for levels of trace var.
                 fixed=TRUE, #Order by factor order in data
                 leg.bty = "o")
model_seedlings<-lm(seedlings$SeedlSum~seedlings$Treatment+seedlings$Block)
anova(model_seedlings)
summary(model_seedlings)
hist(model_seedlings$residuals)
qqnorm(model_seedlings$residuals)
qqline(model_seedlings$residuals)
plot(model_seedlings$residuals ~ model_seedlings$fitted.values)
abline(20,0)
abline(0,0)
abline(-20,0)
library(car)#remember you can now create a QQ plot with a 95% confidence interval
par(mfrow = c(1,1))
qqPlot(model_seedlings$residuals) 
plot(cooks.distance(model_seedlings) ~ hatvalues(model_seedlings))

#run without the block effect
model_seedlings<-lm(seedlings$SeedlSum~seedlings$Treatment)
anova(model_seedlings)
summary(model_seedlings)
hist(model_seedlings$residuals)
qqnorm(model_seedlings$residuals)
qqline(model_seedlings$residuals)
plot(model_seedlings$residuals ~ model_seedlings$fitted.values)
abline(20,0)
abline(0,0)
abline(-20,0)
library(car)#remember you can now create a QQ plot with a 95% confidence interval
par(mfrow = c(1,1))
qqPlot(model_seedlings$residuals) 
plot(cooks.distance(model_seedlings) ~ hatvalues(model_seedlings))
