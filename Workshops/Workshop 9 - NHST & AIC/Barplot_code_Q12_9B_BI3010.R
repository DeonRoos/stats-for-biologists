# Housekeeping
rm(list = ls())
par(mfrow = c(1,1))

# Set working directory
setwd("~/Documents/Demonstrating_Resources/BI3010/Week_4/")
getwd()

Ducks<-read.table("eiders.txt", header=TRUE)
str(Ducks)

Med <- subset(Ducks, size == "Medium")
str(Med)

Med$ln.obs.count=log(Med$obs.count) #this creates a new variable for count that is log-transformed (natural log)
hist(Med$ln.obs.count)

Model_med<- lm(Med$ln.obs.count~Med$time * Med$treat)
summary(Model_med)

library(ggplot2)

#========================================
# Barplot / interval plot for Q12 
#========================================
# 1. Creating mean and standard error dataframe
#For our new dataset, we need to define some variables (or vectors if you prefer):
time <- c("t0","t60", "t0","t60")
treat <- c("without.eiders", "without.eiders", "with.eiders","with.eiders")
#We then need to calculate means and standard errors, and also put these into vectors.
#We can find the means for each combination of treatment levels using:
ln.obs.count <- as.vector(by(Med$ln.obs.count, list(Med$time, Med$treat), mean))

#We then do a similar calculation to find the standard deviation (the first step in calculating standard error).
stde <- as.vector(by(Med$ln.obs.count, list(Med$time,Med$treat), sd))

#We then calculate standard error (each treatment combination contains 10 guinea pigs)
sterr <- stde/sqrt(10)
#We can combine our four vectors into a dataframe.
df <- data.frame(time, treat, ln.obs.count, sterr)

#To make our lives easier going forward, we will change our two character vectors into factors.
df$time <- as.factor(df$time)
df$treat <- as.factor(df$treat)
#We are now ready to make a barchart!!!!


# 2. Barplot
ggplot(df, aes(time, ln.obs.count, fill = treat)) + 
  geom_col(position=position_dodge()) + 
  geom_errorbar(aes(ymin=ln.obs.count-sterr, ymax=ln.obs.count+sterr),position=position_dodge() )

# 3. Interval plot
ggplot() + 
  geom_errorbar(data=df, mapping=aes(x=time, ymin=ln.obs.count-sterr, ymax=ln.obs.count+sterr), width=0.3, size=0.5, color="black", 
                position = position_dodge(width = 0.8)) + 
  geom_point(data=df, mapping=aes(x=time, y=ln.obs.count, col = treat), size=1, fill="white")

