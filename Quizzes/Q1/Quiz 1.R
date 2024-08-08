


# CHECK ANSWER FEEDBACK!!!




# Question 3 -------------------------------------------------------------------

grades<-c(12, rep(14,4), rep(15,5), rep(16,3), rep(17,6), rep(18,8), rep(19,3),
          20,21)

hist(grades, breaks=seq(-0.5,22.5), xlim=c(0,22), xlab="Grade Points", main=NULL)
axis(side=1, at=seq(0,22), labels=seq(0,22))

length(grades)

# Question 7 -------------------------------------------------------------------
survey<-read.csv("BI3010Survey.csv")

probs<-c(0.025, 0.05, 0.10, 0.16, 0.30, 0.50, 0.70, 0.86, 0.90, 0.95, 0.975)
quantile(survey[survey$Year==2020,]$DreamPartner, probs = probs)

qqnorm(survey[survey$Year==2019,]$DreamPartner,
       ylim=c(100,220),
       main="2019")
qqline(survey[survey$Year==2019,]$DreamPartner)
axis(2, at=seq(100,220,10))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
abline(h=seq(100,220,10), col = "lightgray", lty = "dotted")

qqnorm(survey[survey$Year==2020,]$DreamPartner,
       ylim=c(100,220),
       main="2020")
qqline(survey[survey$Year==2020,]$DreamPartner)
axis(2, at=seq(100,220,10))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
abline(h=seq(100,220,10), col = "lightgray", lty = "dotted")

qqnorm(survey[survey$Year==2021,]$DreamPartner,
       ylim=c(100,220),
       main=2021)
qqline(survey[survey$Year==2021,]$DreamPartner)
axis(2, at=seq(100,220,10))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
abline(h=seq(100,220,10), col = "lightgray", lty = "dotted")


# like this or the same as in the quiz? YES + CHANGE QUESTION - ASK MICHELLE

# Question 16 ------------------------------------------------------------------
library(gplots)
plotmeans(Height ~ interaction(Diet1.HiSug, DietChild2), data = survey[survey$Year==2019,], 
          connect = FALSE, n.label = FALSE, main="mean +/- 95%CI", cex.axis=0.75,
          xaxt = "n", xlab="Interaction between sugar diet and calories") 
grid(NA, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
axis(1, at=1:6, padj = 0.25, labels=c("Low sugar\nHigh calories",
                                      "High sugar\nHigh calories",
                                      "Low sugar\nLow calories",
                                      "High sugar\nLow calories",
                                      "Low sugar\nMedium calories",
                                      "High sugar\nMedium calories"))


penguins2<-read.table("penguins_A_C.txt",header=TRUE)
#trying to make a plot of mean values with 95%CI for Adelie and Chinstrap, male and female
plotmeans(flipper_length_mm ~ interaction(species, sex), data = penguins2, 
          connect = FALSE, n.label = FALSE, main="mean +/- 95%CI", cex.axis=0.75,
          xaxt = "n", xlab="Interaction between species and sex") 
grid(NA, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
axis(1, at=1:4, padj = 0.25, labels=c("Adelie\nFemale",
                                      "Chinstrap\nFemale",
                                      "Adelie\nMale",
                                      "Chinstrap\nMale"))



# Questions 20 and 21 ----------------------------------------------------------
prawn<-read.csv("prawngrowth.csv")

model1<-aov(prawn$GrowthR~prawn$Diet)
summary(model1)

0.951/(31.377+0.951)*100

# Questions 22-26 --------------------------------------------------------------

mussels<-read.csv("lengths_mussels.csv")

# Answers for question 22
mean(mussels[mussels$temperature=="LEN5D",]$length) # same as in quiz
sd(mussels[mussels$temperature=="LEN5D",]$length) # same as in quiz
mean(mussels[mussels$temperature=="LEN15D",]$length) # same as in quiz
sd(mussels[mussels$temperature=="LEN15D",]$length) # same as in quiz

# Answers for question 23
quantile(mussels[mussels$temperature=="LEN5D",]$length, probs=c(0.25,0.75)) # same as in quiz
quantile(mussels[mussels$temperature=="LEN15D",]$length, probs=c(0.25,0.75)) # DIFFERENT from quiz (214.25 instead of 215)
# Does Michelle want to change the answer? CHANGE TO 214 - DONE

# Answers for question 24
quantile(mussels[mussels$temperature=="LEN5D",]$length, probs=c(0.9)) # DIFFERENT from quiz (157.2 instead of 158)
# Does Michelle want to change the answer? CHANGE TO 157 - DONE
quantile(mussels[mussels$temperature=="LEN15D",]$length, probs=c(0.1)) # same as in quiz
quantile(mussels[mussels$temperature=="LEN5D",]$length, probs=c(0.5)) # same as in quiz

# Answers for question 25
1-pnorm(185, mean(mussels[mussels$temperature=="LEN5D",]$length), sd(mussels[mussels$temperature=="LEN5D",]$length)) # same as in quiz
pnorm(185, mean(mussels[mussels$temperature=="LEN15D",]$length), sd(mussels[mussels$temperature=="LEN15D",]$length)) # same as in quiz

# Answers for question 26
(res<-t.test(length ~ temperature, data=mussels, 
             alternative = "two.sided", var.equal = FALSE)) # Blank, 1,2 and 4 same as in quiz
res$estimate[1]-res$estimate[2] # Blank 3 same as in quiz
