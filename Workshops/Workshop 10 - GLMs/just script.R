lions<-read.table("Lions.txt", header=TRUE)
str(lions)
plot(lions$Black~lions$age_table1)
plot(lions$arcsine_black~lions$age_table1)
plot(lions$predicted~lions$age_table1)

plot(lions$arcsine_black~lions$age_table1)
model<-lm(lions$arcsine_black~lions$age_table1)
abline(model)

plot(lions$Black~lions$age_table1)
model<-lm(lions$Black~lions$age_table1)
abline(model)
age_0.5<-2.0667*5.9037*(asin(sqrt(0.5)))
proportion_5y<-sin((5-2.0667)/5.9037)^2


age_0.5<-2.0667*5.9037*(asin(sqrt(0.5)))
proportion_5y<-sin((5-2.0667)/5.9037)^2
lions<-read.table("Lions.txt", header=TRUE)
str(lions)
plot(lions$Black~lions$age_table1)
plot(lions$arcsine_black~lions$age_table1)
plot(lions$predicted~lions$age_table1)

plot(lions$arcsine_black~lions$age_table1)
model<-lm(lions$arcsine_black~lions$age_table1)
abline(model)

plot(lions$Black~lions$age_table1)
model<-lm(lions$Black~lions$age_table1)
abline(model)
