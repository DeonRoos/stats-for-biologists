#part 3 workshop 10
#this dataset has 5 development categories, one is mislabeled
eggs<-read.table("Egg_mass_again.txt", header = TRUE)
# this dataset has s, sa and sp included as altricial
eggs2<-read.table("Egg_mass_again_correct.txt", header=TRUE) 
#this dataset has s and sa included as altricial, sp included as precocial
eggs3<-read.table("Egg_mass_again_correct2.txt", header=TRUE) 

#look at sample size for each development category
table(eggs$Development_category)
table(eggs2$Development_category)
table(eggs3$Development_category)

#explore the data, working with dataset 3
str(eggs3)
#change development category to factor
eggs3$Development_category<-as.factor(eggs3$Development_category)
head(eggs3)
table(eggs3$Development_category)
hist(eggs3$egg_mass)
hist(eggs3$Log_egg_mass)

plot(eggs3$egg_mass~eggs$female_mass)
col=("eggs3$Development_category")
plot(eggs3$Log_egg_mass~eggs3$Log_female_mass, col=c(1:2), pch=16)

#run the model with an interaction term which would allow for two slopes as in the figure
model_eggs3<-lm(eggs3$Log_egg_mass~eggs3$Log_female_mass*eggs3$Development_category)
anova(model_eggs3)
summary(model_eggs3)

#subset the eggs3 dataframe to allow us to run separate models
eggs3_a<-subset(eggs3, eggs3$Development_category=="a")
eggs3_p<-subset(eggs3, eggs3$Development_category=="p")

#first model for altricial species
model_eggs3a<-lm(eggs3_a$Log_egg_mass~eggs3_a$Log_female_mass)
anova(model_eggs3a)
summary(model_eggs3a)

#second model for precocial species
model_eggs3p<-lm(eggs3_p$Log_egg_mass~eggs3_p$Log_female_mass)
anova(model_eggs3p)
summary(model_eggs3p)
