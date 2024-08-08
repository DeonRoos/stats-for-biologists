data("iris")
par(mfrow=c(2,2))#2 vertical panels, two horizontal panels
par(mfrow=c(1,1))#one single plotting panel
par(mfrow=c(1,2))
hist(iris$Petal.Length)
hist(iris$Petal.Width)

#task one - make a plot with two panels comparing histograms with same x scale
par(mfrow=c(1,2))
hist(iris$Petal.Length,xlim=c(0,8))
hist(iris$Petal.Width, xlim=c(0,8))

#task two - fit a linear model of petal width against petal length and species, 
#display four assumption checks in a 2x2 arrangement
model_iris<-lm(iris$Petal.Width~iris$Petal.Length+iris$Species)
#checking assumptions
par(mfrow=c(2,2))#2 vertical panels, two horizontal panels
hist(model_iris$residuals, breaks = 20)
qqnorm(model_iris$residuals)
qqline(model_iris$residuals)
plot(model_iris$residuals ~ model_iris$fitted.values)
abline(0.2,0)
abline(0,0)
abline(-0.2,0)
plot(cooks.distance(model_iris) ~ hatvalues(model_iris))

#making a scatterplot in ggplot2
library(ggplot2)
ggplot(iris, aes(Sepal.Width,Petal.Width))#basic axes
ggplot(iris, aes(Sepal.Width,Petal.Width))+geom_point() #adds datapoints
ggplot(iris, aes(Sepal.Width,Petal.Width))+geom_point()+labs(y="Petal Width (cm)",
                                                             x="Sepal Width (cm)")
ggplot(iris, aes(Sepal.Width,Petal.Width, colour=Species))+
  geom_point()+labs(y="Petal Width (cm)",x="Sepal Width (cm)")#change of colour, adds legend

ggplot(iris, aes(Sepal.Width,Petal.Width, colour=Species))+
  geom_point()+labs(y="Petal Width (cm)",x="Sepal Width (cm)")+
  scale_colour_manual(values=c("black","pink","red"))#adds specific colours

ggplot(iris, aes(Sepal.Width,Petal.Width,colour=Petal.Length))+
  geom_point(size=4)+labs(y="Petal Width (cm)",x="Sepal Width (cm)")
 
ggplot(iris, aes(Sepal.Width,Petal.Width,colour=Petal.Length))+
  geom_point(size=4)+labs(y="Petal Width (cm)",x="Sepal Width (cm)")+
  scale_colour_gradientn(colours=rainbow(4))

#Task Three
ggplot(iris, aes(Sepal.Width,Petal.Width,colour=Petal.Length))+
  geom_point(size=4)+labs(y="Petal Width (cm)",x="Sepal Width (cm)")+
  scale_colour_gradientn(colours=rainbow(6))
#when you change the number of rainbow colours you end up with that number of distinct hues
ggplot(iris, aes(Sepal.Width,Petal.Width,colour=Petal.Length))+
  geom_point(size=4)+labs(y="Petal Width (cm)",x="Sepal Width (cm)")+
  scale_colour_gradientn(colours=rainbow(1))
#when you set it to one, you get a single colour
ggplot(iris, aes(Sepal.Width,Petal.Width,colour=Petal.Length))+
  geom_point(size=4)+labs(y="Petal Width (cm)",x="Sepal Width (cm)")+
  scale_colour_gradientn(colours=rainbow(20))
#when you set it to 20, you still only get the number of integers in the variable of interest

#to manually set the colour gradient
ggplot(iris, aes(Sepal.Width,Petal.Width,colour=Petal.Length))+
  geom_point(size=4)+labs(y="Petal Width (cm)",x="Sepal Width (cm)")+
  scale_colour_gradient(low="yellow", high="red",na.value=NA)

#Task Four
#Task four: load the finch egg dataset. 
#Make a scatter plot of egg mass against female mass, 
#with colour proportional to clutch size.
# Read data in 
eggs<-read.table("FinchEggs.txt", header=TRUE)
str(eggs)

ggplot(eggs, aes(F_mass, Egg_mass, colour = Clutch_size)) +
  geom_point() +
  scale_colour_gradient(low = "yellow", high = "red")
eggs$Ln.egg_mass<-log(eggs$Egg_mass)
eggs$Ln.F_mass<-log(eggs$F_mass)

ggplot(eggs, aes(Ln.F_mass, Ln.egg_mass, colour = Clutch_size)) +
  geom_point() +
  scale_colour_gradient(low = "yellow", high = "red")


# Grey values = missing / NA values
# Fix with: 
# scale_colour_gradient(low = "yellow", high = "red", na.value = NA) 

# using Facet wrap
#NOTE: par does not work for ggplot2. There are ways of doing this but they involve additional packages and they are overkill for a simple introduction to gglpot2. However we can split our scatterplot by a categorical predictor by adding another layer called facet_wrap() to our command:
ggplot(iris, aes(Sepal.Width, Petal.Width,colour = Petal.Length)) + 
  geom_point(size = 4) + 
  labs(y = "Petal width (cm)", x = "Sepal width (cm)") + 
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) + 
  theme_classic() + 
  facet_wrap( ~ Species, nrow = 1)

#=========================
# Boxplots in ggplot 
str(eggs)
geom_boxplot(eggs, aes(M_mass, Mating_System,colour=Mating_System))+theme_bw



#=========================
data("ToothGrowth")
TG <- ToothGrowth # shortens the object name
#For our new dataset, we need to define some variables (or vectors if you prefer):
Supp <- c("VC","OJ", "VC","OJ","VC","OJ")
dose <- c(0.5,0.5,1,1,2,2)
#We then need to calculate means and standard errors, and also put these into vectors.
#We can find the means for each combination of treatment levels using:
by(TG$len, list(TG$supp,TG$dose), mean)
#We then need to add these values into a “vector”
len <- c(13.23, 7.98, 22.7, 16.77, 26.06, 26.14)
#We then do a similar calculation to find the standard deviation (the first step in calculating standard error).
by(TG$len, list(TG$supp,TG$dose), sd)
#Again, we put these into a vector:
stde <- c(4.46, 2.75,3.91,2.52,2.66,4.80)
#We then calculate standard error (each treatment combination contains 10 guinea pigs)
sterr <- stde/sqrt(10)
#We can combine our four vectors into a dataframe.
df <- data.frame(Supp, dose, len, sterr)
#To make our lives easier going forward, we will change our two character vectors into factors.
df$dose <- as.factor(df$dose)
df$Supp <- as.factor(df$Supp)
#We are now ready to make a barchart!!!!
#To make the default chart:
ggplot(df, aes(Supp, len, fill = dose)) + geom_col()
#As you can see, it has produced a stacked chart with no error bars. To unstack:
ggplot(df, aes(Supp, len, fill = dose)) + geom_col(position=position_dodge())
#And then to add error bars:
ggplot(df, aes(dose, len, fill = Supp)) + 
  geom_col(position=position_dodge()) + 
  geom_errorbar(aes(ymin=len-sterr, ymax=len+sterr),position=position_dodge() )

# Interval plot of bar chart ... with standard errors
str(df)
ggplot() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "slategrey") + 
  geom_errorbar(data=df, mapping=aes(x=dose, ymin=len-sterr, ymax=len+sterr), width=0.3, size=0.5, color="black", 
                position = position_dodge(width = 0.8)) + 
  geom_point(data=df, mapping=aes(x=dose, y=len, col = Supp), size=1, fill="white")


# Interval plot of bar chart ... with CIs  
ggplot() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "slategrey") + 
  geom_errorbar(data=df, mapping=aes(x=dose, ymin=len-(1.96*sterr), ymax=len+(1.96*sterr)), width=0.3, size=0.5, color="black") +
  geom_point(data=df, mapping=aes(x=dose, y=len, col = Supp), size=1, fill="white")




plot(iris$Petal.Length~iris$Petal.Width)
