# Housekeeping
rm(list = ls())
par(mfrow = c(1,1))

# Set working directory
setwd("~/Documents/Demonstrating_Resources/BI3010/Week_4/")
getwd()


# ==================================
# Plotting in baseR
# ==================================
# Change plotting settings
par(mfrow = c(2, 2))

# Load Iris data a see how it all plots out 
library(datasets)
data(iris)
summary(iris)

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)

#Task one: make a plot with just two panels comparing a histogram of petal length against petal width, 
#with both graphs having the same x scale.
  # Set plotting panels
    par(mfrow = c(2, 1))

  # Plot out histograms and see what default X axis setting are. 
    hist(iris$Petal.Length)
    hist(iris$Petal.Width)
    
  # Standardise X lims to match - 0-7
    hist(iris$Petal.Length, xlim = c(0, 7))
    hist(iris$Petal.Width, xlim = c(0, 7))
  
  # Now need breaks to match as well. Easiest to standardise one ot match the other  
  # Create breaks arguement for the Petal length, that matches Width axis
    breaks_PL <- seq(1, 7, by = 0.2)

  # Plot out final plots again
    hist(iris$Petal.Length, breaks = breaks_PL, xlim = c(0, 7), main = "Histogram of Petal Length", xlab = "Petal Length")
    hist(iris$Petal.Width, xlim = c(0, 7), main = "Histogram of Petal Width", xlab = "Petal Width")

#Task two: fit a linear model of petal width against petal length and species. 
  #Display the four assumptions checks in a 2 x 2 arrangement.
  # Check structure of dataframe - ensure factors = factors and covariates - numerics!
    str(iris)

  # Run simple lm / regression 
    model <- lm(iris$Petal.Width ~ iris$Petal.Length)
    
  # Check model summary and anova (just good practice!)
    summary(model)
    anova(model)

  # Set up 2 x 2 plotting window
    par(mfrow = c(2,2))
  
  # plot out model diagnostic plots in turn.. 
    # Histogram of residuals
    hist(model$residuals, breaks = 20)
    
    # Normal Q-Q plot
    qqnorm(model$residuals)
    qqline(model$residuals)
    
    # Fitted values vs. residuals
    plot(model$residuals ~ model$fitted.values, pch=16)
    abline(0.4,0)
    abline(0,0)
    abline(-0.4,0)
    
    # Cooks distance plot
    plot(cooks.distance(model) ~ hatvalues(model))   
    
# ========================================
# Using CAR package to improve QQPLOT 
# ========================================
#install.packages("car") # Note - answered no to Question during install
library(car)
par(mfrow = c(1,1))
qqPlot(model$residuals) 
    
# ==================================
# Adding Legends in baseR
# ==================================
plot(iris$Sepal.Width ~ iris$Petal.Length, col = (4:6)[iris$Species], pch = 16)

legend("top", title = "Species", legend = c("setosa", "versicolor", "virginica"), fill = (4:6))


# ==================================
# Advanced Graphics with GGPLOT 
# ==================================
#install.packages("ggplot2") - # This should work on people's own laptops
library(ggplot2) 


#Task four: load the finch egg dataset. 
#Make a scatter plot of egg mass against female mass, 
#with colour proportional to clutch size.
# Read data in 
eggs<-read.table("FinchEggs.txt", header=TRUE)
str(eggs)

ggplot(eggs, aes(Egg_mass, F_mass, colour = Clutch_size)) +
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
  
  
