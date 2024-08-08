# Housekeeping
rm(list = ls())
par(mfrow = c(1,1))

# Set working directory


# Read data in 
suzi<-read.table("suzidata.txt", header=TRUE)
str(suzi)

# Explore data prior to analysis - plots 
plot(suzi$Silica~suzi$HerbivoreDamage, col=c(1:3), pch=16)
hist(suzi$Silica)
boxplot(suzi$Silica~suzi$HerbivoreSpecies, col=c(1:3))

# 1. formulate a model for the data including all the appropriate predictors and interactions
model <- lm(suzi$Silica ~ suzi$HerbivoreSpecies + suzi$HerbivoreDamage + 
              suzi$HerbivoreSpecies*suzi$HerbivoreDamage)
summary(model)
anova(model)

# 2. remove non-significant interactions (in this case: HerbivoreDamage:HerbivoreSpecies)
model2 <- lm(suzi$Silica~suzi$HerbivoreDamage+suzi$HerbivoreSpecies)
summary(model2)
anova(model2)

# 3. remove non-significant main effects 
# NB. None to remove here!

# 4. Check model assumption in plots for simplest/best model
  # Histogram of residuals
  hist(model2$residuals, breaks = 20)
  # Normal Q-Q plot
  qqnorm(model2$residuals)
  qqline(model2$residuals)
  
  # Fitted values vs. residuals
  plot(model2$residuals ~ model2$fitted.values, col=c(1:3), pch=16)
  abline(5,0)
  abline(0,0)
  abline(-5,0)

  # Subset data to look at each factor level for species
  sheep_res<-subset(model2$residuals,suzi$HerbivoreSpecies=="1.Sheep")
  vole_res<-subset(model2$residuals,suzi$HerbivoreSpecies=="2.Vole")
  rd_res<-subset(model2$residuals,suzi$HerbivoreSpecies=="3.RedDeer")

  # Plot out subsets - histogram of residuals
  hist(sheep_res)
  hist(vole_res)
  hist(rd_res)

  # QQ plots
  qqnorm(sheep_res)
  qqline(sheep_res)
  qqnorm(vole_res)
  qqline(vole_res)
  qqnorm(rd_res)
  qqline(rd_res)

  # Box plot of residuals for each species
  boxplot(sheep_res, vole_res, rd_res, col=c(1:3))

  # Cooks distance plot
  plot(cooks.distance(model2) ~ hatvalues(model2))

# 5. Interpret model coefficients 
summary(model2)
















