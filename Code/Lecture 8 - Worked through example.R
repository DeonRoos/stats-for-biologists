# Analysis of Netflix average total watch time between regions
# Author: Deon Roos
library(ggplot2) # For plotting
# Step 1: Get data --------------------------------------------------------
net_dat <- read.table("H:\\003 - Teaching\\Lecture 8 - Cat diagnostics\\Data\\netflix_time.csv",
                 header = TRUE, stringsAsFactors = TRUE)
# Step 2: Explore data ----------------------------------------------------
summary(net_dat)
ggplot(net_dat, aes(x = region, y = netflix))+
  geom_jitter(height = 0, width = 0.1)
# Step 3: Fit model -------------------------------------------------------
net_mod <- lm(netflix ~ region, data = net_dat)
# Step 4: Diagnose --------------------------------------------------------
plot(net_mod, ask = FALSE)
# Step 5: Summarise -------------------------------------------------------
summary(net_mod)
# Step 6: Plot predictions ------------------------------------------------
fake <- data.frame(region = unique(net_dat$region))
prds <- predict(net_mod, newdata = fake, se.fit = TRUE)
fake$fit <- prds$fit
fake$low <- prds$fit - prds$se.fit * 1.96
fake$upp <- prds$fit + prds$se.fit * 1.96
ggplot(fake) +
  geom_point(aes(x = region, y = fit)) +
  geom_errorbar(aes(x = region, ymin = low, ymax = upp)) +
  labs(x = "Region",
       y = "Netflix time (minutes)")


