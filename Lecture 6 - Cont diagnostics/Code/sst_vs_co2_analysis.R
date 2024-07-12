# Analysis of SST to see if related to CO2 emissions
# Author: Deon Roos
library(ggplot2) # For plotting
# Step 1: Get data --------------------------------------------------------
cc_dat <- read.csv("H:\\003 - Teaching\\006-BI3010\\Lecture 6 - Cont diagnostics\\Data\\cc_dat.csv",
                   header = TRUE)
# Step 2: Explore data ----------------------------------------------------
summary(cc_dat)
ggplot(dat, aes(x = co2, y = sst)) +
  geom_point()
# Step 3: Fit model -------------------------------------------------------
cc_mod <- lm(sst ~ co2, data = dat)
# Step 4: Diagnose --------------------------------------------------------
plot(cc_mod, ask = FALSE)
# Step 5: Summarise -------------------------------------------------------
summary(cc_mod)
# Step 6: Plot predictions ------------------------------------------------
fake <- data.frame(co2 = seq(from = min(cc_dat$co2),
                             to = max(cc_dat$co2),
                             length.out = 25))
fake$fit <- predict(cc_mod, newdata = fake)
ggplot() +
  geom_line(data = fake, aes(x = co2, y = fit)) +
  geom_point(data = cc_dat, aes(x = co2, y = sst)) +
  labs(x = "CO2 (millions of tonnes)",
       y = "Sea surface temperature (degrees C)")