library(ggplot2)

# Provided intercept, slope, and standard deviation
intercept <- coef(owl_model)[1]
slope <- coef(owl_model)[2]
sigma <- sigma(owl_model)

break_size <- 1
marten_values <- 1:5

# Generate the expected distributions
dens <- do.call(rbind, lapply(marten_values, function(marten) {
  expected_mean <- intercept + slope * marten
  xs <- seq(expected_mean - 3*sigma, expected_mean + 3*sigma, length.out = 50)
  res <- data.frame(y = xs,
                    x = marten - break_size * dnorm(xs, expected_mean, sigma),
                    section = as.factor(marten))
  res$type <- rep("Expected", 50)
  res
}))

# Plot the expected distributions
p1 <- ggplot(data = df, aes(x = marten, y = tawny)) +
  geom_point(data = df, aes(marten, tawny), colour = "white", alpha = 0.4) +
  labs(colour = "Marten Value",
       x = "Pine marten",
       y = "Tawny owl") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(limits = c(min(df$marten), NA)) +
  scale_y_continuous(limits = c(5, 55)) +
  sbs_theme()
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p1.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

p2 <- ggplot() +
  geom_abline(intercept = intercept, slope =  slope, colour = "white") +
  geom_point(data = df, aes(marten, tawny), colour = "white", alpha = 0.4) +
  labs(colour = "Marten Value",
       x = "Pine marten",
       y = "Tawny owl") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(limits = c(min(df$marten), NA)) +
  scale_y_continuous(limits = c(5, 55)) +
  sbs_theme()
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p2.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)

p3 <- ggplot() +
  geom_abline(intercept = intercept, slope =  slope, colour = "white") +
  geom_point(data = df, aes(marten, tawny), colour = "white", alpha = 0.4) +
  geom_path(data = dens, aes(x, y, group = section), lwd = 1, colour = "red") +
  labs(colour = "Marten Value",
       x = "Pine marten",
       y = "Tawny owl") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(limits = c(min(df$marten), NA)) +
  scale_y_continuous(limits = c(5, 55)) +
  sbs_theme()
p3
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p3.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)

p4 <- ggplot() +
  geom_ribbon(data = data.frame(x = seq(min(df$marten), max(df$marten), length.out = 100)), 
              aes(x = x, 
                  ymin = (intercept + slope * x) - sigma, 
                  ymax = (intercept + slope * x) + sigma), 
              fill = "red", alpha = 0.6) +
  geom_abline(intercept = intercept, slope =  slope, colour = "white") +
  geom_point(data = df, aes(marten, tawny), colour = "white", alpha = 0.4) +
  geom_path(data = dens, aes(x, y, group = section), lwd = 1, colour = "red") +
  labs(colour = "Marten Value",
       x = "Pine marten",
       y = "Tawny owl") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(limits = c(min(df$marten), NA)) +
  scale_y_continuous(limits = c(5, 55)) +
  sbs_theme()
p4
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p4.png"), plot = p4, width = 650/72, height = 775/72, dpi = 72)

p5 <- ggplot() +
  geom_ribbon(data = data.frame(x = seq(min(df$marten), max(df$marten), length.out = 100)), 
              aes(x = x, 
                  ymin = (intercept + slope * x) - sigma, 
                  ymax = (intercept + slope * x) + sigma), 
              fill = "red", alpha = 0.4) +
  geom_ribbon(data = data.frame(x = seq(min(df$marten), max(df$marten), length.out = 100)), 
              aes(x = x, 
                  ymin = (intercept + slope * x) - 2 * sigma, 
                  ymax = (intercept + slope * x) + 2 * sigma), 
              fill = "red", alpha = 0.4) +
  geom_abline(intercept = intercept, slope =  slope, colour = "white") +
  geom_point(data = df, aes(marten, tawny), colour = "white", alpha = 0.4) +
  geom_path(data = dens, aes(x, y, group = section), lwd = 1, colour = "red") +
  labs(colour = "Marten Value",
       x = "Pine marten",
       y = "Tawny owl") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(limits = c(min(df$marten), NA)) +
  scale_y_continuous(limits = c(5, 55)) +
  sbs_theme()
p5
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p5.png"), plot = p5, width = 650/72, height = 775/72, dpi = 72)

p6 <- ggplot() +
  geom_ribbon(data = data.frame(x = seq(min(df$marten), max(df$marten), length.out = 100)), 
              aes(x = x, 
                  ymin = (intercept + slope * x) - sigma, 
                  ymax = (intercept + slope * x) + sigma), 
              fill = "red", alpha = 0.35) +
  geom_ribbon(data = data.frame(x = seq(min(df$marten), max(df$marten), length.out = 100)), 
              aes(x = x, 
                  ymin = (intercept + slope * x) - 2 * sigma, 
                  ymax = (intercept + slope * x) + 2 * sigma), 
              fill = "red", alpha = 0.25) +
  geom_ribbon(data = data.frame(x = seq(min(df$marten), max(df$marten), length.out = 100)), 
              aes(x = x, 
                  ymin = (intercept + slope * x) - 3 * sigma, 
                  ymax = (intercept + slope * x) + 3 * sigma), 
              fill = "red", alpha = 0.2) +
  geom_abline(intercept = intercept, slope =  slope, colour = "white") +
  geom_point(data = df, aes(marten, tawny), colour = "white", alpha = 0.4) +
  geom_path(data = dens, aes(x, y, group = section), lwd = 1, colour = "red") +
  labs(colour = "Marten Value",
       x = "Pine marten",
       y = "Tawny owl") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(limits = c(min(df$marten), NA)) +
  scale_y_continuous(limits = c(5, 55)) +
  sbs_theme()
p6
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p6.png"), plot = p6, width = 650/72, height = 775/72, dpi = 72)

df$resids <- resid(owl_model)

ggplot(df) +
  geom_histogram(aes(x = resids)) +
  labs(x = "Residuals",
       y = "Frequency") +
  geom_vline(xintercept = 0 + sigma, colour = "red", linewidth = 2.5, linetype = 2) +
  geom_vline(xintercept = 0 - sigma, colour = "red", linewidth = 2.5, linetype = 2) +
  geom_vline(xintercept = 0 + 2 * sigma, colour = "red", linewidth = 1.5, linetype = 2) +
  geom_vline(xintercept = 0 - 2 * sigma, colour = "red", linewidth = 1.5, linetype = 2) +
  geom_vline(xintercept = 0 + 3 * sigma, colour = "red", linewidth = .5, linetype = 2) +
  geom_vline(xintercept = 0 - 3 * sigma, colour = "red", linewidth = .5, linetype = 2) +
  sbs_theme()





breaks <- seq(min(df$resids), max(df$resids), by = 0.5)  # Specify breaks for better control
bins <- length(breaks) - 1  # Number of bins based on breaks

df_histogram <- as.data.frame(table(cut(df$resids, breaks = breaks)))
df_histogram$middle_value <- (breaks[-1] + breaks[-length(breaks)]) / 2

alpha_min <- 0.2
alpha_max <- 1

df_histogram$alpha <- ifelse(abs(df_histogram$middle_value) <= sigma, 1,
                             ifelse(abs(df_histogram$middle_value) <= 2 * sigma, 0.5,
                                    ifelse(abs(df_histogram$middle_value) <= 3 * sigma, 0.2, NA)))

df_histogram <- df_histogram[!is.na(df_histogram$alpha), ]


p7 <- ggplot(df_histogram, aes(x = middle_value, y = Freq, fill = "red", alpha = factor(alpha))) +
  geom_bar(stat = "identity", show.legend = FALSE, colour = "transparent") +
  geom_vline(xintercept = 0, colour = "white") +
  labs(x = "Residuals", y = "Frequency") +
  scale_alpha_manual(values = c("1" = 1, "0.5" = 0.6, "0.2" = 0.4)) +
  sbs_theme()
p7
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p7.png"), plot = p7, width = 650/72, height = 775/72, dpi = 72)


# Calculate expected normal distribution values
xs <- seq(min(df_histogram$middle_value), max(df_histogram$middle_value), length.out = 100)
expected_density <- dnorm(xs, mean = 0, sd = sigma)

# Scale expected_density to match maximum Freq in df_histogram
scaling_factor <- max(df_histogram$Freq) / max(expected_density)
expected_density <- expected_density * scaling_factor

df_expected <- data.frame(xs = xs, expected_density = expected_density)

# Create the histogram with customized fill and alpha
p8 <- ggplot(df_histogram, aes(x = middle_value, y = Freq, fill = "red")) +
  geom_bar(stat = "identity", aes(alpha = factor(alpha)), show.legend = FALSE, colour = "transparent") +
  geom_vline(xintercept = 0, colour = "white") +
  labs(x = "Residuals", 
       y = "Frequency",
       title = expression(paste("Normal(0, ", sigma[i], ")"))) +
  scale_alpha_manual(values = c("1" = 1, "0.5" = 0.5, "0.2" = 0.2), guide = "none") +
  geom_line(data = df_expected, aes(xs, expected_density), color = "red", size = 1) +
  sbs_theme()
p8
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p8.png"), plot = p8, width = 650/72, height = 775/72, dpi = 72)

par(mfrow = c(2,2), bg = "black", col = "white", col.axis = "white", col.lab = "white")
plot(owl_model)

par(mfrow = c(1,1), bg = "black", col = "white", col.axis = "white", col.lab = "white")
plot(owl_model, ask = FALSE)

df$fitted <- predict(owl_model)

df[df$resids < -5 & df$fitted < 28,]

library(mgcv)
dat <- gamSim(eg = 1, n = 373, dist = "normal")

eg_mod <- lm(y ~ x2, data = dat)
par(mfrow = c(1,1), bg = "black", col = "white", col.axis = "white", col.lab = "white")
plot(eg_mod, ask = FALSE)

set.seed(123)  # Set seed for reproducibility
n <- 100  # Total number of observations
x <- rep(1:10, each = n/10)  # Independent variable with 5 levels

# Define different standard deviations for each level of x
sigma <- c(1, 2, 3, 4, 5)

# Simulate data with heteroscedasticity
sim_data <- lapply(1:5, function(i) {
  sd_i <- sigma[i]
  x_i <- x == i
  y_i <- rnorm(sum(x_i), mean = 0, sd = sd_i)
  data.frame(x = rep(i, sum(x_i)), y = y_i)
})

# Combine simulated data into a single dataframe
sim_data <- do.call(rbind, sim_data)

plot(lm(y ~ x, data = sim_data), ask = FALSE)



# Cooks -------------------------------------------------------------------



dat <- data.frame(
  x = c(1,2,3,4)
)

dat$y <- 5 + 0.5 * dat$x
coef(lm(y ~ x, data = dat))
p1 <- ggplot(data = dat, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = 5, slope = 0.5, colour = "white") +
  labs(x = "Explanatory variable",
       y = "Response variable") +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 10)) +
  sbs_theme()


dat <- data.frame(
  x = c(1,2,3,4)
)

dat$y <- 5 + 0.5 * dat$x
dat1 <- data.frame(x = 5,
                   y = 2)
dat_bad <- rbind(dat, dat1)

p2 <- ggplot(data = dat_bad, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = 5, slope = 0.5, colour = "white") +
  labs(x = "Explanatory variable",
       y = "Response variable") +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 10)) +
  sbs_theme()

coef(lm(y ~ x, data = dat_bad))

p3 <- ggplot(data = dat_bad, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = 7.2, slope = -0.6, colour = "white") +
  labs(x = "Explanatory variable",
       y = "Response variable") +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 10)) +
  sbs_theme()


dat1 <- data.frame(x = 8,
                   y = 2)
dat_bad <- rbind(dat, dat1)

coef(lm(y ~ x, data = dat_bad))

p4 <- ggplot(data = dat_bad, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = 7.4, slope = -0.55, colour = "white") +
  labs(x = "Explanatory variable",
       y = "Response variable") +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 10)) +
  sbs_theme()

p4.1 <- ggplot(data = dat_bad, aes(x = x, y = y)) +
  geom_point() +
  labs(x = "Explanatory variable",
       y = "Response variable") +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 10)) +
  sbs_theme()


dat_bad$y <- 5 + 0.5 * dat_bad$x

p5 <- ggplot(data = dat_bad, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = 5, slope = 0.5, colour = "white") +
  labs(x = "Explanatory variable",
       y = "Response variable") +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 10)) +
  sbs_theme()

p1
p2
p3
p4
p4.1
p5

ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p9.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p10.png"), plot = p5, width = 650/72, height = 775/72, dpi = 72)
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p11.png"), plot = p4.1, width = 650/72, height = 775/72, dpi = 72)
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p12.png"), plot = p4, width = 650/72, height = 775/72, dpi = 72)

dat1 <- data.frame(x = 8,
                   y = 2)
dat2 <- data.frame(x = 8)
dat2$y <- 5 + 0.5 * dat2$x
dat_bad <- rbind(dat, dat1, dat2)

p6 <- ggplot(data = dat_bad, aes(x = x, y = y)) +
  geom_point() +
  labs(x = "Explanatory variable",
       y = "Response variable") +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 10)) +
  sbs_theme()
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "p13.png"), plot = p6, width = 650/72, height = 775/72, dpi = 72)


dat1 <- data.frame(x = 8,
                   y = 2)
dat_bad <- rbind(dat, dat1)
plot(lm(y ~ x, data = dat_bad))




# Worked example data -----------------------------------------------------

library(tidyverse)
library(lubridate)

co2 <- read.csv("H:\\003 - Teaching\\006-BI3010\\Lecture 6 - Cont diagnostics\\Data\\co2_owid.csv",
                header = TRUE,
                stringsAsFactors = TRUE)
co2 <- co2 |> 
  group_by(Year) |> 
  summarise(co2 = sum(Annual.CO..emissions))

co2 <- co2[co2$Year > 1950 & co2$Year < 2021,]

sst <- read.csv("H:\\003 - Teaching\\006-BI3010\\Lecture 6 - Cont diagnostics\\Data\\sst_owid.csv",
                header = TRUE,
                stringsAsFactors = TRUE)

sst$date <- dmy(sst$Day)
sst$year <- year(sst$date)
sst <- sst[!is.na(sst$date),]
sst <- sst[sst$year > 1950 & sst$year < 2021,]

sst <- sst |> 
  group_by(year) |> 
  summarise(sst = mean(Monthly.sea.surface.temperature.anomaly))

head(sst)

dat <- data.frame(
  year = sst$year,
  sst = sst$sst,
  co2 = co2$co2/1000000
)

# write.csv(dat, here("Lecture 6 - Cont diagnostics/Data", file = "cc_dat.csv"),
#           row.names = FALSE)

cc_dat <- read.csv("H:\\003 - Teaching\\006-BI3010\\Lecture 6 - Cont diagnostics\\Data\\cc_dat.csv",
                   header = TRUE,
                   stringsAsFactors = TRUE)
head(cc_dat, n = 10)
summary(cc_dat)
p <- ggplot(dat, aes(x = co2, y = sst)) +
  geom_point() +
  sbs_theme()
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "cc_eda.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)

cc_mod <- lm(sst ~ co2, data = dat)
summary(cc_mod)

par(mfrow = c(2,2), bg = "black", col = "white", col.axis = "white", col.lab = "white")
plot(cc_mod, ask = F)

summary(cc_mod)

fake <- data.frame(
  co2 = seq(from = min(cc_dat$co2),
            to = max(cc_dat$co2),
            length.out = 25)
)
fake$fit <- predict(cc_mod, newdata = fake)
fake

p1 <- ggplot(dat) +
  geom_line(aes(x = co2, y = fit)) +
  labs(x = "CO2 (millions of tonnes)",
       y = "Sea surface temperature (degrees C)") +
  scale_x_continuous(labels = scales::comma) +
  sbs_theme()
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "cc_pred.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)


p2 <- ggplot() +
  geom_line(data = fake, aes(x = co2, y = fit)) +
  geom_point(data = cc_dat, aes(x = co2, y = sst)) +
  labs(x = "CO2 (millions of tonnes)",
       y = "Sea surface temperature (degrees C)") +
  scale_x_continuous(labels = scales::comma) +
  sbs_theme()
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "cc_pred_dat.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)

cc_dat$resid <- resid(cc_mod)
cc_dat$residf <- ifelse(cc_dat$resid <= 0, "Negative", "Positive")
p3 <- ggplot() +
  geom_line(data = fake, aes(x = co2, y = fit)) +
  geom_point(data = cc_dat, aes(x = co2, y = sst, colour = residf), size = 2) +
  scale_colour_brewer(palette = "Set2") +
  labs(x = "CO2 (millions of tonnes)",
       y = "Sea surface temperature (degrees C)",
       colour = "Residual error") +
  scale_x_continuous(labels = scales::comma) +
  sbs_theme()
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "cc_pred_nonlin.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)


p4 <- ggplot() +
  geom_smooth(data = cc_dat, aes(x = co2, y = sst), colour = "red", method = "gam", se = FALSE) +
  geom_line(data = fake, aes(x = co2, y = fit)) +
  geom_point(data = cc_dat, aes(x = co2, y = sst, colour = residf), size = 2) +
  scale_colour_brewer(palette = "Set2") +
  labs(x = "CO2 (millions of tonnes)",
       y = "Sea surface temperature (degrees C)",
       colour = "Residual error") +
  scale_x_continuous(labels = scales::comma) +
  sbs_theme()
p4
ggsave(here("Lecture 6 - Cont diagnostics/Figures", file = "cc_pred_sline.png"), plot = p4, width = 650/72, height = 775/72, dpi = 72)
