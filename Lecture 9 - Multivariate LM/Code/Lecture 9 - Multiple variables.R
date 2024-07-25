# lecture 9 - multivariate linear models
# Using horn length with hunting as example (data from Marco paper)

library(ggplot2)
library(here)
library(patchwork)
library(gganimate)
library(dplyr)
source("sbs_theme.R")
source("sbsvoid_theme.R")


# Example dataset ---------------------------------------------------------
# Number of varrao mites (if infected) in honey bee hives

n_years <- 23
n_sites <- 5
n_samples <- n_years * n_sites
yst_dat <- expand.grid(
  year = 1:n_years,
  site = toupper(letters[1:n_sites])
)

yst_dat$park <- ifelse(yst_dat$year > 13, "Fully protected", "Partially protected")

thresholds <- 13 + sample(-2:2, n_sites, replace = TRUE)
yst_dat$wolf <- sapply(1:n_samples, function(i) {
  site_idx <- which(toupper(letters[1:n_sites]) == yst_dat$site[i])
  ifelse(yst_dat$year[i] > thresholds[site_idx], "Present", "Absent")
})

# Beaver survival - varies over time and sites
mean_survival <- 0.8
sd_survival <- 0.1
calculate_alpha_beta <- function(mean, sd) {
  var = sd^2
  alpha = ((1 - mean) / var - 1 / mean) * mean^2
  beta = alpha * (1 / mean - 1)
  return(c(alpha, beta))
}
yst_dat$survival <- sapply(1:n_samples, function(i) {
  site_idx <- which(toupper(letters[1:n_sites]) == yst_dat$site[i])
  year_idx <- yst_dat$year[i]
  site_variation <- runif(1, -0.02, 0.02) # Site-specific variation
  year_variation <- runif(1, -0.01, 0.01) # Year-specific variation
  adjusted_mean_survival <- mean_survival + site_variation + year_variation
  adjusted_mean_survival <- max(min(adjusted_mean_survival, 1), 0) # Ensure within [0, 1] range
  params <- calculate_alpha_beta(adjusted_mean_survival, sd_survival)
  rbeta(1, params[1], params[2])
})

yst_dat$survival <- round(yst_dat$survival, digits = 2)

# Beaver reproduction - varies over time and sites
mean_lambda <- 3
yst_dat$reproduction <- sapply(1:n_samples, function(i) {
  site_idx <- which(toupper(letters[1:n_sites]) == yst_dat$site[i])
  year_idx <- yst_dat$year[i]
  site_variation <- runif(1, -0.5, 0.5) # Site-specific variation
  year_variation <- runif(1, -0.3, 0.3) # Year-specific variation
  adjusted_lambda <- mean_lambda + site_variation + year_variation
  adjusted_lambda <- max(adjusted_lambda, 0) # Ensure lambda is non-negative
  
  rpois(1, adjusted_lambda)
})


# Beaver density ----------------------------------------------------------

b0 <- 3 # Intercept (site A, fully protected, wolf absent, survival 0, reproduction 0)
b1 <- 2.5 # Site B
b2 <- 1.5 # Site C
b3 <- 0.5 # Site D
b4 <- 3   # Site E
b5 <- -3  # partially protected
b6 <- 0   # wolves
b7 <- 2   # survival
b8 <- 0.5 # offspring

yst_dat$b <- ifelse(yst_dat$site == "B", 1, 0)
yst_dat$c <- ifelse(yst_dat$site == "C", 1, 0)
yst_dat$d <- ifelse(yst_dat$site == "D", 1, 0)
yst_dat$e <- ifelse(yst_dat$site == "E", 1, 0)
yst_dat$part <- ifelse(yst_dat$park == "Partially protected", 1, 0)
yst_dat$present <- ifelse(yst_dat$wolf == "Present", 1, 0)

yst_dat$mu <- b0 + b1 * yst_dat$b + b2 * yst_dat$c + b3 * yst_dat$d + b4 * yst_dat$e +
  b5 * yst_dat$part +
  b6 * yst_dat$present +
  b7 * yst_dat$survival +
  b8 * yst_dat$reproduction
yst_dat$beaver <- rnorm(n = n_samples, mean = yst_dat$mu, sd = 0.5)

range(yst_dat$beaver)

m1 <- lm(beaver ~ wolf, data = yst_dat)
summary(m1)

m2 <- lm(beaver ~ site + survival + reproduction + wolf + park, data = yst_dat)
summary(m2)


# Confounded relationship -------------------------------------------------

nu_data <- data.frame(wolf = unique(yst_dat$wolf))
prds <- predict(m1, nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p <- ggplot(nu_data) +
  geom_point(aes(x = wolf, y = fit)) +
  geom_errorbar(aes(x = wolf, ymin = low, ymax = upp), width = 0.1, colour = "white") +
  labs(x = "Wolf presence",
       y = "Beaver density") +
  sbs_theme()

ggsave(here("Lecture 9 - Multivariate LM/Figures", file = "beaver_wolf.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)
