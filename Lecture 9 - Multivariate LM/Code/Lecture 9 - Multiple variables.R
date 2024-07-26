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



# Model fitting -----------------------------------------------------------

df1 <- data.frame(
  b0 = c(8, 0, 6, 3, 4, 3.4),  # intercept
  b1 = c(-7, 4, -2, 5, 3, 2.4),  # slope for wolf (Present vs Absent)
  b2 = c(0.5, 1.5, -1, 0, 1, 1.6),  # slope for survival
  grp = factor(1:6)
)

combined_df <- bind_rows(
  lapply(1:nrow(df1), function(i) {
    temp_df <- yst_dat
    temp_df$pred <- df1$b0[i] + df1$b1[i] * temp_df$present + df1$b2[i] * temp_df$survival
    temp_df$grp <- df1$grp[i]
    temp_df$fit_mean <- df1$b0[i] + df1$b1[i] * temp_df$present + df1$b2[i] * temp_df$survival
    temp_df$b0 <- df1$b0[i]
    temp_df$b1 <- df1$b1[i]
    temp_df$b2 <- df1$b2[i]
    temp_df
  })
)

a1 <- ggplot(combined_df, aes(x = survival, y = beaver, group = grp)) +
  geom_point(size = 2, aes(color = wolf)) +
  geom_abline(data = df1, aes(intercept = b0 + b1, slope = b2), 
              size = 1.5, show.legend = FALSE, colour = "red") +
  geom_abline(data = df1, aes(intercept = b0, slope = b2), 
              size = 1.5, show.legend = FALSE, colour = "orange") +
  geom_segment(aes(x = survival, xend = survival, y = beaver, yend = fit_mean), 
               linetype = "dashed", color = "white", alpha = 0.5) +
  scale_color_manual(values = c("Present" = "red", "Absent" = "orange")) +
  labs(x = "Survival",
       y = "Beaver Density",
       color = "Wolf Presence",
       subtitle = '{paste("mu =", round(df1[df1$grp == closest_state, "b0"], 2), 
                          "+", round(df1[df1$grp == closest_state, "b1"], 2), "* W",
                          "+", round(df1[df1$grp == closest_state, "b2"], 2), "* S")}') +
  sbs_theme() +
  transition_states(grp) + 
  ease_aes("cubic-in-out")

anim_save(here("Lecture 9 - Multivariate LM/Figures", file = "fitting.gif"), animation = a1)


# Calculate densities for each group
density_df <- bind_rows(
  lapply(unique(combined_df$grp), function(g) {
    temp_df <- combined_df %>% filter(grp == g)
    x_vals <- seq(min(temp_df$beaver) - 10, max(temp_df$beaver) + 10, length.out = 75)
    densities <- dnorm(x_vals, mean = mean(temp_df$fit_mean), sd = 1.69)
    data.frame(x = x_vals, y = densities, grp = g)
  })
)

# Update df1 with the new model parameters
df1 <- df1 %>%
  mutate(subtitle_text = paste0("beta[0] == ", round(b0, 2), ", ", "beta[1] == ", round(b1, 2), ", ", "beta[2] == ", round(b2, 2)))

# Join the new df1 to combined_df
combined_df <- combined_df %>%
  left_join(df1, by = "grp")

# Create the histogram with likelihood overlay plot
a2 <- ggplot() +
  geom_histogram(data = combined_df, aes(x = beaver, y = ..density..), binwidth = 1, alpha = 0.5, position = "identity") +
  geom_line(data = density_df, aes(x = x, y = y), size = 1.5) +
  scale_x_continuous(limits = c(min(combined_df$beaver) - 2, max(combined_df$beaver) + 2)) +
  labs(x = "Beaver Density", 
       y = "Density",
       fill = "Wolf Presence",
       subtitle = '{paste("mu =", round(df1[df1$grp == closest_state, "b0"], 2), "+ ", 
       round(df1[df1$grp == closest_state, "b1"], 2), "* W", "+ ", 
       round(df1[df1$grp == closest_state, "b2"], 2), "* S")}') +
  sbs_theme() +
  transition_states(grp) + 
  ease_aes("cubic-in-out")
a2

anim_save(here("Lecture 9 - Multivariate LM/Figures", file = "data_v_dnorm.gif"), animation = a2)


sigma_values <- c(10, 6, 0.9, 4, 2, 1.69)

# Calculate densities for each group with varying sigma
density_df <- bind_rows(
  lapply(unique(combined_df$grp), function(g) {
    temp_df <- combined_df %>% filter(grp == g)
    x_vals <- seq(min(temp_df$beaver) - 2, max(temp_df$beaver) + 2, length.out = 75)
    sigma <- sigma_values[as.numeric(g)]
    densities <- dnorm(x_vals, mean = mean(temp_df$fit_mean), sd = sigma)
    data.frame(x = x_vals, y = densities, grp = g, sigma = sigma)
  })
)
df1 <- df1 %>%
  mutate(subtitle_text = paste0("beta[0] == ", round(b0, 2), ", ", 
                                "beta[1] == ", round(b1, 2), ", ", 
                                "beta[2] == ", round(b2, 2)),
         sigma = sigma_values)
combined_df <- combined_df %>%
  left_join(df1, by = "grp")

a3 <- ggplot() +
  geom_histogram(data = combined_df, aes(x = beaver, y = ..density..), binwidth = 1, alpha = 0.5, position = "identity") +
  geom_line(data = density_df, aes(x = x, y = y), size = 1.5) +
  scale_x_continuous(limits = c(min(combined_df$beaver) - 2, max(combined_df$beaver) + 2)) +
  labs(
    x = "Beaver Density", 
    y = "Density",
    fill = "Wolf Presence",
    subtitle = '{paste("mu =", round(df1[df1$grp == closest_state, "b0"], 2), 
                     "+", round(df1[df1$grp == closest_state, "b1"], 2), "* W",
                     "+", round(df1[df1$grp == closest_state, "b2"], 2), "* S", 
                     "\nsigma =", round(df1[df1$grp == closest_state, "sigma"], 2))}'
  ) +
  sbs_theme() +
  transition_states(grp) + 
  ease_aes("cubic-in-out")

anim_save(here("Lecture 9 - Multivariate LM/Figures", file = "data_v_dnorm_sd.gif"), animation = a3)

m2 <- lm(beaver ~ wolf + survival, data = yst_dat)

nu_data <- expand.grid(wolf = unique(yst_dat$wolf),
                       survival = seq(0,1,length.out = 10
                                      ))
prds <- predict(m2, nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p <- ggplot(nu_data) +
  geom_line(aes(x = survival, y = fit, colour = wolf)) +
  geom_ribbon(aes(x = survival, ymin = low, ymax = upp, fill = wolf), alpha = 0.5) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x = "Beaver survival",
       colour = "Wolves",
       fill = "Wolves",
       y = "Beaver density") +
  sbs_theme()
p
ggsave(here("Lecture 9 - Multivariate LM/Figures", file = "beaver_wolf_survival.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)



# Complex model -----------------------------------------------------------

m3 <- lm(beaver ~ wolf + survival + reproduction + park, data = yst_dat)

nu_data <- expand.grid(wolf = "Present",
                      survival = seq(min(yst_dat$survival), max(yst_dat$survival), 0.1),
                      reproduction = median(yst_dat$reproduction),
                      park = "Fully protected")
prds <- predict(m3, nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p1 <- ggplot(nu_data) +
  geom_ribbon(aes(x = survival, ymin = low, ymax = upp), alpha = 0.3, fill = "lightgrey") +
  geom_line(aes(x = survival, y = fit)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Beaver survival",
       colour = "Wolves",
       fill = "Wolves",
       y = "Beaver density") +
  sbs_theme()
p1

nu_data <- expand.grid(wolf = c("Present", "Absent"),
                       survival = median(yst_dat$survival),
                       reproduction = median(yst_dat$reproduction),
                       park = "Fully protected")
nu_data$wolf <- factor(nu_data$wolf, levels = c("Absent", "Present"))
prds <- predict(m3, nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p2 <- ggplot(nu_data) +
  geom_point(aes(x = wolf, y = fit)) +
  geom_errorbar(aes(x = wolf, ymin = low, ymax = upp), width = 0.1, colour = "white") +
  labs(x = "Wolf presence",
       y = "Beaver density") +
  sbs_theme()
p2

nu_data <- expand.grid(wolf = "Present",
                       survival = median(yst_dat$survival),
                       reproduction = seq(min(yst_dat$reproduction), max(yst_dat$reproduction), 0.25),
                       park = "Fully protected")
prds <- predict(m3, nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p3 <- ggplot(nu_data) +
  geom_ribbon(aes(x = reproduction, ymin = low, ymax = upp), alpha = 0.3, fill = "lightgrey") +
  geom_line(aes(x = reproduction, y = fit)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Beaver reproduction",
       colour = "Wolves",
       fill = "Wolves",
       y = "Beaver density") +
  sbs_theme()
p3

nu_data <- expand.grid(wolf = "Present",
                       survival = median(yst_dat$survival),
                       reproduction = median(yst_dat$reproduction),
                       park = c("Fully protected", "Partially protected"))
prds <- predict(m3, nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p4 <- ggplot(nu_data) +
  geom_point(aes(x = park, y = fit)) +
  geom_errorbar(aes(x = park, ymin = low, ymax = upp), width = 0.1, colour = "white") +
  labs(x = "Park protection",
       y = "Beaver density") +
  sbs_theme()
p4

design <- "
AB
CD
"

p <- p1 + p4 + p3 + p2 + plot_layout(design = design)
ggsave(here("Lecture 9 - Multivariate LM/Figures", file = "complex_results.png"), plot = p,width = 1400/72, height = 650/72, dpi = 72)
ggsave(here("Lecture 9 - Multivariate LM/Figures", file = "beaver_no_wolf.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)
