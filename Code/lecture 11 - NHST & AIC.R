
# AIC and NHST ------------------------------------------------------------


# Penguin data ------------------------------------------------------------

library(palmerpenguins)
library(ggplot2)
library(gganimate)
source("sbs_theme.R")
source("sbsvoid_theme.R")

df <- data.frame(
  x = seq(0, 2.5, by = 0.01)
)

df$L <- dnorm(df$x, mean = 1.14, sd = 0.15)
ci_low <- qnorm(0.002501893, mean = 1.14, sd = 0.15)
ci_high <- qnorm(1-0.002501893, mean = 1.14, sd = 0.15)

p1 <- ggplot(df) +
  geom_line(data = df, aes(x = x, y = L)) +
  labs(x = "Adult Emporer Penguin Height (m)",
       y = "") +
  annotate("segment", x = 1.7, xend = 1.7, y = 0.4, yend = dnorm(2, mean = 1.14, sd = 0.3)+0.01,
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  annotate("text", x = 1.7, y = dnorm(1.7, mean = 1.14, sd = 0.3) + 0.35, 
           label = "Our 1.7 m\ntall penguin", color = "white", hjust = 0.5, size = 8) +
  sbs_theme()
p1
ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "distribution.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

p2 <- ggplot(df) +
  geom_line(aes(x = x, y = L)) +
  geom_area(aes(x = x, y = L), fill = "#1B9E77") +
  #geom_area(data = subset(df, x >= ci_low & x <= ci_high), aes(x = x, y = L), fill = "blue", alpha = 0.5) +
  labs(x = "Adult Emporer Penguin Height (m)",
       y = "") +
  annotate("segment", x = 1.7, xend = 1.7, y = 0.4, yend = dnorm(2, mean = 1.14, sd = 0.3)+0.01,
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  annotate("text", x = 1.7, y = dnorm(1.7, mean = 1.14, sd = 0.3) + 0.35, 
           label = "Our 1.7 m\ntall penguin", color = "white", hjust = 0.5, size = 8) +
  annotate("segment", x = 0.75, xend = 1.14, y = 1.2, yend = dnorm(1.14, mean = 1.14, sd = 0.3) - 0.01,
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  annotate("text", x = 0.3, y = 1.2, 
           label = "100% of the area\nunder the curve", color = "white", hjust = 0.5, size = 8) +
  sbs_theme()
p2

ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "100_distribution.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)

p3 <- ggplot(df) +
  geom_area(aes(x = x, y = L), fill = "#1B9E77") +
  geom_area(data = subset(df, x < ci_low), aes(x = x, y = L), fill = "#D95F02") +
  geom_area(data = subset(df, x > ci_high), aes(x = x, y = L), fill = "#D95F02") +
  geom_line(aes(x = x, y = L)) +
  labs(x = "Emperor Penguin Height (m)",
       y = "") +
  # annotate("segment", x = 2, xend = 2, y = 0.4, yend = dnorm(2, mean = 1.14, sd = 0.3)+0.01,
  #          arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  # annotate("text", x = 2, y = dnorm(2, mean = 1.14, sd = 0.3) + 0.45, 
  #          label = "Our 2m tall penguin", color = "white", hjust = 0.5, size = 8) +
  annotate("segment", x = 0.75, xend = 1.14, y = 1.2, yend = dnorm(1.14, mean = 1.14, sd = 0.3) - 0.01,
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  annotate("text", x = 0.3, y = 1.2, 
           label = "99.75% of the area\nunder the curve", color = "white", hjust = 0.5, size = 8) +
  annotate("segment", x = 0.5, xend = ci_low-0.1, y = 0.3, yend = 0.05,
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  annotate("text", x = 0.3, y = 0.5, 
           label = "0.025% of the area\nunder the curve", color = "white", hjust = 0.5, size = 8) +
  annotate("segment", x = 1.8, xend = ci_high+0.1, y = 0.3, yend = 0.05,
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  annotate("text", x = 2.1, y = 0.5, 
           label = "0.025% of the area\nunder the curve", color = "white", hjust = 0.5, size = 8) +
  sbs_theme() +
  geom_vline(xintercept = ci_low, linetype = "dashed", color = "white") +
  geom_vline(xintercept = ci_high, linetype = "dashed", color = "white")
p3

ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "95_distribution.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)


# Create data frame for df1 with varying means
df1 <- data.frame(
  x = seq(0.5, 2.5, by = 0.01),
)

df1$L <- dnorm(df1$x, mean = 1.5, sd = 0.15)

means <- c(1.95, 1.8, 1.7, 2.2, 1.6)

# Create data frame for df1 with varying means
df1 <- data.frame(
  x = rep(seq(0, 2.5, by = 0.01), times = length(means)),
  mean = rep(means, each = length(seq(0, 2.5, by = 0.01)))
)

df1$L <- dnorm(df1$x, mean = df1$mean, sd = 0.15)

p5 <- ggplot() +
  geom_line(data = df, aes(x = x, y = L)) +
  geom_line(data = df1, aes(x = x, y = L, group = mean), color = "#D95F02") +
  labs(x = "Adult Emperor Penguin Height (m)", y = "") +
  annotate("segment", x = 1.7, xend = 1.7, y = 0.4, yend = dnorm(2, mean = 1.14, sd = 0.3) + 0.01,
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  annotate("text", x = 1.7, y = dnorm(2, mean = 1.14, sd = 0.3) + 0.55, 
           label = "Our 1.7 m\ntall penguin", color = "white", hjust = 0.5, size = 8) +
  sbs_theme() +
  transition_states(mean, transition_length = 2, state_length = 1)
p5
anim_save(here::here("Figures/Lecture 11 - NHST & AIC", file = "alternative_distribution.gif"), animation = animate(p5, width = 650, height = 775))



# Parameter NHST ----------------------------------------------------------



df <- data.frame(
  x = seq(-10, 10, by = 0.01)
)

df$L <- dnorm(df$x, mean = 0, sd = 1.5)

p1 <- ggplot(df) +
  geom_line(data = df, aes(x = x, y = L)) +
  labs(x = expression("Feasible values for " * beta[1]),
       y = "") +
  annotate("segment", x = 5.3, xend = 5.3, y = 0.15, yend = dnorm(1.7, mean = 0, sd = 0.5) + 0.01,
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  annotate("text", x = 5.3, y = dnorm(5.3, mean = 0, sd = 0.5) + 0.16, 
           label = expression(beta[1]~"estimate"), color = "white", hjust = 0.5, size = 8) +
  sbs_theme()

p1
ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "parameter_distribution.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

df <- data.frame(
  krill = 0:50
)

df$tall <- 1.4

p2 <- ggplot(df) +
  geom_line(aes(x = krill, y = tall)) +
  labs(x = "Krill (kg)",
       y = "Peguin height (m)") +
  sbs_theme()

ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "peng_tall.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)


# Penguins ----------------------------------------------------------------

data(penguins)

df <- penguins[penguins$species == "Adelie",]

colnames(df) <- c("species", "island", "bill", "bill2", "flip", "body", "sex", "year")

p1 <- ggplot(df) +
  geom_jitter(aes(x = island, y = body), width = 0.1) +
  labs(x = "Island",
       y = "Body mass (g)") +
  sbs_theme()

ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "adelie.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

peng_mod <- lm(body ~ island, data = df)

summary(peng_mod)

fake <- data.frame(
  island = unique(df$island)
)

prds <- predict(peng_mod, fake, se.fit = TRUE)
fake$fit <- prds$fit
fake$low <- prds$fit - 1.96 * prds$se.fit
fake$upp <- prds$fit + 1.96 * prds$se.fit

p <- ggplot(fake) +
  geom_errorbar(aes(x = island, ymin = low, ymax = upp), width = 0.1, colour = "white") +
  geom_point(aes(x = island, y = fit)) +
  labs(x = "Island",
       y = "Body mass (g)") +
  sbs_theme()

ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "adelie_fit.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)

par(mfrow = c(2,2), bg = "black", col = "white", col.axis = "white", col.lab = "white")
plot(m1)


df <- data.frame(
  x = seq(-5, 5, by = 0.01)
)

df$L <- dt(df$x, df = nrow(penguins) - 3 - 1)

p1 <- ggplot(df) +
  geom_line(data = df, aes(x = x, y = L)) +
  labs(x = "T distribution",
       y = "") +
  sbs_theme()
p1
ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "T.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)


df <- data.frame(
  x = seq(-10, 70, by = 0.01)
)

df$L <- dt(df$x, df = nrow(penguins) - 3 - 1)

p1 <- ggplot(df) +
  geom_line(data = df, aes(x = x, y = L)) +
  labs(x = "T distribution",
       y = "") +
  annotate("segment", x = 53.314, xend = 53.314, y = 0.05, yend = 0.001,
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  annotate("text", x = 53.314, y = 0.075, 
           label = bquote(t ~ value ~ "for" ~ beta[1]), color = "white", hjust = 0.5, size = 8) +
  sbs_theme()
p1
ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "T_b1.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

df <- data.frame(
  x = seq(-3, 3, by = 0.01)
)

df$L <- dt(df$x, df = nrow(penguins) - 3 - 1)

p1 <- ggplot(df) +
  geom_line(data = df, aes(x = x, y = L)) +
  labs(x = "T distribution",
       y = "") +
  annotate("segment", x = -0.229, xend = -0.229, y = 0.05, yend = 0.001,
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  annotate("text", x = -0.229, y = 0.075, 
           label = bquote(t ~ value ~ "for" ~ beta[2]), color = "white", hjust = 0.5, size = 8) +
  sbs_theme()
p1
ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "T_b2.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

df <- data.frame(
  x = seq(-3, 3, by = 0.01)
)

df$L <- dt(df$x, df = nrow(penguins) - 3 - 1)

p1 <- ggplot(df) +
  geom_line(data = df, aes(x = x, y = L)) +
  labs(x = "T distribution",
       y = "") +
  annotate("segment", x = -0.035, xend = -0.035, y = 0.05, yend = 0.001,
           arrow = arrow(type = "closed", length = unit(0.2, "cm")), color = "white", size = 1) +
  annotate("text", x = -0.035, y = 0.075, 
           label = bquote(t ~ value ~ "for" ~ beta[3]), color = "white", hjust = 0.5, size = 8) +
  sbs_theme()
p1
ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "T_b3.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)




# AIC ---------------------------------------------------------------------

df <- data.frame(
  age = 0:4
)

df$height <- rnorm(n = 5, mean = 50 + 15 * df$age, sd = 5)

p <- ggplot(df, aes(x = age, y = height)) +
  geom_point(size = 5) +
  labs(x = "Impala age",
       y = "Height (cm)") +
  sbs_theme()

ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "fitting_data.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)


m1 <- lm(height ~ factor(age), data = df)
df$fit <- predict(m1)

p <- ggplot(df) +
  geom_point(size = 5, aes(x = age, y = height)) +
  geom_point(aes(x = age, y = fit), colour = "red") +
  labs(x = "Impala age",
       y = "Height (cm)") +
  sbs_theme()

ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "overfitting_data.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)

m1 <- lm(height ~ 1, data = df)
df$fit <- predict(m1)

p <- ggplot(df) +
  geom_point(size = 5, aes(x = age, y = height)) +
  geom_line(aes(x = age, y = fit), colour = "red") +
  labs(x = "Impala age",
       y = "Height (cm)") +
  sbs_theme()

ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "underfitting_data.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)

m1 <- lm(height ~ age, data = df)
df$fit <- predict(m1)

p <- ggplot(df) +
  geom_point(size = 5, aes(x = age, y = height)) +
  geom_line(aes(x = age, y = fit), colour = "red") +
  labs(x = "Impala age",
       y = "Height (cm)") +
  sbs_theme()

ggsave(here::here("Figures/Lecture 11 - NHST & AIC", file = "parsi_fitting_data.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)






N <- 50

df <- data.frame(
  age = runif(n = N, 0, 4)
)
df$height <- rnorm(n = N, mean = 50 + 15 * df$age, sd = 10)

set.seed(88)
df <- df %>%
  mutate(age_bin = cut(age, breaks = quantile(age, probs = seq(0, 1, 0.2)), include.lowest = TRUE))

calculate_likelihood <- function(observed, mean, sd = 10) {
  dnorm(observed, mean = mean, sd = sd)
}

df1 <- data.frame(
  intercept = c(30, 60, 75, 45, 55, 50),
  slope = c(0, 30, -18, 26, 8, 15),
  grp = factor(1:6)
)

combined_df <- bind_rows(
  lapply(1:nrow(df1), function(i) {
    temp_df <- df
    temp_df$line <- df1$intercept[i] + df1$slope[i] * temp_df$age
    temp_df$grp <- df1$grp[i]
    temp_df$likelihood <- calculate_likelihood(temp_df$height, temp_df$line)
    temp_df$fit_mean <- df1$intercept[i] + df1$slope[i] * temp_df$age
    temp_df$intercept <- df1$intercept[i]
    temp_df$slope <- df1$slope[i]
    return(temp_df)
  })
)

density_df <- bind_rows(
  lapply(unique(combined_df$grp), function(g) {
    temp_df <- combined_df %>% filter(grp == g)
    x_vals <- seq(40, 130, length.out = 75)
    densities <- dnorm(x_vals, mean = mean(temp_df$fit_mean), sd = 10)
    data.frame(x = x_vals, y = densities, grp = g)
  })
)


df1 <- df1 %>%
  mutate(subtitle_text = paste0("beta[0] == ", round(intercept, 2), ", ", "beta[1] == ", round(slope, 2)))

combined_df <- combined_df %>%
  left_join(df1, by = "grp")

a2 <- ggplot() +
  geom_histogram(data = combined_df, aes(x = height, y = ..density..), alpha = 0.5, position = "identity") +
  geom_line(data = density_df, aes(x = x, y = y), size = 1.5) +
  labs(x = "Height (cm)",
       y = "",
       subtitle = '{paste("mu =", round(df1[df1$grp == closest_state, "intercept"], 2), "+ ", round(df1[df1$grp == closest_state, "slope"], 2), "* Age")}'
       ) +
  sbs_theme() +
  transition_states(grp) + 
  ease_aes("cubic-in")
a2

anim_save(here::here("Figures/Lecture 11 - NHST & AIC", file = "correct_fit_likelihood.gif"), animation = animate(a2, width = 650, height = 775))





# Underfit ----------------------------------------------------------------

df1 <- data.frame(
  intercept = c(30, 60, 75, 45, 55, 50),
  grp = factor(1:6)
)

combined_df <- bind_rows(
  lapply(1:nrow(df1), function(i) {
    temp_df <- df
    temp_df$line <- df1$intercept[i]
    temp_df$grp <- df1$grp[i]
    temp_df$likelihood <- calculate_likelihood(temp_df$height, temp_df$line)
    temp_df$fit_mean <- df1$intercept[i]
    temp_df$intercept <- df1$intercept[i]
    return(temp_df)
  })
)

density_df <- bind_rows(
  lapply(unique(combined_df$grp), function(g) {
    temp_df <- combined_df %>% filter(grp == g)
    x_vals <- seq(40, 130, length.out = 75)
    densities <- dnorm(x_vals, mean = mean(temp_df$fit_mean), sd = 10)
    data.frame(x = x_vals, y = densities, grp = g)
  })
)


df1 <- df1 %>%
  mutate(subtitle_text = paste0("beta[0] == ", round(intercept, 2)))

combined_df <- combined_df %>%
  left_join(df1, by = "grp")

a3 <- ggplot() +
  geom_histogram(data = combined_df, aes(x = height, y = ..density..), alpha = 0.5, position = "identity") +
  geom_line(data = density_df, aes(x = x, y = y), size = 1.5) +
  labs(x = "Height (cm)",
       y = "",
       subtitle = '{paste("mu =", round(df1[df1$grp == closest_state, "intercept"], 2))}'
  ) +
  sbs_theme() +
  transition_states(grp) + 
  ease_aes("cubic-in")
a3

anim_save(here::here("Figures/Lecture 11 - NHST & AIC", file = "underfit_likelihood.gif"), animation = animate(a3, width = 650, height = 775))


# Overfit -----------------------------------------------------------------





params_df <- data.frame(
  grp = 1:4, 
  b0 = c(50, 60, 70, 80),
  b1 = c(10, 15, 20, 25),
  b2 = c(-5, -10, -15, -20),
  b3 = c(5, 10, 15, 20),
  b4 = c(30, 35, 40, 45)
)

# Define binary indicators for age levels (assuming age1, age2, age3, age4 are derived from 'age')
df <- df %>%
  mutate(
    age1 = ifelse(age == 1, 1, 0),
    age2 = ifelse(age == 2, 1, 0),
    age3 = ifelse(age == 3, 1, 0),
    age4 = ifelse(age == 4, 1, 0)
  )

# Combine datasets based on parameter values and calculate likelihood
combined_df <- bind_rows(
  lapply(1:nrow(params_df), function(i) {
    temp_df <- df
    temp_df$grp <- i
    temp_df$mu <- with(params_df[i, ],
                       b0 + b1 * temp_df$age1 + b2 * temp_df$age2 + b3 * temp_df$age3 + b4 * temp_df$age4)
    temp_df$likelihood <- dnorm(temp_df$height, mean = temp_df$mu, sd = 10)
    temp_df
  })
)

# Create density data
density_df <- bind_rows(
  lapply(unique(combined_df$grp), function(g) {
    temp_df <- combined_df %>% filter(grp == g)
    x_vals <- seq(min(temp_df$height), max(temp_df$height), length.out = 75)
    densities <- dnorm(x_vals, mean = mean(temp_df$mu), sd = 10)
    data.frame(x = x_vals, y = densities, grp = g)
  })
)

# Prepare the subtitle text for each set of parameters
params_df <- params_df %>%
  mutate(subtitle_text = paste0("beta[0] = ", round(b0, 2), 
                                ", beta[1] = ", round(b1, 2), 
                                ", beta[2] = ", round(b2, 2), 
                                ", beta[3] = ", round(b3, 2), 
                                ", beta[4] = ", round(b4, 2)))

combined_df <- combined_df %>%
  left_join(params_df, by = "grp")

# Create the animated plot
a4 <- ggplot() +
  geom_histogram(data = combined_df, aes(x = height, y = ..density..), alpha = 0.5, position = "identity") +
  geom_line(data = density_df, aes(x = x, y = y), size = 1.5) +
  labs(x = "Height (cm)",
       y = "",
       subtitle = '{paste("mu =", round(params_df[params_df$grp == closest_state, "b0"], 2), "+", 
                   round(params_df[params_df$grp == closest_state, "b1"], 2), "* A1 +", 
                   round(params_df[params_df$grp == closest_state, "b2"], 2), "* A2 +", 
                   round(params_df[params_df$grp == closest_state, "b3"], 2), "* A3 +", 
                   round(params_df[params_df$grp == closest_state, "b4"], 2), "* A4")}'
  ) +
  sbs_theme() +
  transition_states(grp) + 
  ease_aes("cubic-in")

# Render the plot
a4

anim_save(here::here("Figures/Lecture 11 - NHST & AIC", file = "overfit_likelihood.gif"), animation = animate(a4, width = 650, height = 775))
