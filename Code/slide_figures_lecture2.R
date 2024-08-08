library(ggplot2)
library(here)
library(patchwork)
source("sbs_theme.R")

# Uniform -----------------------------------------------------------------

df <- data.frame(x = 0:100)
df$L <- dunif(df$x, min = 0, max = 100)
realisation <- data.frame(x = runif(n = nrow(df), min = 0, max = 100))

p <- ggplot(df) +
  geom_line(aes(x = x, y = L*100)) +
  labs(x = "Your randomly generated number",
       y = "Fequency of values\n(if I asked 100 of you)") +
  sbs_theme()

ggsave(here("Lecture 2 - lm overview/Figures", file = "dunif.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)

p1 <- ggplot(df) +
  geom_histogram(data = realisation, aes(x = x, y = ..density.. * 100), fill = "#72758d", alpha = 0.5) +
  geom_line(aes(x = x, y = L*100)) +
  labs(x = "Your randomly generated number",
       y = "Fequency of values\n(if I asked 100 of you)") +
  sbs_theme()

ggsave(here("Lecture 2 - lm overview/Figures", file = "runif.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

realisation <- data.frame(x = rnorm(n = nrow(df), mean = 62.4, sd = 10.1))

p2 <- ggplot(df) +
  geom_histogram(data = realisation, aes(x = x, y = ..density.. * 100), fill = "#72758d", alpha = 0.5) +
  geom_line(aes(x = x, y = L*100)) +
  labs(x = "Average length of impala horns from 100 impalas",
       y = "Fequency of values") +
  sbs_theme()

ggsave(here("Lecture 2 - lm overview/Figures", file = "rnorm.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)

df <- data.frame(x = 0:100)
df$L <- dnorm(df$x, mean = 22, sd = 5.1)

p1 <- ggplot(df) +
  geom_histogram(data = realisation, aes(x = x, y = ..density.. * 100), fill = "#72758d", alpha = 0.5) +
  geom_line(aes(x = x, y = L*100)) +
  labs(x = "Average length of impala horns from 100 impalas",
       title = expression(paste("Normal(", mu, "= 22, ", sigma, "= 5.1)")),
       y = "Fequency of values") +
  sbs_theme()
p1
df <- data.frame(x = 0:100)
df$L <- dnorm(df$x, mean = 78, sd = 15.8)

p2 <- ggplot(df) +
  geom_histogram(data = realisation, aes(x = x, y = ..density.. * 100), fill = "#72758d", alpha = 0.5) +
  geom_line(aes(x = x, y = L*100)) +
  labs(x = "Average length of impala horns from 100 impalas",
       title = expression(paste("Normal(", mu, "= 78, ", sigma, "= 15.8)")),
       y = "Fequency of values") +
  sbs_theme()
p2

df <- data.frame(x = 0:100)
df$L <- dnorm(df$x, mean = 62.4, sd = 10.1)

p3 <- ggplot(df) +
  geom_histogram(data = realisation, aes(x = x, y = ..density.. * 100), fill = "#72758d", alpha = 0.5) +
  geom_line(aes(x = x, y = L*100)) +
  labs(x = "Average length of impala horns from 100 impalas",
       title = expression(paste("Normal(", mu, "= 62.4, ", sigma, "= 10.1)")),
       y = "Fequency of values") +
  sbs_theme()

p1 
ggsave(here("Lecture 2 - lm overview/Figures", file = "impala_small.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)
p2 
ggsave(here("Lecture 2 - lm overview/Figures", file = "impala_big.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)
p3
ggsave(here("Lecture 2 - lm overview/Figures", file = "impala_correct.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)



# Deon height -------------------------------------------------------------

b0 <- 50
b1 <- 15

df <- data.frame(
  age = 1:5
)

df$height <- b0 + b1 * df$age

p <- ggplot(df, aes(x = age, y = height)) +
  geom_point() +
  labs(x = "Deon's age",
       y = "Deon's height") +
  scale_x_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 140)) +
  sbs_theme()

ggsave(here("Lecture 2 - lm overview/Figures", file = "deon_height.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)

p1 <- ggplot(df, aes(x = age, y = height)) +
  geom_point() +
  geom_abline(intercept = b0, slope = b1, colour = "white") +
  labs(x = "Deon's age",
       y = "Deon's height") +
  scale_x_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 140)) +
  sbs_theme()

ggsave(here("Lecture 2 - lm overview/Figures", file = "deon_height_line.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

df <- expand.grid(
  age = 1:5,
  baby = 1
)

df$height[df$age == 1] <- rnorm(max(df$baby), 50, 10)

for(i in 1:max(df$baby)) {
  for(j in 2:max(df$age)) {
   df$height[df$baby == i & df$age == j] <- rnorm(1, mean = df$height[df$baby == i & df$age == j-1] + b1, sd = 5)
  }
}

p2 <- ggplot(df, aes(x = age, y = height, colour = factor(baby))) +
  geom_point(show.legend = FALSE) +
  geom_smooth(show.legend = FALSE, method = "lm", se = FALSE) +
  labs(x = "Child's age",
       y = "Child's height") +
  scale_x_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 140)) +
  sbs_theme()

ggsave(here("Lecture 2 - lm overview/Figures", file = "child_height.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)


df <- expand.grid(
  age = 1:5,
  baby = 1:20
)

df$height[df$age == 1] <- rnorm(max(df$baby), 50, 10)

for(i in 1:max(df$baby)) {
  for(j in 2:max(df$age)) {
    df$height[df$baby == i & df$age == j] <- rnorm(1, mean = df$height[df$baby == i & df$age == j-1] + b1, sd = 10)
  }
}

p3 <- ggplot(df, aes(x = age, y = height, colour = factor(baby))) +
  geom_point(show.legend = FALSE) +
  geom_smooth(show.legend = FALSE, method = "lm", se = FALSE) +
  labs(x = "Child's age",
       y = "Child's height") +
  scale_x_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 140)) +
  sbs_theme()
p3
ggsave(here("Lecture 2 - lm overview/Figures", file = "children_height.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)

p4 <- ggplot(df, aes(x = age, y = height)) +
  geom_smooth(show.legend = FALSE, method = "lm", colour = "white", se = TRUE, fill = "lightgrey") +
  geom_point(aes(colour = factor(baby)), show.legend = FALSE) +
  labs(x = "Child's age",
       y = "Child's height") +
  scale_x_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 140)) +
  sbs_theme()
p4
ggsave(here("Lecture 2 - lm overview/Figures", file = "children_height_fit.png"), plot = p4, width = 650/72, height = 775/72, dpi = 72)


df <- expand.grid(
  age = 1:5,
  baby = 1:20
)

df$height[df$age == 1] <- rnorm(max(df$baby), 50, 10)

for(i in 1:max(df$baby)) {
  for(j in 2:max(df$age)) {
    df$height[df$baby == i & df$age == j] <- rnorm(1, mean = df$height[df$baby == i & df$age == j-1] + b1, sd = 10)
  }
}

cfs <- coef(lm(height ~ age, data = df))
sig <- sigma(lm(height ~ age, data = df))

df1 <- data.frame(x = 0:200)
df1$L <- dnorm(df1$x, mean = mean(cfs[1] + cfs[2] * 1:5), sd = sig)

p5 <- ggplot() +
  geom_histogram(data = df, aes(x = height, y = ..density.. * 100), fill = "#72758d", alpha = 0.5) +
  geom_line(data = df1, aes(x = x, y = L*100)) +
  labs(x = "Child height",
       y = "Fequency of values") +
  sbs_theme()

ggsave(here("Lecture 2 - lm overview/Figures", file = "children_height_dist.png"), plot = p5, width = 650/72, height = 775/72, dpi = 72)


# Range of Normal ---------------------------------------------------------

df <- data.frame(x = -100:100)
df$L <- dnorm(df$x, mean = -10, sd = 10)
df$grp <- 1
df1 <- data.frame(x = -100:100)
df1$L <- dnorm(df1$x, mean = -0, sd = 4.5)
df1$grp <- 2
df2 <- data.frame(x = -100:100)
df2$L <- dnorm(df2$x, mean = 25, sd = 25)
df2$grp <- 3
df3 <- data.frame(x = -100:100)
df3$L <- dnorm(df3$x, mean = 64, sd = 8)
df3$grp <- 4
df4 <- data.frame(x = -100:100)
df4$L <- dnorm(df4$x, mean = -58, sd = 12)
df4$grp <- 5

df <- rbind(df, df1, df2, df3, df4)
df$grp <- factor(df$grp)
p1 <- ggplot(df) +
  geom_line(aes(x = x, y = L, group = grp, colour = grp), show.legend = FALSE) +
  geom_area(aes(x = x, y = L, fill = grp), position = position_identity(), alpha = 0.4, show.legend = F) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Value of our response variable",
       y = "Fequency of values") +
  sbs_theme()
p1

ggsave(here("Lecture 2 - lm overview/Figures", file = "normal_dist.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)



# Reworked likelihood example ---------------------------------------------



N <- 50

df <- data.frame(
  age = runif(n = N, 0, 5)
)
df$height <- rnorm(n = N, mean = 36.5 + 14.8 * df$age, sd = 10)

set.seed(88)
df <- df %>%
  mutate(age_bin = cut(age, breaks = quantile(age, probs = seq(0, 1, 0.2)), include.lowest = TRUE))

calculate_likelihood <- function(observed, mean, sd = 10) {
  dnorm(observed, mean = mean, sd = sd)
}

df1 <- data.frame(
  intercept = c(30.2, 35.7, 36.5, 46.1, 54.3, 60.7),
  slope = c(0.1, 5.3, 14.8, 15.2, 16.5, 18.3),
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
    x_vals <- seq(15, 130, length.out = 75)
    densities <- dnorm(x_vals, mean = mean(temp_df$fit_mean), sd = 18)
    data.frame(x = x_vals, y = densities, grp = g)
  })
)


df1 <- df1 %>%
  mutate(subtitle_text = paste0("beta[0] == ", round(intercept, 2), ", ", "beta[1] == ", round(slope, 2)))

combined_df <- combined_df %>%
  left_join(df1, by = "grp")

hist_max_density <- combined_df %>%
  group_by(grp) %>%
  summarise(max_density = max(density(height)$y))

# Merge the max density with the density_df to apply scaling
density_df <- density_df %>%
  left_join(hist_max_density, by = "grp") %>%
  mutate(y_scaled = y / max(y) * 0.025)


a2 <- ggplot() +
  geom_histogram(data = combined_df, aes(x = height, y = ..density..), alpha = 0.5, position = "identity") +
  geom_line(data = density_df, aes(x = x, y = y_scaled), size = 1.5) +
  labs(x = "Child height (cm)",
       y = "",
       subtitle = '{paste("mu =", round(df1[df1$grp == closest_state, "intercept"], 2), "+ ", round(df1[df1$grp == closest_state, "slope"], 2), "* Age")}'
  ) +
  sbs_theme() +
  transition_states(grp) + 
  ease_aes("cubic-in")
a2

anim_save(here::here("Figures/Lecture 2 - lm overview/Figures", file = "tweak_params.gif"), animation = animate(a2, width = 650, height = 775))
