library(ggplot2)
library(here)
library(patchwork)
library(gganimate)
library(dplyr)
source("sbs_theme.R")
source("sbsvoid_theme.R")


# Pine marten tawny dataset -----------------------------------------------

set.seed(666)

df <- expand.grid(
  site = 1:23,
  year = 0:44
)

# PM
b0 <- -3.5
b1 <- 0.08

g0 <- rnorm(23, mean = 0, sd = 0.2)
names(g0) <- 1:23
df$g0 <- g0[df$site]

df$pm_lamb <- exp(b0 + b1 * df$year + df$g0)
df$pm <- rlnorm(nrow(df), meanlog = df$pm_lamb, sdlog = 0.1)

p1 <- ggplot(df) +
  geom_point(aes(x = year, y = pm)) +
  labs(x = "Year",
       y = "Pine marten density (ha)") +
  sbs_theme()

ggsave(here("Lecture 5 - Cont LM/Figures", file = "pm_timeseries.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)
  
  
# Tawny
a0 <- 40
a1 <- -3.5

df$mu <- a0 + a1 * df$pm
df$tawny <- rnorm(nrow(df), mean = df$mu, sd = 5)

p2 <- ggplot(df) +
  geom_point(aes(x = year, y = tawny)) +
  labs(x = "Year",
       y = "Tawny owl density (ha)") +
  sbs_theme()
p2
ggsave(here("Lecture 5 - Cont LM/Figures", file = "tawny_timeseries.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)

p3 <- ggplot(df) +
  geom_point(aes(x = pm, y = tawny)) +
  labs(x = "Pine marten density (ha)",
       y = "Tawny owl density (ha)") +
  sbs_theme()
p3

ggsave(here("Lecture 5 - Cont LM/Figures", file = "tawny_pm.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)


df1 <- data.frame(
  intercept = c(30, 19, 47, 40, 30, 45),
  slope = c(0, 3, -0.5, -3.5, -0.3, -8),
  grp = factor(1:6)
)

a <- ggplot() +
  geom_point(data = df, aes(x = pm, y = tawny)) +
  geom_abline(data = df1, aes(intercept = intercept, slope = slope),
              size = 1.5, colour = "red",
              show.legend = FALSE) +
  labs(x = "Pine marten density (ha)",
       y = "Tawny owl density (ha)") +
  scale_x_continuous(limits = c(0,NA)) +
  sbs_theme() +
  transition_states(grp) + 
  ease_aes("cubic-in")

anim_save(here("Lecture 5 - Cont LM/Figures", file = "model_fit.gif"), animation = a)

df1$grp <- factor(toupper(letters[1:6]))
p4 <- ggplot() +
  geom_point(data = df, aes(x = pm, y = tawny), size = 1) +
  geom_abline(data = df1, aes(intercept = intercept, slope = slope),
              size = 1.5, colour = "red",
              show.legend = FALSE) +
  labs(x = "Pine marten density (ha)",
       y = "Tawny owl density (ha)") +
  scale_x_continuous(limits = c(0,NA)) +
  sbs_theme() +
  facet_wrap(~grp)
p4
ggsave(here("Lecture 5 - Cont LM/Figures", file = "model_fits.png"), plot = p4, width = 1400/72, height = 650/72, dpi = 72)

df1$votes <- c(13, 1, 8, 52, 19, 36)

p5 <- ggplot(df1) +
  geom_col(aes(x = grp, y = votes)) +
  labs(x = "Line",
       y = "Number of votes") +
  sbs_theme()

design <- "
AAAB
AAAB
"  
p6 <- p4 + p5 + plot_layout(design = design)

ggsave(here("Lecture 5 - Cont LM/Figures", file = "subj_likelihood.png"), plot = p6, width = 1400/72, height = 650/72, dpi = 72)

p7 <- ggplot() +
  geom_point(data = df, aes(x = pm, y = tawny)) +
  geom_abline(data = df1[df1$grp == "D",], aes(intercept = intercept, slope = slope),
              size = 1.5, colour = "red",
              show.legend = FALSE) +
  labs(x = "Pine marten density (ha)",
       y = "Tawny owl density (ha)") +
  scale_x_continuous(limits = c(0,NA)) +
  sbs_theme()
p7

ggsave(here("Lecture 5 - Cont LM/Figures", file = "best_fit.png"), plot = p7, width = 650/72, height = 775/72, dpi = 72)


set.seed(88)
df <- df %>%
  mutate(pm_bin = cut(pm, breaks = quantile(pm, probs = seq(0, 1, 0.2)), include.lowest = TRUE))

sub_df <- df %>%
  group_by(pm_bin) %>%
  sample_n(10) %>%
  ungroup()

calculate_likelihood <- function(observed, mean, sd = 5) {
  dnorm(observed, mean = mean, sd = sd)
}

combined_df <- bind_rows(
  lapply(1:nrow(df1), function(i) {
    temp_df <- sub_df
    temp_df$line <- df1$intercept[i] + df1$slope[i] * temp_df$pm
    temp_df$grp <- df1$grp[i]
    temp_df$likelihood <- calculate_likelihood(temp_df$tawny, temp_df$line)
    temp_df$fit_mean <- df1$intercept[i] + df1$slope[i] * temp_df$pm
    temp_df$intercept <- df1$intercept[i]
    temp_df$slope <- df1$slope[i]
    return(temp_df)
  })
)

a1 <- ggplot() +
  geom_point(data = combined_df, aes(x = pm, y = tawny)) +
  geom_abline(data = df1, aes(intercept = intercept, slope = slope),
              size = 1.5, colour = "red",
              show.legend = FALSE) +
  geom_segment(data = combined_df, aes(x = pm, xend = pm, y = tawny, yend = line), 
               linetype = "dashed", colour = "white") +
  labs(x = "Pine marten density (ha)",
       y = "Tawny owl density (ha)") +
  scale_x_continuous(limits = c(0, NA)) +
  sbs_theme() +
  transition_states(grp) + 
  ease_aes("cubic-in")
a1
anim_save(here("Lecture 5 - Cont LM/Figures", file = "model_fit_resid.gif"), animation = a1)



df <- data.frame(
  combo = c('1st Parameters', '2nd Parameters'),
  Likelihood = c(6496159278500, -241593),
  check.names = FALSE
)

p8 <- ggplot(df) +
  geom_point(aes(x = combo, y = Likelihood)) +
  scale_y_continuous(labels = scales::comma) +
  #scale_x_continuous(limits = c(0, 3)) +
  labs(x = "") +
  sbs_theme()

p8
ggsave(here("Lecture 5 - Cont LM/Figures", file = "likeli_compare.png"), plot = p8, width = 650/72, height = 775/72, dpi = 72)



temp_df <- sub_df
temp_df$line <- 5 + 2 * temp_df$pm

p9 <- ggplot(data = temp_df) +
  geom_point(aes(x = pm, y = tawny)) +
  geom_abline(intercept = 5, slope = 2,
              size = 1.5, colour = "red",
              show.legend = FALSE) +
  geom_segment(aes(x = pm, xend = pm, y = tawny, yend = line),
               linetype = "dashed", colour = "white") +
  labs(x = "Pine marten density (ha)",
       y = "Tawny owl density (ha)",
       title = "Intercept = 5, Slope = 2",
       subtitle = "𝐿=6,496,159,278,500") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA)) +
  sbs_theme()

temp_df$line <- 40 + -2 * temp_df$pm

p10 <- ggplot(data = temp_df) +
  geom_point(aes(x = pm, y = tawny)) +
  geom_abline(intercept = 40, slope = -2,
              size = 1.5, colour = "red",
              show.legend = FALSE) +
  geom_segment(aes(x = pm, xend = pm, y = tawny, yend = line),
               linetype = "dashed", colour = "white") +
  labs(x = "Pine marten density (ha)",
       y = "Tawny owl density (ha)",
       title = "Intercept = 40, Slope = -2",
       subtitle = "𝐿=−241,593") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA)) #+
  sbs_theme()

p9 + p10

ggsave(here("Lecture 5 - Cont LM/Figures", file = "subj_likelihood.png"), plot = p6, width = 1400/72, height = 650/72, dpi = 72)


density_df <- bind_rows(
  lapply(unique(combined_df$grp), function(g) {
    temp_df <- combined_df %>% filter(grp == g)
    x_vals <- seq(20, 60, length.out = 75)
    densities <- dnorm(x_vals, mean = mean(temp_df$fit_mean), sd = 5)
    data.frame(x = x_vals, y = densities, grp = g)
  })
)

df1 <- df1 %>%
  mutate(subtitle_text = paste0("beta[0] == ", round(intercept, 2), ", ", "beta[1] == ", round(slope, 2)))

combined_df <- combined_df %>%
  left_join(df1, by = "grp")

a2 <- ggplot() +
  geom_histogram(data = combined_df, aes(x = tawny, y = ..density..), binwidth = 2, alpha = 0.5, position = "identity") +
  geom_line(data = density_df, aes(x = x, y = y), size = 1.5) +
  scale_x_continuous(limits = c(20, 60)) +
  labs(x = "Tawny owl density (ha)", 
       y = "Density",
       subtitle = '{paste("mu =", round(df1[df1$grp == closest_state, "intercept"], 2), "+ ", round(df1[df1$grp == closest_state, "slope"], 2), "* Pine Marten")}') +
  sbs_theme() +
  transition_states(grp) + 
  ease_aes("cubic-in")

anim_save(here("Lecture 5 - Cont LM/Figures", file = "dnorm_vs_data.gif"), animation = a2)

likelihood <- function(intercept, slope, data) {
  pm <- data$pm
  tawny <- data$tawny
  
  # Calculate mu_i
  mu <- intercept + slope * pm
  
  # Calculate negative log-likelihood
  n <- length(tawny)
  nll <- n/2 * log(2 * pi) + n/2 * log(var(tawny - mu)) + sum((tawny - mu)^2) / (2 * var(tawny - mu))
  
  return(nll)
}

likelihood_values <- apply(df1, 1, function(row) {
  intercept <- row["intercept"]
  slope <- row["slope"]
  
  # Convert intercept and slope to numeric if needed
  intercept <- as.numeric(intercept)
  slope <- as.numeric(slope)
  
  # Calculate likelihood for current parameter combination
  likelihood(intercept, slope, combined_df)
})

df1_likelihood <- cbind(df1, likelihood = likelihood_values)
df1_likelihood$parms = paste0("Intercept = ", df1_likelihood$intercept, ",\nSlope = ", df1_likelihood$slope)


p7 <- ggplot(df1_likelihood) +
  geom_point(aes(x = parms, y = likelihood)) +
  labs(x = "",
       y = "Likelihood") +
  sbs_theme() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 0.5))

ggsave(here("Lecture 5 - Cont LM/Figures", file = "mle.png"), plot = p7, width = 650/72, height = 775/72, dpi = 72)


df <- data.frame(
  marten = seq(from = 0, to = 10, by = 0.5)
)

df$tawny <- 40 - 3.5 * df$marten

a3 <- ggplot(df, aes(x = marten, y = tawny)) +
  geom_line() +
  labs(x = "Pine marten",
       y = "Tawny owls") +
  sbs_theme() +
  transition_reveal(marten)
anim_save(here("Lecture 5 - Cont LM/Figures", file = "tawny_pred.gif"), animation = a3)

p8 <- ggplot(df, aes(x = marten, y = tawny)) +
  geom_line() +
  labs(x = "Pine marten",
       y = "Tawny owls") +
  sbs_theme()
ggsave(here("Lecture 5 - Cont LM/Figures", file = "tawny_pred.png"), plot = p8, width = 650/72, height = 775/72, dpi = 72)



# Running the model -------------------------------------------------------

set.seed(666)

df <- expand.grid(
  site = 1:23,
  year = 0:44
)

# PM
b0 <- -3.5
b1 <- 0.08

g0 <- rnorm(23, mean = 0, sd = 0.2)
names(g0) <- 1:23
df$g0 <- g0[df$site]

df$pm_lamb <- exp(b0 + b1 * df$year + df$g0)
df$marten <- rlnorm(nrow(df), meanlog = df$pm_lamb, sdlog = 0.1)

a0 <- 40
a1 <- -3.5

df$mu <- a0 + a1 * df$marten
df$tawny <- rnorm(nrow(df), mean = df$mu, sd = 5)
owl_data <- df


owl_model <- lm(tawny ~ marten,
                data = owl_data)

summary(owl_model)
