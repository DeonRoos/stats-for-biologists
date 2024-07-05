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

df1$votes <- c(13, 1, 8, 52, 36, 19)

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

combined_df <- bind_rows(
  lapply(1:nrow(df1), function(i) {
    temp_df <- sub_df
    temp_df$line <- df1$intercept[i] + df1$slope[i] * temp_df$pm
    temp_df$grp <- df1$grp[i]
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




m1 <- lm(tawny ~ pm, data = df)
summary(m1)
plot(m1, ask = F)


# Plot with sigma ---------------------------------------------------------
breaks <- seq(0, max(df$pm), len = 5)
df$section <- cut(df$pm, breaks, include.lowest = TRUE)

## Get the residuals
df$res <- residuals(m1)

break_size <- 25

## Compute densities for each section, and flip the axes, and add means of sections
## Note: the densities need to be scaled in relation to the section size (2000 here)
dens <- do.call(rbind, lapply(split(df, df$section), function(x) {
  if (nrow(x) == 0) return(NULL)
  
  d <- density(x$res, n = 50)
  xs <- seq(min(x$res), max(x$res), length.out = 50)
  
  # Create the expected data only
  res <- data.frame(
    y = xs + mean(x$tawny, na.rm = TRUE),
    x = max(x$pm, na.rm = TRUE) - break_size * dnorm(xs, 0, sd(x$res, na.rm = TRUE))
  )
  
  res <- res[order(res$y), ]
  res
}))

dens$section <- rep(levels(df$section), each = 50)

ggplot(df, aes(pm, tawny)) +
  geom_point(colour = "white", size = 0.2, alpha = 0.4) +
  geom_smooth(method = "lm", fill = NA, lwd = 1, colour = "white") +
  geom_path(data = dens, aes(x, y, group = section), lwd = 1, colour = "yellow") +
  labs(x = "Pine marten density",
       y = "Tawny owl density")
