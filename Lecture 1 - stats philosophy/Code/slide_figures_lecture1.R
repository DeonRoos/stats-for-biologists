# Script for lecture 1 of BI3010

library(here)
library(ggplot2)
source("sbs_theme.R")

set.seed(3031) #3025
xmin <- 1
xmax <- 12
df <- data.frame(
  x = rep(xmin:xmax, times = 2) + 2010
)
df$site = rep(c("Protected", "Not protected"), each = nrow(df)/2)
df$y <- rpois(nrow(df), lambda = 20)

p <- ggplot(df, aes(x = x, y = y, group = site, colour = site)) +
  geom_line(alpha = 0.8) +
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Year",
       y = "Number of gorillas",
       colour = "Site") +
  sbs_theme()
p

ggsave(here("Lecture 1 - stats philosophy/Figures", file = "but_ma_data.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)

# Why statistics course?

df <- expand.grid(
  time = c("Before", "After"),
  method = c("Control", "Treatment")
)

df$after <- ifelse(df$time == "After", 1, 0)
df$treat <- ifelse(df$method == "Treatment", 1, 0)
b0 <- -0    # before method in no conservation
b1 <- -0.5 # after method in no conservation
b2 <- 0     # before method in conservation
b3 <- 0.35  # after method in conservation

df$mu <- b0 + b1 * df$after + b2 * df$treat + b3 * df$after * df$treat
df$growth <- rnorm(n = nrow(df), mean = df$mu, sd = 0.01)

df$time2 <- ifelse(df$time == "Before", "Before conservation", "After conservation")
df$time2 <- factor(df$time2, levels = c("Before conservation", "After conservation"))
df$method2 <- ifelse(df$method == "Control", "Do nothing", "Do conservation")

p1 <- ggplot(df[df$method2 == "Do conservation",], aes(x = time2, y = mu, colour = method2)) +
  geom_path(aes(group = method2), size = 0.5, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = mu - runif(2, 0.02, 0.08), ymax = mu + runif(2, 0.02, 0.08)),
                width = 0.05, position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "",
       y = "Gorilla population\ngrowth rate",
       colour = "Treatment") +
  sbs_theme()
p1

ggsave(here("Lecture 1 - stats philosophy/Figures", file = "ba_gorilla.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)


p2 <- ggplot(df, aes(x = time2, y = mu, colour = method2)) +
  geom_path(aes(group = method2), size = 0.5, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = mu - runif(4, 0.02, 0.08), ymax = mu + runif(4, 0.02, 0.08)),
                width = 0.05, position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "",
       y = "Gorilla population\ngrowth rate",
       colour = "Treatment") +
  sbs_theme()
p2
ggsave(here("Lecture 1 - stats philosophy/Figures", file = "baci_gorilla.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)


# Bias example ------------------------------------------------------------

df <- data.frame(
  cream = c("Cream administered", "No cream administered"),
  decrease = c(223, 107),
  total = c(298, 128)
)
df$cream <- factor(df$cream, levels = c("No cream administered", "Cream administered"))
p3 <- ggplot(df, aes(x = cream, y = decrease/total)) +
  geom_point() +
  labs(x = "Arthitis cream",
       y = "Percent of patients\nwhose sypmtoms decreased"
       ) +
  scale_y_continuous(limits = c(0.5, 1), labels = scales::percent) +
  sbs_theme()
p3
ggsave(here("Lecture 1 - stats philosophy/Figures", file = "cream.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)

df <- data.frame(
  gun = c("Banned concealed guns", "Allow concealed guns"),
  decrease = c(223, 107),
  total = c(298, 128)
)

p4 <- ggplot(df, aes(x = gun, y = decrease/total)) +
  geom_point() +
  labs(x = "Concealed gun policy",
       y = "Percent of states with\ndecrease in crime"
  ) +
  scale_y_continuous(limits = c(0.5, 1), labels = scales::percent) +
  sbs_theme()
p4
ggsave(here("Lecture 1 - stats philosophy/Figures", file = "gun.png"), plot = p4, width = 650/72, height = 775/72, dpi = 72)
