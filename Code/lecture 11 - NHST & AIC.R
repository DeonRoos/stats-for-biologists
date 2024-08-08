
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
