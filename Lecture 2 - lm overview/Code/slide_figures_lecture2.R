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
  scale_y_continuous(limits = c(50, 140)) +
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
#df$height <- rnorm(nrow(df), mean = b0 + b1 * df$age, sd = 10)

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
