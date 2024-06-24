# Script for lecture 1 of BI3010

# library(here)
# library(ggplot2)
# source("sbs_theme.R")

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
