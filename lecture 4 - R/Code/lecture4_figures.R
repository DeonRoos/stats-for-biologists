library(ggplot2)
library(here)
library(patchwork)
library(mgcv)
source("sbs_theme.R")

df <- gamSim(eg = 1, n = 373, dist = "normal")

p <- ggplot(df, aes(x = x2, y = y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, k = 50, bs = "tp"),
              colour = "white") +
  labs(y = "Growth rate",
       x = "Temperature (scaled)") +
  sbs_theme()
p
ggsave(here("Lecture 4 - R/Figures", file = "gam.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)

m1 <- gam(y ~ s(x2, k = 50), data = df, method = "REML")
gam.check(m1)


lp_est <- function(info1, info2, info3) {
  N = info1 * info2 / info3
  return(N)
}

lp_est(a = n1, info2 = n2, info3 = m2)

n1 <- 20
n2 <- 30
m2 <- 10
