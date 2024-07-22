# lecture 7 - categorical variables
# Using horn length with hunting as example (data from Marco paper)

library(ggplot2)
library(here)
library(patchwork)
library(gganimate)
library(dplyr)
source("sbs_theme.R")
source("sbsvoid_theme.R")

dat <- read.table(header = TRUE, sep = "\t", here("Lecture 7 - Cat LM/data", file = "sheep.txt"))

p1 <- ggplot(dat) +
  geom_jitter(aes(x = region, y = horn),
              height = 0, width = 0.1, alpha = 0.3) +
  labs(x = "Region",
       y = "Horn length (mm)") +
  sbs_theme()

ggsave(here("Lecture 7 - Cat LM/figures", file = "horn_desc.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)


m1 <- lm(horn ~ region, data = dat)
summary(m1)

plot(ggeffects::ggpredict(m1))
