df <- read.csv("C:/006-BI3010/q1q2.csv", header = TRUE)
df$failq1 <- ifelse(df$Q1 < 9, "Fail", "Pass")
df$failq2 <- ifelse(df$Q2 < 9, "Fail", "Pass")
df$diff <- df$Q2 - df$Q1
library(ggplot2)
library(patchwork)

p1 <- ggplot(df) +
  geom_histogram(aes(x = Q1, fill = failq1), colour = "white", binwidth = 1, bins = 22) +
  scale_fill_brewer(palette = "Dark2") +
  labs(fill = "Pass") +
  geom_vline(xintercept = median(df$Q1), linetype = 1, size = 2) +
  geom_vline(xintercept = quantile(df$Q1, prob = 0.25), linetype = 2, size = 1) +
  geom_vline(xintercept = quantile(df$Q1, prob = 0.75), linetype = 2, size = 1) +
  scale_x_continuous(limits = c(-2,24)) +
  theme_bw()

p2 <- ggplot(df) +
  geom_histogram(aes(x = Q2, fill = failq2), colour = "white", binwidth = 1, bins = 22) +
  scale_fill_brewer(palette = "Dark2") +
  labs(fill = "Pass") +
  geom_vline(xintercept = median(df$Q2), linetype = 1, size = 2) +
  geom_vline(xintercept = quantile(df$Q2, prob = 0.25), linetype = 2, size = 1) +
  geom_vline(xintercept = quantile(df$Q2, prob = 0.75), linetype = 2, size = 1) +
  scale_x_continuous(limits = c(-2,24)) +
  theme_bw()

p1 / p2

ggplot(df) +
  geom_histogram(aes(x = diff), fill = "#8814EB", colour = "white", binwidth = 1, bins = 22) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Change in grade (Q2 - Q1)",
       y = "Grade",
       caption = "Large grade increases are from\nstudents who did not take Q1") +
  geom_vline(xintercept = median(df$diff), linetype = 1, size = 2) +
  geom_vline(xintercept = quantile(df$diff, prob = 0.25), linetype = 2, size = 1) +
  geom_vline(xintercept = quantile(df$diff, prob = 0.75), linetype = 2, size = 1) +
  theme_bw()

df[df$diff > 5,]
