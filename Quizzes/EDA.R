grade <- read.csv("H:/003 - Teaching/006-BI3010/Grades/2024_Q1.csv", header = TRUE)
library(ggplot2)

ggplot(grade) +
  geom_histogram(aes(x = score), colour = "white") +
  geom_vline(xintercept = median(grade$score), linetype = 1, colour = "red", size = 2) +
  geom_vline(xintercept = quantile(grade$score, probs = 0.1), linetype = 2, colour = "red", size = 1) +
  geom_vline(xintercept = quantile(grade$score, probs = 0.9), linetype = 2, colour = "red", size = 1) +
  scale_x_continuous(limits = c(0, 22)) +
  theme_minimal()


ggplot(grade) +
  geom_histogram(aes(x = score, fill = Trade), position = position_dodge(), colour = "white") +
  geom_vline(xintercept = median(grade$score[grade$Trade == "No"]), linetype = 2, colour = "red") +
  geom_vline(xintercept = median(grade$score[grade$Trade == "Yes"]), linetype = 2, colour = "blue") +
  scale_x_continuous(limits = c(0, 22)) +
  theme_minimal()

grade[grade$score == min(grade$score),]
grade[grade$score >= quantile(grade$score, probs = 0.9),]
grade[grade$score >= quantile(grade$score[grade$Trade == "Yes"], probs = 0.9) & grade$Trade == "Yes",]
View(grade[grade$score <= quantile(grade$score, probs = 0.1),])
nrow(grade[grade$score < 9,]) / nrow(grade) * 100
