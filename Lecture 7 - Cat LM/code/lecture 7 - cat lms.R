# lecture 7 - categorical variables
# Using horn length with hunting as example (data from Marco paper)

library(ggplot2)
library(here)
library(patchwork)
library(gganimate)
library(dplyr)
source("sbs_theme.R")
source("sbsvoid_theme.R")


# Category example --------------------------------------------------------

df <- data.frame(exper = rep(c("Control", "Treatment"), each = 25))
df$treat <- ifelse(df$exper == "Treatment", 1, 0)
b0 <- 5
b1 <- 2.5
df$dna <- rnorm(n = nrow(df), mean = b0 + b1 * df$treat, sd = 1.8)

p1 <- ggplot(df) +
  geom_jitter(aes(x = exper, y = dna),
              width = 0.1) +
  labs(x = "Experimental group",
       y = "DNA in counterfeit banknote (mm)") +
  sbs_theme()

ggsave(here("Lecture 7 - Cat LM/data/figures", file = "bank_notes.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

p2 <- ggplot(df) +
  geom_jitter(aes(x = exper, y = dna),
              width = 0.1) +
  geom_abline(intercept = 3, slope = 1.2, colour = "white") +
  labs(x = "Experimental group",
       y = "DNA in counterfeit banknote (mm)") +
  sbs_theme()
p2
ggsave(here("Lecture 7 - Cat LM/data/figures", file = "bank_notes.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)






# Sheep data --------------------------------------------------------------

df <- read.table(header = TRUE, sep = "\t", here("Lecture 7 - Cat LM/data", file = "sheep.txt"))

p1 <- ggplot(dat) +
  geom_jitter(aes(x = region, y = horn),
              height = 0, width = 0.1, alpha = 0.3) +
  labs(x = "Region",
       y = "Horn length (mm)") +
  sbs_theme()

ggsave(here("Lecture 7 - Cat LM/figures", file = "horn_desc.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)


m1 <- lm(horn ~ region, data = df)
summary(m1)

plot(ggeffects::ggpredict(m1))


























set.seed(1988)

df1 <- data.frame(
  b0 = c(350, 300, 340, 310, 322, 322),
  b1 = c(-20, 10, -20, 16, -0.3, 9),
  grp = factor(1:6)
)

df <- read.table(header = TRUE, sep = "\t", here("Lecture 7 - Cat LM/data", file = "sheep.txt"))

calculate_likelihood <- function(observed, mean, sd = 5) {
  dnorm(observed, mean = mean, sd = sd)
}

df$skeena <- ifelse(df$region == "Skeena", 0, 1)

df <- df %>%
  group_by(region) %>%
  mutate(
    row_num = row_number(),
    max_row_num = n(),
    jitter_value = runif(n(), -0.2, 0.2)  # Generate jitter within the range -0.2 to 0.2
  ) %>%
  ungroup()

combined_df <- bind_rows(
  lapply(1:nrow(df1), function(i) {
    temp_df <- df
    temp_df$pred <- df1$b0[i] + df1$b1[i] * temp_df$skeena
    temp_df$grp <- df1$grp[i]
    temp_df$fit_mean <- df1$b0[i] + df1$b1[i] * temp_df$skeena
    temp_df$b0 <- df1$b0[i]
    temp_df$b1 <- df1$b1[i]
    temp_df
  })
)

# Map regions to numeric values using ifelse
combined_df$region_num <- ifelse(combined_df$region == "Skeena", 1, 2)
combined_df <- combined_df %>%
  mutate(jittered_x = region_num + jitter_value)


a1 <- ggplot(combined_df, aes(x = jittered_x, y = horn, group = grp)) +
  geom_point(size = 2) +
  geom_segment(aes(x = jittered_x, xend = jittered_x, y = horn, yend = fit_mean), 
               linetype = "dashed", color = "white") +
  geom_point(aes(x = region_num, y = fit_mean), 
               size = 10, show.legend = FALSE, colour = "red") +
  geom_segment(aes(x = region_num - 0.2, xend = region_num + 0.2, 
                   y = fit_mean, yend = fit_mean), 
               size = 1.5, show.legend = FALSE, colour = "red") +
  labs(x = "Region", 
       y = "Horn length (mm)",
       subtitle = '{paste("mu =", round(df1[df1$grp == closest_state, "b0"], 2), "+ ", round(df1[df1$grp == closest_state, "b1"], 2), "* Skeena")}') +
  sbs_theme() +
  scale_x_continuous(breaks = 1:2, labels = c("Peace", "Skeena")) +
  transition_states(grp) +
  ease_aes("sine-in-out")

a1
anim_save(here("Lecture 7 - Cat LM/figures", file = "model_fit_resid.gif"), animation = a1)



density_df <- bind_rows(
  lapply(unique(combined_df$grp), function(g) {
    temp_df <- combined_df %>% filter(grp == g)
    # Use the predicted fit_mean values for density calculations
    densities <- dnorm(temp_df$horn, mean = mean(temp_df$fit_mean), sd = 18)
    data.frame(x = temp_df$horn, y = densities, grp = g)
  })
)

df1 <- df1 %>%
  mutate(subtitle_text = paste0("b0 == ", round(b0, 2), ", ", "b1 == ", round(b1, 2)))

combined_df <- combined_df %>%
  left_join(df1, by = "grp")

a2 <- ggplot() +
  # Plot histogram of the actual data with densities
  geom_histogram(data = combined_df, aes(x = horn, y = ..density..), binwidth = 2, alpha = 0.5, position = "identity") +
  # Plot density lines
  geom_line(data = density_df, aes(x = x, y = y), size = 1.5) +
  scale_x_continuous(limits = c(min(combined_df$horn), max(combined_df$horn))) +
  labs(x = "Horn length (mm)", 
       y = "Density",
       subtitle = '{paste("mu =", round(df1[df1$grp == closest_state, "b0"], 2), "+ ", round(df1[df1$grp == closest_state, "b1"], 2), "* Skeena")}') +
  sbs_theme() +
  transition_states(grp) + 
  ease_aes("cubic-in")

anim_save(here("Lecture 7 - Cat LM/figures", file = "dnorm_vs_data.gif"), animation = a2)


# EDA ---------------------------------------------------------------------

p2 <- ggplot(df) +
  geom_point(aes(x = cohort, y = horn)) +
  labs(x = "Cohort",
       y = "Horn length (mm)") +
  sbs_theme()

ggsave(here("Lecture 7 - Cat LM/figures", file = "horn_desc2.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)

p3 <- ggplot(df) +
  geom_jitter(aes(x = region, y = horn),
              height = 0, width = 0.1) +
  labs(x = "Region",
       y = "Horn length (mm)") +
  sbs_theme()
p3

p4 <- ggplot(df) +
  geom_boxplot(aes(x = region, y = horn)) +
  geom_jitter(aes(x = region, y = horn),
              height = 0, width = 0.1) +
  labs(x = "Region",
       y = "Horn length (mm)") +
  sbs_theme()
p4
ggsave(here("Lecture 7 - Cat LM/figures", file = "horn_desc4.png"), plot = p4, width = 650/72, height = 775/72, dpi = 72)


# Visual contrast treatment -----------------------------------------------

df <- data.frame(
  region = c("Peace", "Skeena"),
  est = c(100, 300)
)

p5 <- ggplot(df) +
  geom_point(aes(x = region, y = est)) +
  labs(x = "Region",
       y = "Horn length (mm)") +
  scale_y_continuous(limits = c(0, NA)) +
  sbs_theme()

ggsave(here("Lecture 7 - Cat LM/figures", file = "contrast.png"), plot = p5, width = 650/72, height = 775/72, dpi = 72)


df <- data.frame(
  region = c("Peace", "Skeena", "Overhear", "Somewhere", "Hither"),
  est = c(100, 300, 150, 300, 280)
)
df$region <- factor(df$region, levels = c("Peace", "Skeena", "Overhear", "Somewhere", "Hither"))
p6 <- ggplot(df) +
  geom_point(aes(x = region, y = est)) +
  labs(x = "Region",
       y = "Horn length (mm)") +
  scale_y_continuous(limits = c(0, NA)) +
  sbs_theme()
p6
ggsave(here("Lecture 7 - Cat LM/figures", file = "multi_contrast.png"), plot = p6, width = 650/72, height = 775/72, dpi = 72)

