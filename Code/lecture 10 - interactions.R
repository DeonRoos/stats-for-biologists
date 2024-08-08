library(ggplot2)
library(gganimate)
library(here)
library(dplyr)

n <- 40

df <- data.frame(
  species = c(rep("Boar", times = n),
              rep("Monkey", times = n),
              rep("Deer", times = n),
              rep("Rat", times = n)),
  density = round(c(seq(length.out = n, 
                        from = 0, 
                        to = 50), 
                    seq(length.out = n, 
                        from = 0, 
                        to = 50), 
                    seq(length.out = n, 
                        from = 0, 
                        to = 50), 
                    seq(length.out = n, 
                        from = 0,
                        to = 50)),
  digits = 1)
)

df

df$monkey <- ifelse(df$species == "Monkey", 1, 0)
df$deer <- ifelse(df$species == "Deer", 1, 0)
df$rat <- ifelse(df$species == "Rat", 1, 0)

b0 <- 2000
b1 <- 0
b2 <- 0
b3 <- 0
b4 <- -15.5
b5 <- 10.25
b6 <- 5.5
b7 <- 13.5

df$yield <- rnorm(n = nrow(df),
                   mean = b0 + 
                     b1 * df$monkey +
                     b2 * df$deer +
                     b3 * df$rat +
                     b4 * df$density +
                     b5 * df$density * df$monkey +
                     b6 * df$density * df$deer +
                     b7 * df$density * df$rat,
                  sd = 150)

m1 <- lm(yield ~ species : density, data = df)

summary(m1)

par(mfrow = c(2,2), bg = "black", col = "white", col.axis = "white", col.lab = "white")
plot(m1)

prds <- predict(m1, se.fit = TRUE)
df$fit <- prds$fit
df$low <- prds$fit - prds$se.fit * 1.96
df$upp <- prds$fit + prds$se.fit * 1.96

p3 <- ggplot(df) +
  geom_point(aes(x = density, y = yield)) +
  geom_ribbon(aes(x = density, ymin = low, ymax = upp, group = species), fill = "white", alpha = 0.5) +
  geom_line(aes(x = density, y = fit, group = species)) +
  labs(x = "Controlled density",
       colour = "Species",
       y = "Yield from field (kg)") +
  facet_wrap(~species) +
  sbs_theme()
p3
ggsave(here::here("Figures/Lecture 10 - Interactions", file = "indian_interaction_fit.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)

p1 <- ggplot(df, aes(x = density, y = yield)) +
  geom_point() +
  labs(x = "Controlled density",
       y = "Yield from field (kg)") +
  sbs_theme()
p1

ggsave(here::here("Figures/Lecture 10 - Interactions", file = "indian_interaction.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

p2 <- ggplot(df, aes(x = density, y = yield, colour = species)) +
  geom_point() +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Controlled density",
       colour = "Species",
       y = "Yield from field (kg)") +
  sbs_theme()
p2

ggsave(here::here("Figures/Lecture 10 - Interactions", file = "indian_interaction_colour.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)

p3 <- ggplot(df, aes(x = density, y = yield)) +
  geom_point() +
  labs(x = "Controlled density",
       colour = "Species",
       y = "Yield from field (kg)") +
  facet_wrap(~species) +
  sbs_theme()
p3

ggsave(here::here("Figures/Lecture 10 - Interactions", file = "indian_interaction_facet.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)


ggplot(df, aes(x = density, y = yield)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~species) +
  labs(x = "Controlled density",
       y = "Yield from field (kg)") +
  sbs_theme()

b0_values <- 2000
b1_values <- 500
b2_values <- -400
b3_values <- 100
b4_values <- -10

# Create a combination of all b0 to b4 values
params <- data.frame(b0 = b0_values, b1 = b1_values, b2 = b2_values, b3 = b3_values, b4 = b4_values)

# Generate data for each combination of b0 to b4
df_expanded <- params %>%
  mutate(param_id = row_number()) %>%  # Create an ID for each parameter set
  group_by(param_id) %>%
  do({
    data <- df
    data$param_id <- .$param_id
    data$addit <- .$b0 +
      .$b1 * data$monkey +
      .$b2 * data$deer +
      .$b3 * data$rat +
      .$b4 * data$density
    data
  }) %>%
  ungroup()

# Plot and animate
p4.1 <- ggplot(df_expanded, aes(x = density, y = addit, group = species, color = species)) +
  geom_line() +
  labs(x = "Controlled density", y = "Yield from field (kg)", colour = "Species") +
  scale_colour_brewer(palette = "Dark2") +
  sbs_theme()
p4.1

ggsave(here::here("Figures/Lecture 10 - Interactions", file = "additive_static.png"), plot = p4.1, width = 650/72, height = 775/72, dpi = 72)


b0_values <- c(2000, 2500, 1900, 2100)
b1_values <- c(500, 600, -300, 400)
b2_values <- c(-400, 200, 100, -500)
b3_values <- c(100, -200, 300, 150)
b4_values <- c(-10, 10, -15, 6)

# Create a combination of all b0 to b4 values
params <- data.frame(b0 = b0_values, b1 = b1_values, b2 = b2_values, b3 = b3_values, b4 = b4_values)

# Generate data for each combination of b0 to b4
df_expanded <- params %>%
  mutate(param_id = row_number()) %>%  # Create an ID for each parameter set
  group_by(param_id) %>%
  do({
    data <- df
    data$param_id <- .$param_id
    data$addit <- .$b0 +
      .$b1 * data$monkey +
      .$b2 * data$deer +
      .$b3 * data$rat +
      .$b4 * data$density
    data
  }) %>%
  ungroup()

# Plot and animate
p4 <- ggplot(df_expanded, aes(x = density, y = addit, group = species, color = species)) +
  geom_line() +
  labs(x = "Controlled density", y = "Yield from field (kg)") +
  scale_colour_brewer(palette = "Dark2") +
  transition_states(param_id, transition_length = 2, state_length = 1) +
  sbs_theme()
p4

anim_save(here::here("Figures/Lecture 10 - Interactions", file = "additive.gif"), animation = animate(p4, width = 650, height = 775))


# Define base coefficient and a specific set of values for b0 to b7
b0_values <- c(2000, 2500, 1900, 2100)
b1_values <- c(500, 600, -300, 400)
b2_values <- c(-400, 200, 100, -500)
b3_values <- c(100, -200, 300, 150)
b4_values <- c(-10, 10, -15, 6)
b5_values <- c(8, -9, -11, 10)
b6_values <- c(-6, 7, 5, -4)
b7_values <- c(14, -13, 12, -15)

# Create a combination of all b0 to b7 values
params <- data.frame(b0 = b0_values, b1 = b1_values, b2 = b2_values, b3 = b3_values, b4 = b4_values, b5 = b5_values, b6 = b6_values, b7 = b7_values)

# Generate data for each combination of b0 to b7
df_expanded <- params %>%
  mutate(param_id = row_number()) %>%  # Create an ID for each parameter set
  group_by(param_id) %>%
  do({
    data <- df
    data$param_id <- .$param_id
    data$addit <- .$b0 +
      .$b1 * data$monkey +
      .$b2 * data$deer +
      .$b3 * data$rat +
      .$b4 * data$density +
      .$b5 * data$density * data$monkey +
      .$b6 * data$density * data$deer +
      .$b7 * data$density * data$rat
    data
  }) %>%
  ungroup()

p5 <- ggplot(df_expanded, aes(x = density, y = addit, group = species, color = species)) +
  geom_line() +
  labs(x = "Controlled density", y = "Yield from field (kg)") +
  scale_colour_brewer(palette = "Dark2") +
  transition_states(param_id, transition_length = 2, state_length = 1) +
  sbs_theme()

anim_save(here::here("Figures/Lecture 10 - Interactions", file = "interaction.gif"), animation = animate(p5, width = 650, height = 775))

# Define base coefficient and a specific set of values for b0 to b7
b0_values <- 2000
b1_values <- 500
b2_values <- -400
b3_values <- 100
b4_values <- -10
b5_values <- 8
b6_values <- -6
b7_values <- 14

# Create a combination of all b0 to b7 values
params <- data.frame(b0 = b0_values, b1 = b1_values, b2 = b2_values, b3 = b3_values, b4 = b4_values, b5 = b5_values, b6 = b6_values, b7 = b7_values)

# Generate data for each combination of b0 to b7
df_expanded <- params %>%
  mutate(param_id = row_number()) %>%  # Create an ID for each parameter set
  group_by(param_id) %>%
  do({
    data <- df
    data$param_id <- .$param_id
    data$addit <- .$b0 +
      .$b1 * data$monkey +
      .$b2 * data$deer +
      .$b3 * data$rat +
      .$b4 * data$density +
      .$b5 * data$density * data$monkey +
      .$b6 * data$density * data$deer +
      .$b7 * data$density * data$rat
    data
  }) %>%
  ungroup()

p5.1 <- ggplot(df_expanded, aes(x = density, y = addit, group = species, color = species)) +
  geom_line() +
  labs(x = "Controlled density", y = "Yield from field (kg)", colour = "Species") +
  scale_colour_brewer(palette = "Dark2") +
  sbs_theme()
p5.1
ggsave(here::here("Figures/Lecture 10 - Interactions", file = "interaction_static.png"), plot = p5.1, width = 650/72, height = 775/72, dpi = 72)

y_start_b1 <- b0_values + b1_values
y_end_b1 <- y_start_b1 - 10 * 50
y_start_b2 <- b0_values + b2_values
y_end_b2 <- y_start_b2 - 10 * 50
y_start_b3 <- b0_values + b3_values
y_end_b3 <- y_start_b3 - 10 * 50

p5.2 <- ggplot(df_expanded, aes(x = density, y = addit, group = species, color = species)) +
  geom_line() +
  geom_segment(aes(x = 0, y = y_start_b1, xend = 50, yend = y_end_b1), colour = "#1B9E77", alpha = 0.5, linetype = 2) +
  geom_segment(aes(x = 0, y = y_start_b2, xend = 50, yend = y_end_b2), colour = "#1B9E77", alpha = 0.5, linetype = 2) +
  geom_segment(aes(x = 0, y = y_start_b3, xend = 50, yend = y_end_b3), colour = "#1B9E77", alpha = 0.5, linetype = 2) +
  labs(x = "Controlled density", y = "Yield from field (kg)", colour = "Species") +
  scale_colour_brewer(palette = "Dark2") +
  sbs_theme()
p5.2
ggsave(here::here("Figures/Lecture 10 - Interactions", file = "interaction_static_slope_contrast.png"), plot = p5.2, width = 650/72, height = 775/72, dpi = 72)
c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")
custom_colors <- c("#1B9E77", "grey30", "grey30", "#E7298A")

p5.3 <- ggplot(df_expanded, aes(x = density, y = addit, group = species, color = species)) +
  geom_line() +
  geom_segment(aes(x = 0, y = y_start_b1, xend = 50, yend = y_end_b1), colour = "grey30", linetype = 2) +
  geom_segment(aes(x = 0, y = y_start_b2, xend = 50, yend = y_end_b2), colour = "grey30", linetype = 2) +
  geom_segment(aes(x = 0, y = y_start_b3, xend = 50, yend = y_end_b3), colour = "#1B9E77", alpha = 0.5, linetype = 2) +
  labs(x = "Controlled density", y = "Yield from field (kg)", colour = "Species") +
  scale_color_manual(values = custom_colors) +
  sbs_theme()
p5.3
ggsave(here::here("Figures/Lecture 10 - Interactions", file = "interaction_static_slope_contrast_rat_focus.png"), plot = p5.3, width = 650/72, height = 775/72, dpi = 72)


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

p1 <- ggplot(df, aes(x = time2, y = mu, colour = method2)) +
  geom_errorbar(aes(ymin = mu - runif(4, 0.02, 0.08), ymax = mu + runif(4, 0.02, 0.08)),
                width = 0.05, position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "",
       y = "Gorilla population\ngrowth rate",
       colour = "Treatment") +
  sbs_theme()

ggsave(here::here("Figures/Lecture 10 - Interactions", file = "2cat_inter.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)



# 2 continuous interaction ------------------------------------------------

b1 <- 200
b2 <- 3
b3 <- -2.5
b4 <- -0.05

df <- expand.grid(
  x1 = 0:100,
  x2 = 10:40
)

df$y <- b1 + b2 * df$x1 + b3 * df$x2 + b4 * df$x1 * df$x2

p2 <- ggplot(df) +
  geom_tile(aes(x = x1, y = x2, fill = y)) +
  scale_fill_viridis_c(option = "magma") +
  labs(x = "Precipitation",
       y = "Temperature",
       fill = "Tree\nheight\n(cm)") +
  sbs_theme()
p2
ggsave(here::here("Figures/Lecture 10 - Interactions", file = "2cont_inter.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)

df <- expand.grid(
  x1 = 0:100,
  x2 = 10:40,
  x3 = c(0,1)
)

b1 <- 200
b2 <- 3
b3 <- -2.5
b4 <- -0.05
b5 <- 50
b6 <- -0.5
b7 <- 1
b8 <- 0.02

df$y <- b1 + 
  b2 * df$x1 + 
  b3 * df$x2 + 
  b4 * df$x1 * df$x2 +
  b5 * df$x3 +
  b6 * df$x1 * df$x3 + 
  b7 * df$x2 * df$x3 + 
  b8 * df$x1 * df$x2 * df$x3

df$climate <- ifelse(df$x3 == 1, "Tropical", "Tundra")

p3 <- ggplot(df) +
  geom_tile(aes(x = x1, y = x2, fill = y)) +
  scale_fill_viridis_c(option = "magma") +
  labs(x = "Precipitation",
       y = "Temperature",
       fill = "Tree\nheight\n(cm)") +
  facet_wrap(~climate, ncol = 1) +
  sbs_theme()
p3
ggsave(here::here("Figures/Lecture 10 - Interactions", file = "3way_inter.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)







# Realistic design --------------------------------------------------------

n <- 40

df <- data.frame(
  species = c(rep("Boar", times = n),
              rep("Monkey", times = n),
              rep("Deer", times = n),
              rep("Rat", times = n)),
  density = round(c(seq(length.out = n, 
                        from = 0, 
                        to = 15), 
                    seq(length.out = n, 
                        from = 0, 
                        to = 30), 
                    seq(length.out = n, 
                        from = 0, 
                        to = 50), 
                    seq(length.out = n, 
                        from = 0,
                        to = 800)),
                  digits = 1)
)

df

df$monkey <- ifelse(df$species == "Monkey", 1, 0)
df$deer <- ifelse(df$species == "Deer", 1, 0)
df$rat <- ifelse(df$species == "Rat", 1, 0)

b0 <- 2000
b1 <- 0
b2 <- 0
b3 <- 0
b4 <- -15.5
b5 <- 10.25
b6 <- 5.5
b7 <- 13.5

df$yield <- rnorm(n = nrow(df),
                  mean = b0 + 
                    b1 * df$monkey +
                    b2 * df$deer +
                    b3 * df$rat +
                    b4 * df$density +
                    b5 * df$density * df$monkey +
                    b6 * df$density * df$deer +
                    b7 * df$density * df$rat,
                  sd = 150)

m1 <- lm(yield ~ species : density, data = df)

summary(m1)

prds <- predict(m1, se.fit = TRUE)
df$fit <- prds$fit
df$low <- prds$fit - prds$se.fit * 1.96
df$upp <- prds$fit + prds$se.fit * 1.96

p3 <- ggplot(df) +
  geom_point(aes(x = density, y = yield)) +
  geom_ribbon(aes(x = density, ymin = low, ymax = upp, group = species), fill = "white", alpha = 0.5) +
  geom_line(aes(x = density, y = fit, group = species)) +
  labs(x = "Controlled density",
       colour = "Species",
       y = "Yield from field (kg)") +
  facet_wrap(~species, scales = "free_x") +
  sbs_theme()
p3
ggsave(here::here("Figures/Lecture 10 - Interactions", file = "india_nonexp.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)
