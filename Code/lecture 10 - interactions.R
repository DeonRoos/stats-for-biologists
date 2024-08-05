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
b1 <- -180
b2 <- -150
b3 <- 150
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

library(ggplot2)

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
