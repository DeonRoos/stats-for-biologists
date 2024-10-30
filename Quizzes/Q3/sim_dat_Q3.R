# Penguin: Flipper * Bill -------------------------------------------------
set.seed(2024)
library(ggplot2)

# Set up
n <- 334
flipper <- rnorm(n = n, mean = 200, sd = 10)
bill <- rnorm(n = n, mean = 46, sd = 4)

# Parameters
b0 <- 4615
b1 <- -200
b2 <- -22
b3 <- 1.5

# Response
mass <- rnorm(n = n, mean = b0 + b1 * bill + b2 * flipper + b3 * flipper * bill, sd = 200)

# Dataframe
penguin <- data.frame(mass, flipper, bill)

# Model
penguin_model <- lm(mass ~ bill * flipper, data = penguin)
summary(penguin_model)

# Figure
fake <- expand.grid(bill = seq(from = min(bill), to = max(bill), by = 0.5),
                    flipper = seq(from = min(flipper), to = max(flipper), by = 1))
fake$fit <- predict(penguin_model, newdata = fake)
ggplot(fake) +
  geom_tile(aes(x = bill, y = flipper, fill = fit)) +
  scale_fill_viridis_c(option = "magma") +
  labs(x = "Bill length",
       y = "Flipper length",
       fill = "Predicted\nbody mass") +
  theme_minimal()

# Penguin: Species * Bill -------------------------------------------------
set.seed(2024)
library(ggplot2)

# Set up
n <- 1334
species <- sample(size = n, c("Gentoo", "Adelie", "Chinstrap"), replace = TRUE)
chinstrap <- ifelse(species == "Chinstrap", 1, 0)
gentoo <- ifelse(species == "Gentoo", 1, 0)
bill <- rnorm(n = n, mean = 46, sd = 4)

# Parameters
b0 <- 2700 # Intercept (Adelie)
b1 <- 20  # Reference slope (Adelie)
b2 <- -2000   # Contrast for Chinstraps
b3 <- 1000  # Contrast for Gentoo
b4 <- 50   # Constrast slope for Chinstraps   
b5 <- -10   # Constrast slope for Gentoo

# Response
mass <- rnorm(n = n, 
              mean = b0 + 
                b1 * bill + 
                b2 * chinstrap + 
                b3 * gentoo +
                b4 * chinstrap * bill + 
                b5 * gentoo * bill,
              sd = 200)

# Dataframe
penguin <- data.frame(mass, species, bill)
penguin$species <- factor(penguin$species, levels = c("Adelie", "Chinstrap", "Gentoo"))

# Model
penguin_model <- lm(mass ~ bill * species, data = penguin)
summary(penguin_model)

# Figure
fake <- expand.grid(bill = seq(from = min(bill), to = max(bill), by = 0.5),
                    species = unique(species))
preds <- predict(penguin_model, newdata = fake, se.fit = TRUE)
fake$fit <- preds$fit
fake$low <- preds$fit - preds$se.fit * 1.96
fake$upp <- preds$fit + preds$se.fit * 1.96
fake$species <- factor(fake$species, levels = c("Adelie", "Chinstrap", "Gentoo"))
ggplot(fake) +
  geom_ribbon(aes(x = bill, ymin = low, ymax = upp, fill = species), alpha = 0.3) +
  geom_line(aes(x = bill, y = fit, colour = species)) +
  #geom_point(data = penguin, aes(x = bill, y = mass, colour = species)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Bill length",
       y = "Predicted body mass",
       colour = "Species",
       fill = "Species") +
  theme_minimal()

ggplot() +
  geom_point(data = penguin[1:400,], aes(x = bill, y = mass, colour = species)) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Bill length",
       y = "Body mass",
       colour = "Species",
       fill = "Species") +
  theme_bw() +
  facet_wrap(~species)


# DAGS --------------------------------------------------------------------

library(performance)

fork <- check_dag(
  X ~ Z,
  Y ~ Z,
  outcome = "Y",
  exposure = "X"
)

plot(fork)

pipe <- check_dag(
  Z ~ X,
  Y ~ Z,
  outcome = "Y",
  exposure = "X"
)

plot(pipe)

collide <- check_dag(
  Z ~ X,
  Z ~ Y,
  outcome = "Y",
  exposure = "X"
)

plot(collide)

eg <- check_dag(
  X ~ Z + V,
  V ~ Z,
  Y ~ X + Z,
  outcome = "Y",
  exposure = "X"
)

plot(eg)
