
set.seed(1988)
n <- 322

field <- rlnorm(n, meanlog = 2, sdlog = 1.5)
field <- ifelse(field > 500, 500, field)
tree <- rgamma(n, shape = 3, rate = 0.025)
slope <- rbeta(n, shape1 = 0.5, shape2 = 5)

vegetation <- sample(c("Sparse", "Dense"), size = n, replace = TRUE, prob = c(0.5, 0.5))
rock <- sample(c("None", "Some", "Lots"), size = n, replace = TRUE, prob = c(0.6, 0.3, 0.1))
aspect <- sample(c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), size = n, replace = TRUE)

veg_sparse <- ifelse(vegetation == "Sparse", 1, 0)
veg_dense  <- ifelse(vegetation == "Dense",  1, 0)

rock_none <- ifelse(rock == "None", 1, 0)
rock_some <- ifelse(rock == "Some", 1, 0)
rock_lots <- ifelse(rock == "Lots", 1, 0)

asp_N  <- ifelse(aspect == "N",  1, 0)
asp_NE <- ifelse(aspect == "NE", 1, 0)
asp_E  <- ifelse(aspect == "E",  1, 0)
asp_SE <- ifelse(aspect == "SE", 1, 0)
asp_S  <- ifelse(aspect == "S",  1, 0)
asp_SW <- ifelse(aspect == "SW", 1, 0)
asp_W  <- ifelse(aspect == "W",  1, 0)
asp_NW <- ifelse(aspect == "NW", 1, 0)

log_lambda <- -2 +
  3   * slope +
  (-0.01) * tree +
  1   * veg_dense +
  1.5 * rock_none +
  0.5 * rock_some

lambda_sett <- exp(log_lambda)
n_setts     <- rpois(n, lambda_sett)

plot(n_setts ~ slope)
plot(n_setts ~ tree)
boxplot(n_setts ~ vegetation)
boxplot(n_setts ~ rock)

df <- data.frame(n_setts, 
                 field_dist = round(field, digits = 2), 
                 tree_dist = round(tree, digits = 2), 
                 slope = round(slope, digits = 2), 
                 vegetation, 
                 rock, 
                 aspect)

df |>
  write.table("C:\\006-BI3010\\Quizzes\\resit_data\\badger_setts.txt",
              sep = "\t",
              row.names = FALSE,
              quote = FALSE)

summary(glm(n_setts ~ slope + tree + vegetation + rock, data = df, family = poisson))








set.seed(1988)

n <- 162

fertiliser <- factor(sample(c("A", "B", "C"), n, TRUE))
leaf_n_percent <- rnorm(n, 2.5, 0.4)
par_mol_m2_day <- runif(n, 5, 25)

mu <- 20 +
  ifelse(fertiliser == "B", 2, 0) +
  ifelse(fertiliser == "C", 4, 0) +
  2.5 * leaf_n_percent -
  0.3 * par_mol_m2_day

biomass_g <- rnorm(n, mu, 0.5)

df <- data.frame(biomass = biomass_g, 
                 fertiliser, 
                 leaf_n = leaf_n_percent, 
                 light = par_mol_m2_day)

boxplot(df$biomass ~ df$fertiliser)
plot(df$biomass ~ df$leaf_n)
plot(df$biomass ~ df$light)
summary(df)
df |>
  write.table("C:\\006-BI3010\\Quizzes\\resit_data\\leaf_n.txt",
              sep = "\t",
              row.names = FALSE,
              quote = FALSE)

summary(lm(biomass ~ fertiliser + leaf_n + light, data = df))





set.seed(1988)

n <- 531

habitat <- factor(sample(c("Urban", "Orchard", "Meadow"),
                         n, TRUE, c(0.3, 0.4, 0.3)))

varroa <- rbinom(n, 1, 0.4)

mu <- 1200 +
  ifelse(habitat == "Orchard", -200,
         ifelse(habitat == "Meadow", -300, 0)) +
  -150 * varroa

disp <- rnorm(n, mu, 250)

bee_df <- data.frame(disp, habitat, varroa)

bee_df$varroa <- ifelse(bee_df$varroa == 1, "Present", "Absent")
bee_df$disp <- round(bee_df$disp, digits = 1)

head(bee_df)

boxplot(bee_df$disp ~ bee_df$habitat)
boxplot(bee_df$disp ~ bee_df$varroa)

summary(lm(disp ~ habitat + varroa, data = bee_df))

bee_df |>
  write.table("C:\\006-BI3010\\Quizzes\\resit_data\\bees.txt",
              sep = "\t",
              row.names = FALSE,
              quote = FALSE)


set.seed(1988)

n <- 400

habitat <- factor(sample(c("Woodland", "Farmland", "Urban"),
                         n, TRUE, c(0.4, 0.35, 0.25)))

temp  <- rnorm(n, 15, 3)
wind  <- runif(n, 0, 6)
moon  <- runif(n, 0, 1)

log_lambda <- 2 +
  0.12 * temp -
  0.25 * wind -
  0.5  * moon +
  ifelse(habitat == "Farmland", -0.3, 0) +
  ifelse(habitat == "Urban",    -1.0, 0)

passes <- rpois(n, exp(log_lambda))

bat_df <- data.frame(passes, temp, wind, moon, habitat)

head(bat_df)

plot(bat_df$passes ~ bat_df$temp)
plot(bat_df$passes ~ bat_df$wind)
plot(bat_df$passes ~ bat_df$moon)
plot(bat_df$passes ~ bat_df$habitat)

bat_df |>
  write.table("C:\\006-BI3010\\Quizzes\\resit_data\\bats.txt",
              sep = "\t",
              row.names = FALSE,
              quote = FALSE)


summary(glm(passes ~ temp + wind + moon + habitat, data = bat_df, family = poisson))
