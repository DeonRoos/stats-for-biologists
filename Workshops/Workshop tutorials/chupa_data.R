# Chupacabra growth rate dataset ------------------------------------------
set.seed(666)
n_years <- 15
texan_states <- c("Houston", "Dallas", "Austin", "San Antonio", "Fort Worth", 
                  "El Paso", "Arlington", "Corpus Christi", "Plano", "Laredo", 
                  "Lubbock", "Garland", "Irving", "Amarillo", "Grand Prairie")
n_sites <- length(texan_states)
chup <- expand.grid(
  year = 1980 + 1:n_years,
  city = texan_states
)
phi <- 0.9 # autocorrelation
mean_lambda <- -7 # rate of UFO sightings
alpha <- 0.5 # effect of alcohol consumption on UFO sightings
base_alcohol_consumption <- rlnorm(n = n_sites, meanlog = 2, sdlog = 0.1)
alcohol_matrix <- matrix(nrow = n_years, ncol = n_sites)
for (i in 1:n_sites) {
  alcohol_series <- base_alcohol_consumption[i] + rnorm(n_years, mean = 0, sd = 2)
  alcohol_series <- pmax(0, alcohol_series)  # Ensure no negative values
  alcohol_matrix[, i] <- alcohol_series
}
chup$alcohol_consumption <- as.vector(t(alcohol_matrix))
ufo_matrix <- matrix(nrow = n_years, ncol = n_sites)
for (i in 1:n_sites) {
  ufo_series <- arima.sim(model = list(ar = phi), n = n_years) + mean_lambda
  alcohol_series <- chup$alcohol_consumption[chup$city == texan_states[i]]
  adjusted_lambda <- exp(ufo_series + alpha * alcohol_series)
  ufo_matrix[, i] <- rpois(n_years, lambda = adjusted_lambda)
}
chup$ufo <- as.vector(t(ufo_matrix))

# Sociological factor: Belief in paranormal activity ---------------------
belief_levels <- c("high", "medium", "low")
belief_in_paranormal <- sample(belief_levels, n_sites, replace = TRUE)
belief_matrix <- matrix(rep(belief_in_paranormal, each = n_years), ncol = n_sites)
chup$belief_in_paranormal <- as.vector(t(belief_matrix))

# Impact of belief on chupacabra growth ----------------------------------
b0 <- -1.7
b1 <- 0.1
b2 <- 0.2
b_belief <- c("high" = 0.3, "medium" = 0.1, "low" = -0.2) # Effect sizes for belief levels
belief_effect <- as.numeric(b_belief[chup$belief_in_paranormal])

chup$chup <- rnorm(n = nrow(chup), 
                   mean = b0 + b1 * chup$ufo + b2 * chup$alcohol_consumption + belief_effect, 
                   sd = 0.1)

# Round and clean up the dataset -----------------------------------------
chup$alcohol_consumption <- round(chup$alcohol_consumption, digits = 1)
chup$chup <- round(chup$chup, digits = 2)
chup$city <- as.character(chup$city)
colnames(chup) <- c("year", "city", "alc", "ufo", "belief", "chupa")

# Data entry mistakes -----------------------------------------------------
chup$chupa[31] <- chup$chupa[31] * 100 # Should be 0.05
chup$city[198] <- "amarillo"           # Should be Amarillo
chup$ufo[50] <- 9999                   # Should be 0
chup$alc[120] <- -5                    # Should be 9

head(chup)

write.table(chup, here::here("Workshops/Workshop tutorials", file = "chupa.txt"),
            row.names = FALSE)



# Test run ----------------------------------------------------------------

summary(chup)
str(chup)

library(ggeffects)
plot(ggpredict(lm(chupa ~ ., data = chup)))
