
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


# Parameters for the AR(1) process
phi <- 0.9 # stronger autocorrelation parameter
mean_lambda <- -7 # average rate of UFO sightings
alpha <- 0.5 # effect of alcohol consumption on UFO sightings

# Generate site-specific base levels of alcohol consumption
base_alcohol_consumption <- rlnorm(n = n_sites, meanlog = 2, sdlog = 0.1)

# Create a matrix to store alcohol consumption data
alcohol_matrix <- matrix(nrow = n_years, ncol = n_sites)

for (i in 1:n_sites) {
  # Generate yearly variation around the base level for each site
  alcohol_series <- base_alcohol_consumption[i] + rnorm(n_years, mean = 0, sd = 2)
  # Ensure alcohol consumption is positive
  alcohol_series <- pmax(0, alcohol_series)
  alcohol_matrix[, i] <- alcohol_series
}

# Reshape the alcohol matrix to a long format and add to chup dataframe
chup$alcohol_consumption <- as.vector(t(alcohol_matrix))

# Create a matrix to store the ufo sightings
ufo_matrix <- matrix(nrow = n_years, ncol = n_sites)

for (i in 1:n_sites) {
  # Generate a time series with temporal autocorrelation
  ufo_series <- arima.sim(model = list(ar = phi), n = n_years) + mean_lambda
  
  # Get the alcohol consumption for this site
  alcohol_series <- chup$alcohol_consumption[chup$city == texan_states[i]]
  
  # Modify lambda based on alcohol consumption
  adjusted_lambda <- exp(ufo_series + alpha * alcohol_series)
  
  # Store the series in the matrix
  ufo_matrix[, i] <- rpois(n_years, lambda = adjusted_lambda)
}

# Reshape the matrix to a long format and add to chup dataframe
chup$ufo <- as.vector(t(ufo_matrix))

# View the resulting dataframe
head(chup)




library(ggplot2)
ggplot(chup, aes(x = year, y = ufo, colour = city, group = city)) +
  geom_point() +
  geom_path()

ggplot(chup, aes(y = ufo, x = alcohol_consumption, colour = city, group = city)) +
  geom_point()

b0 <- -1.7
b1 <- 0.1 # UFO
b2 <- 0.2

chup$chup <- rnorm(n = nrow(chup), mean = b0 + b1 * chup$ufo + b2 * chup$alcohol_consumption, sd = 0.1)

ggplot(chup, aes(x = year, y = chup, colour = city, group = city)) +
  geom_point() +
  geom_path()

chup$alcohol_consumption <- round(chup$alcohol_consumption, digits = 1)
chup$chup <- round(chup$chup, digits = 2)

colnames(chup) <- c("year", "site", "alcho_units", "ufo", "chupa")

write.table(chup, here::here("Workshops/Workshop 2 - Data vis", file = "chupa.txt"),
            row.names = FALSE)
