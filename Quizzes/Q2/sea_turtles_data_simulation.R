# Quiz data for 2nd quiz:

# Set seed for reproducibility
set.seed(2024)

# Number of observations
n <- 342

# Categorical predictor: Region (with 4 levels)
region <- factor(sample(c("North", "South", "East", "West"), n, replace = TRUE))

# Continuous predictor: Water temperature (ranging between 20 and 30 degrees Celsius)
water_temp <- runif(n, 20, 30)

# Define the true relationship between temperature and oxygen consumption rate, differing by region
# Mean oxygen consumption rate (in mL O2/hr) for each region
mean_oxygen_consumption <- c(North = 50, South = 45, East = 55, West = 48)

# Residual variance based on region (heteroscedasticity)
residual_var <- c(North = 5, South = 3, East = 7, West = 4)

# Simulate oxygen consumption rate with different means per region and dependence on water temperature
oxygen_consumption <- sapply(1:n, function(i) {
  region_effect <- mean_oxygen_consumption[as.character(region[i])]
  temp_effect <- 2.2 * water_temp[i]  # a stronger effect of water temperature
  error_sd <- residual_var[as.character(region[i])] * (1 + 0.15 * water_temp[i]) # heteroscedastic error
  rnorm(1, mean = region_effect + temp_effect, sd = error_sd)
})

# Create the dataset
marine_data <- data.frame(
  oxygen_consumption = oxygen_consumption,
  region = region,
  water_temp = water_temp
)

marine_data$oxygen_consumption <- round(marine_data$oxygen_consumption, digits = 1)
marine_data$water_temp <- round(marine_data$water_temp, digits = 2)

# Take a look at the dataset
head(marine_data)

# Visualize the dataset
library(ggplot2)
ggplot(marine_data, aes(x = water_temp, y = oxygen_consumption, color = region)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method = "lm") +
  labs(x = "Water Temperature (°C)",
       y = "Oxygen Consumption Rate (mL O2/hr)") +
  facet_wrap(~region)
head(marine_data)

m1 <- lm(oxygen_consumption ~ region, data = marine_data)
par(mfrow = c(2,2))
plot(m1)
write.table(marine_data, "H:/003 - Teaching/006-BI3010/Quizzes/Q2/sea_turtles.txt", row.names = FALSE)
