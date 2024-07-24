
library(ggplot2)
library(here)
library(patchwork)
library(gganimate)
library(dplyr)
source("sbs_theme.R")
source("sbsvoid_theme.R")

df <- read.table(header = TRUE, sep = "\t", here("Lecture 7 - Cat LM/data", file = "sheep.txt"))

shp_model <- lm(horn ~ region, data = df)

# Provided intercept, slope, and standard deviation
# Extract coefficients and residuals standard deviation
coefficients <- coef(shp_model)
sigma <- summary(shp_model)$sigma

# Extract intercepts for each region
intercept_Peace <- coefficients[1]
intercept_Skeena <- coefficients[1] + coefficients[2]

# Create data frame with fitted means for each region
mean_values <- data.frame(
  region = c("Skeena", "Peace"),
  fit_mean = c(intercept_Skeena, intercept_Peace)
)

# Map regions to numeric values and add jitter
jitter_value <- 0.05
df$region_num <- ifelse(df$region == "Skeena", 1, 2)
df <- df %>% mutate(jittered_x = region_num + runif(n(), -jitter_value, jitter_value))

# Combine df with mean values
combined_df <- df %>%
  left_join(mean_values, by = "region") %>%
  mutate(grp = as.factor(region))

# Create the density data for each region
regions <- c("Skeena", "Peace")
intercepts <- c(intercept_Skeena, intercept_Peace)

scaling_factor <- 4  # Adjust this value to exaggerate the distribution

dens <- do.call(rbind, lapply(seq_along(regions), function(i) {
  region <- regions[i]
  region_num <- ifelse(region == "Skeena", 1, 2)
  expected_mean <- intercepts[i]
  xs <- seq(expected_mean - 3 * sigma, expected_mean + 3 * sigma, length.out = 50)
  data.frame(
    y = xs,
    x = region_num - dnorm(xs, expected_mean, sigma) * scaling_factor,  # Increased scaling factor
    section = region,
    type = "Expected"
  )
}))


# Plot the expected distributions
p1 <- ggplot(data = combined_df, aes(x = jittered_x, y = horn)) +
  geom_point(size = 1.5, colour = "white", alpha = 0.5) +
  labs(colour = "Region",
       x = "Region",
       y = "Horn Length") +
  scale_x_continuous(breaks = 1:2, labels = c("Skeena", "Peace")) +
  sbs_theme()
p1
ggsave(here("Lecture 8 - Cat diagnostics/Figures", file = "p1.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

p2 <- ggplot(combined_df, aes(x = jittered_x, y = horn, group = grp)) +
  geom_point(size = 1.5, colour = "white", alpha = 0.5) +
  geom_segment(aes(x = region_num - 0.1, xend = region_num + 0.1, 
                   y = fit_mean, yend = fit_mean), 
               size = 1.5, show.legend = FALSE, colour = "white") +
  labs(x = "Region", y = "Horn Length") +
  scale_x_continuous(breaks = 1:2, labels = c("Skeena", "Peace")) +
  sbs_theme()
p2
ggsave(here("Lecture 8 - Cat diagnostics/Figures", file = "p2.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)

p3 <- ggplot(combined_df, aes(x = jittered_x, y = horn, group = grp)) +
  geom_point(size = 1.5, colour = "white", alpha = 0.5) +
  geom_segment(aes(x = region_num - 0.1, xend = region_num + 0.1,
                   y = fit_mean, yend = fit_mean),
               size = 1.5, show.legend = FALSE, colour = "white") +
  geom_path(data = dens, aes(x = x, y = y, group = section), size = 1, colour = "red") +
  labs(x = "Region", y = "Horn Length") +
  scale_x_continuous(breaks = 1:2, labels = c("Skeena", "Peace")) +
  sbs_theme()
p3

ggsave(here("Lecture 8 - Cat diagnostics/Figures", file = "p3.png"), plot = p3, width = 650/72, height = 775/72, dpi = 72)

ribbon_Skeena <- data.frame(
  x = seq(from = 1-0.1, to = 1+0.1, by = 0.1),
  ymin = intercept_Skeena - sigma,
  ymax = intercept_Skeena + sigma
)

ribbon_Peace <- data.frame(
  x = seq(from = 2-0.1, to = 2+0.1, by = 0.1),
  ymin = intercept_Peace - sigma,
  ymax = intercept_Peace + sigma
)

# Combine ribbons into one data frame
ribbon_data <- rbind(
  data.frame(ribbon_Skeena, region = "Skeena"),
  data.frame(ribbon_Peace, region = "Peace")
)

p4 <- ggplot() +
  geom_ribbon(data = ribbon_data %>% filter(region == "Skeena"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.6) +
  geom_ribbon(data = ribbon_data %>% filter(region == "Peace"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.6) +
  geom_point(data = combined_df, aes(x = jittered_x, y = horn, group = grp),
             size = 1.5, colour = "white", alpha = 0.5) +
  geom_segment(data = combined_df,
               aes(x = region_num - 0.1, xend = region_num + 0.1,
                   y = fit_mean, yend = fit_mean),
               size = 1.5, show.legend = FALSE, colour = "white") +
  geom_path(data = dens, aes(x = x, y = y, group = section), size = 1, colour = "red") +
  labs(x = "Region", y = "Horn Length") +
  scale_x_continuous(breaks = 1:2, labels = c("Skeena", "Peace")) +
  sbs_theme()
p4

ggsave(here("Lecture 8 - Cat diagnostics/Figures", file = "p4.png"), plot = p4, width = 650/72, height = 775/72, dpi = 72)

# Create data frames for ribbons for each region for 2 * sigma
ribbon_Skeena_2sigma <- data.frame(
  x = seq(from = 1-0.1, to = 1+0.1, by = 0.1),
  ymin = intercept_Skeena - 2 * sigma,
  ymax = intercept_Skeena + 2 * sigma
)

ribbon_Peace_2sigma <- data.frame(
  x = seq(from = 2-0.1, to = 2+0.1, by = 0.1),
  ymin = intercept_Peace - 2 * sigma,
  ymax = intercept_Peace + 2 * sigma
)

# Combine ribbons into one data frame
ribbon_data_2sigma <- rbind(
  data.frame(ribbon_Skeena, region = "Skeena", level = "1 Sigma"),
  data.frame(ribbon_Peace, region = "Peace", level = "1 Sigma"),
  data.frame(ribbon_Skeena_2sigma, region = "Skeena", level = "2 Sigma"),
  data.frame(ribbon_Peace_2sigma, region = "Peace", level = "2 Sigma")
)

# Plot with ribbons for 1 * sigma and 2 * sigma
p5 <- ggplot() +
  geom_ribbon(data = ribbon_data_2sigma %>% filter(region == "Skeena" & level == "1 Sigma"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.6) +
  geom_ribbon(data = ribbon_data_2sigma %>% filter(region == "Peace" & level == "1 Sigma"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.6) +
  geom_ribbon(data = ribbon_data_2sigma %>% filter(region == "Skeena" & level == "2 Sigma"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.4) +
  geom_ribbon(data = ribbon_data_2sigma %>% filter(region == "Peace" & level == "2 Sigma"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.4) +
  geom_point(data = combined_df, aes(x = jittered_x, y = horn, group = grp),
             size = 1.5, colour = "white", alpha = 0.5) +
  geom_segment(data = combined_df,
               aes(x = region_num - 0.1, xend = region_num + 0.1,
                   y = fit_mean, yend = fit_mean),
               size = 1.5, show.legend = FALSE, colour = "white") +
  geom_path(data = dens, aes(x = x, y = y, group = section), size = 1, colour = "red") +
  labs(x = "Region", y = "Horn Length") +
  scale_x_continuous(breaks = 1:2, labels = c("Skeena", "Peace")) +
  sbs_theme()
p5

ggsave(here("Lecture 8 - Cat diagnostics/Figures", file = "p5.png"), plot = p5, width = 650/72, height = 775/72, dpi = 72)

# Create data frames for ribbons for each region for 3 * sigma
ribbon_Skeena_3sigma <- data.frame(
  x = seq(from = 1-0.1, to = 1+0.1, by = 0.1),
  ymin = intercept_Skeena - 3 * sigma,
  ymax = intercept_Skeena + 3 * sigma
)

ribbon_Peace_3sigma <- data.frame(
  x = seq(from = 2-0.1, to = 2+0.1, by = 0.1),
  ymin = intercept_Peace - 3 * sigma,
  ymax = intercept_Peace + 3 * sigma
)

# Combine ribbons into one data frame
ribbon_data_3sigma <- rbind(
  data.frame(ribbon_Skeena, region = "Skeena", level = "1 Sigma"),
  data.frame(ribbon_Peace, region = "Peace", level = "1 Sigma"),
  data.frame(ribbon_Skeena_2sigma, region = "Skeena", level = "2 Sigma"),
  data.frame(ribbon_Peace_2sigma, region = "Peace", level = "2 Sigma"),
  data.frame(ribbon_Skeena_3sigma, region = "Skeena", level = "3 Sigma"),
  data.frame(ribbon_Peace_3sigma, region = "Peace", level = "3 Sigma")
)

# Plot with ribbons for 1 * sigma, 2 * sigma, and 3 * sigma
p6 <- ggplot() +
  geom_ribbon(data = ribbon_data_3sigma %>% filter(region == "Skeena" & level == "1 Sigma"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.6) +
  geom_ribbon(data = ribbon_data_3sigma %>% filter(region == "Peace" & level == "1 Sigma"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.6) +
  geom_ribbon(data = ribbon_data_3sigma %>% filter(region == "Skeena" & level == "2 Sigma"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.4) +
  geom_ribbon(data = ribbon_data_3sigma %>% filter(region == "Peace" & level == "2 Sigma"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.4) +
  geom_ribbon(data = ribbon_data_3sigma %>% filter(region == "Skeena" & level == "3 Sigma"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.2) +
  geom_ribbon(data = ribbon_data_3sigma %>% filter(region == "Peace" & level == "3 Sigma"), 
              aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.2) +
  geom_point(data = combined_df, aes(x = jittered_x, y = horn, group = grp),
             size = 1.5, colour = "white", alpha = 0.5) +
  geom_segment(data = combined_df,
               aes(x = region_num - 0.1, xend = region_num + 0.1,
                   y = fit_mean, yend = fit_mean),
               size = 1.5, show.legend = FALSE, colour = "white") +
  geom_path(data = dens, aes(x = x, y = y, group = section), size = 1, colour = "red") +
  labs(x = "Region", y = "Horn Length") +
  scale_x_continuous(breaks = 1:2, labels = c("Skeena", "Peace")) +
  sbs_theme()
p6

ggsave(here("Lecture 8 - Cat diagnostics/Figures", file = "p6.png"), plot = p6, width = 650/72, height = 775/72, dpi = 72)

df$resids <- resid(shp_model)

ggplot(df) +
  geom_histogram(aes(x = resids)) +
  labs(x = "Residuals",
       y = "Frequency") +
  geom_vline(xintercept = 0 + sigma, colour = "red", linewidth = 2.5, linetype = 2) +
  geom_vline(xintercept = 0 - sigma, colour = "red", linewidth = 2.5, linetype = 2) +
  geom_vline(xintercept = 0 + 2 * sigma, colour = "red", linewidth = 1.5, linetype = 2) +
  geom_vline(xintercept = 0 - 2 * sigma, colour = "red", linewidth = 1.5, linetype = 2) +
  geom_vline(xintercept = 0 + 3 * sigma, colour = "red", linewidth = .5, linetype = 2) +
  geom_vline(xintercept = 0 - 3 * sigma, colour = "red", linewidth = .5, linetype = 2) +
  sbs_theme()

breaks <- seq(min(df$resids), max(df$resids), by = 2)  # Specify breaks for better control
bins <- length(breaks) - 1  # Number of bins based on breaks

df_histogram <- as.data.frame(table(cut(df$resids, breaks = breaks)))
df_histogram$middle_value <- (breaks[-1] + breaks[-length(breaks)]) / 2

alpha_min <- 0.2
alpha_max <- 1

df_histogram$alpha <- ifelse(abs(df_histogram$middle_value) <= sigma, 1,
                             ifelse(abs(df_histogram$middle_value) <= 2 * sigma, 0.5,
                                    ifelse(abs(df_histogram$middle_value) <= 3 * sigma, 0.2, NA)))

df_histogram <- df_histogram[!is.na(df_histogram$alpha), ]


p7 <- ggplot(df_histogram, aes(x = middle_value, y = Freq, fill = "red", alpha = factor(alpha))) +
  geom_bar(stat = "identity", show.legend = FALSE, colour = "transparent") +
  geom_vline(xintercept = 0, colour = "white") +
  labs(x = "Residuals", y = "Frequency") +
  scale_alpha_manual(values = c("1" = 1, "0.5" = 0.6, "0.2" = 0.4)) +
  sbs_theme()
p7


# Calculate expected normal distribution values
xs <- seq(min(df_histogram$middle_value), max(df_histogram$middle_value), length.out = 100)
expected_density <- dnorm(xs, mean = 0, sd = sigma)

# Scale expected_density to match maximum Freq in df_histogram
scaling_factor <- max(df_histogram$Freq) / max(expected_density)
expected_density <- expected_density * scaling_factor

df_expected <- data.frame(xs = xs, expected_density = expected_density)

# Create the histogram with customized fill and alpha
p8 <- ggplot(df_histogram, aes(x = middle_value, y = Freq, fill = "red")) +
  geom_bar(stat = "identity", aes(alpha = factor(alpha)), show.legend = FALSE, colour = "transparent") +
  geom_vline(xintercept = 0, colour = "white") +
  labs(x = "Residuals", 
       y = "Frequency",
       title = expression(paste("Normal(0, ", sigma[i], ")"))) +
  scale_alpha_manual(values = c("1" = 1, "0.5" = 0.6, "0.2" = 0.4), guide = "none") +
  geom_line(data = df_expected, aes(xs, expected_density), color = "red", size = 1) +
  sbs_theme()
p8
ggsave(here("Lecture 8 - Cat diagnostics/Figures", file = "p8.png"), plot = p8, width = 650/72, height = 775/72, dpi = 72)

par(mfrow = c(2,2), bg = "black", col = "white", col.axis = "white", col.lab = "white")
plot(shp_model)

par(mfrow = c(1,1), bg = "black", col = "white", col.axis = "white", col.lab = "white")
plot(shp_model, ask = FALSE)





net_dat <- read.csv(here("Lecture 8 - Cat diagnostics/Data", file = "netflix_time.csv"),
                   header = TRUE,
                   stringsAsFactors = TRUE)
head(net_dat, n = 10)
summary(net_dat)
p <- ggplot(net_dat, aes(x = region, y = netflix)) +
  geom_jitter(width = 0.1, height = 0) +
  sbs_theme()
p
ggsave(here("Lecture 8 - Cat diagnostics/Figures", file = "net_eda.png"), plot = p, width = 650/72, height = 775/72, dpi = 72)


df <- data.frame(
  region = unique(net_dat$region),
  est = c(10000, 30000, 15000, 30000)
)
p6 <- ggplot(df) +
  geom_point(aes(x = region, y = est)) +
  labs(x = "Region",
       y = "Netflix time (minutes)") +
  scale_y_continuous(limits = c(0, NA)) +
  sbs_theme()
p6
ggsave(here("Lecture 8 - Cat diagnostics/Figures", file = "multi_contrast.png"), plot = p6, width = 650/72, height = 775/72, dpi = 72)



net_mod <- lm(netflix ~ region, data = net_dat)
summary(net_mod)

par(mfrow = c(2,2), bg = "black", col = "white", col.axis = "white", col.lab = "white")
plot(net_mod, ask = F)

summary(cc_mod)

fake <- data.frame(region = unique(net_dat$region))
prds <- predict(net_mod, newdata = fake, se.fit = TRUE)
fake$fit <- prds$fit
fake$low <- prds$fit - prds$se.fit * 1.96
fake$upp <- prds$fit + prds$se.fit * 1.96
fake

p1 <- ggplot(fake) +
  geom_point(aes(x = region, y = fit)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Region",
       y = "Netflix time (minutes)") +
  sbs_theme()
p1
ggsave(here("Lecture 8 - Cat diagnostics/Figures", file = "net_pred.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)


p2 <- ggplot(fake) +
  geom_point(aes(x = region, y = fit)) +
  geom_errorbar(aes(x = region, ymin = low, ymax = upp), colour = "white") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Region",
       y = "Netflix time (minutes)") +
  sbs_theme()
p2
ggsave(here("Lecture 8 - Cat diagnostics/Figures", file = "bet_pred_ci.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)

