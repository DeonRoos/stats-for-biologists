# Lecture 3

library(ggplot2)
library(here)
library(patchwork)
source("sbs_theme.R")

set.seed(666)
n <- 347

df <- data.frame(
  forest = round(rbeta(n, shape1 = 0.8, shape2 = 2.3), digits = 2)
)
df$lumber <- rpois(n, lambda = 4 + -0.5 * df$forest)
df$density <- round(rnorm(n, mean = 10.4 + 2.5 * df$forest + -0.8 * df$lumber, sd = 0.8), digits = 1)
df$rainfall <- runif(n, 0, 100)
df$protected <- ifelse(rbinom(n, size = 1, prob = 0.8) == 1, "Unprotected", "Protected")
head(df)

p <- ggplot(df, aes(y = forest, x = density, size = lumber, colour = rainfall)) +
  geom_point() +
  facet_wrap(~protected) +
  labs(x = "Density of nests",
       y = "Percentage old forest", 
       size = "Tonnes of timber\nextracted (mill)",
       colour = "Rainfall (mm)") +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_viridis_c(option = "C") +
  sbs_theme()
p
ggsave(here("Lecture 3 - data_vis/Figures", file = "too_complex.png"), plot = p, width = 1400/72, height = 650/72, dpi = 72)

p1 <- ggplot(df, aes(y = forest, x = density, size = lumber, colour = rainfall)) +
  geom_point() +
  facet_grid(protected~.) +
  labs(x = "Density of nests",
       y = "Percentage old forest", 
       size = "Tonnes of timber\nextracted (mill)",
       colour = "Rainfall (mm)") +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_viridis_c(option = "C") +
  sbs_theme()
p1
ggsave(here("Lecture 3 - data_vis/Figures", file = "too_complex_small.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

p1 <- ggplot(df, aes(x = forest, y = density)) +
  geom_point() +
  labs(y = "Density of nests",
       x = "Percentage old forest") +
  scale_x_continuous(labels = scales::percent) +
  sbs_theme()
p1
ggsave(here("Lecture 3 - data_vis/Figures", file = "just_right.png"), plot = p1, width = 1400/72, height = 650/72, dpi = 72)
ggsave(here("Lecture 3 - data_vis/Figures", file = "just_right_small.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

# Simple layout -----------------------------------------------------------



summary(lm(density ~ forest, data = df))

df

library(datasauRus)

sau_sum <- datasaurus_dozen %>% 
  group_by(dataset) %>% 
  summarize(
    mean_x    = round(mean(x), digits = 1),
    mean_y    = round(mean(y), digits = 1),
    std_dev_x = round(sd(x), digits = 1),
    std_dev_y = round(sd(y), digits = 1),
    corr_x_y  = round(cor(x, y), digits = 1)
  )

write.csv(sau_sum, here("Lecture 3 - data_vis/Figures", file = "datasaurus_table.csv"))

p2 <- ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset))+
  geom_point(size = 0.8) +
  coord_fixed() +
  sbsvoid_theme() +
  theme(legend.position = "none")+
  facet_wrap(~dataset, nrow = 3)
p2
ggsave(here("Lecture 3 - data_vis/Figures", file = "dino.png"), plot = p2, width = 1400/72, height = 650/72, dpi = 72)



# Hullman examples --------------------------------------------------------

df <- data.frame(
  x = c(4,5,6),
  y = c(1,1,1),
  z = c(1,7,10)
)

p1 <- ggplot(df) +
  geom_point(aes(x = x, y = y, colour = z), size = 40, show.legend = FALSE) +
  scale_colour_viridis_c(limits = c(0, 15), option = "C", direction = -1) +
  scale_x_continuous(limits = c(3,7)) +
  geom_text(x = 4, y = 1.02, label = "1", colour = "white", size = 10) +
  geom_text(x = 6, y = 1.02, label = "10", colour = "white", size = 10) +
  geom_text(x = 5, y = 1.02, label = "?", colour = "white", size = 10) +
  labs(colour = "Intensity",
       title = "Humans really struggle to see precise differences in intensity") +
  sbsvoid_theme()
p1
ggsave(here("Lecture 3 - data_vis/Figures", file = "intensity.png"), plot = p1, width = 1400/72, height = 650/72, dpi = 72)

df <- data.frame(
  x = c(1,1,1,
        5,5,5),
  y = c(1,1,1,
        35,7,50),
  group = c("a","b", "c",
            "a","b", "c")
)

p2 <- ggplot(df) +
  geom_line(aes(x = x, y = y, group = group)) +
  scale_y_continuous(limits = c(0, 70)) +
  geom_text(x = 4.9, y = 10, label = "1", colour = "white", size = 10) +
  geom_text(x = 4.9, y = 38, label = "?", colour = "white", size = 10) +
  geom_text(x = 4.9, y = 53, label = "10", colour = "white", size = 10) +
  labs(colour = "Intensity",
       title = "Humans struggle to see precise differences in angles") +
  sbsvoid_theme()
p2
ggsave(here("Lecture 3 - data_vis/Figures", file = "angles.png"), plot = p2, width = 1400/72, height = 650/72, dpi = 72)

df <- data.frame(
  x = c(4,5,6),
  y = c(1,1,1),
  z = c(1,3,10)
)

p3 <- ggplot(df) +
  geom_point(aes(x = x, y = y, size = z),
             show.legend = FALSE) +
  scale_size(range = c(25,50)) +
  scale_x_continuous(limits = c(3,7)) +
  geom_text(x = 4, y = 1.02, label = "1", colour = "white", size = 10) +
  geom_text(x = 5, y = 1.02, label = "?", colour = "white", size = 10) +
  geom_text(x = 6, y = 1.02, label = "10", colour = "white", size = 10) +
  labs(colour = "Intensity",
       title = "Humans are ok with precise differences in size") +
  sbsvoid_theme()
p3
ggsave(here("Lecture 3 - data_vis/Figures", file = "size.png"), plot = p3, width = 1400/72, height = 650/72, dpi = 72)


df <- data.frame(
  x = c(4,5,6),
  y = c(1,1,1),
  z = c(1,4,10)
)

p4 <- ggplot(df) +
  geom_col(aes(x = x, y = z),
             show.legend = FALSE,
           fill = "#72758d") +
  scale_size(range = c(10,15)) +
  scale_x_continuous(limits = c(3,7)) +
  scale_y_continuous(limits = c(0,12)) +
  geom_text(x = 4, y = 2, label = "1", colour = "white", size = 10) +
  geom_text(x = 5, y = 5, label = "?", colour = "white", size = 10) +
  geom_text(x = 6, y = 11, label = "10", colour = "white", size = 10) +
  labs(colour = "Intensity",
       title = "Humans are pretty good with precise differences in length") +
  sbsvoid_theme()
p4
ggsave(here("Lecture 3 - data_vis/Figures", file = "length.png"), plot = p4, width = 1400/72, height = 650/72, dpi = 72)

df <- data.frame(
  y = c(1,8,10),
  x = c(1,1,1)
)

p5 <- ggplot(df) +
  geom_point(aes(x = y, y = x), size = 25,
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0,2)) +
  geom_text(y = 1.3, x = 1, label = "1", colour = "white", size = 10) +
  geom_text(y = 1.3, x = 8, label = "?", colour = "white", size = 10) +
  geom_text(y = 1.3, x = 10, label = "10", colour = "white", size = 10) +
  labs(colour = "Intensity",
       title = "Humans are excellent with precise differences in position") +
  sbsvoid_theme()
p5
ggsave(here("Lecture 3 - data_vis/Figures", file = "position.png"), plot = p5, width = 1400/72, height = 650/72, dpi = 72)


# Merged ------------------------------------------------------------------

df <- data.frame(
  x = c(4,5,6),
  y = c(1,1,1),
  z = c(1,7,10)
)

p1 <- ggplot(df) +
  geom_point(aes(x = x, y = y, colour = z), size = 5, show.legend = FALSE) +
  scale_colour_viridis_c(limits = c(0, 15), option = "C", direction = -1) +
  scale_x_continuous(limits = c(3,7)) +
  geom_text(x = 4, y = 1.009, label = "1", colour = "white", size = 10) +
  geom_text(x = 6, y = 1.009, label = "10", colour = "white", size = 10) +
  geom_text(x = 5, y = 1.009, label = "?", colour = "white", size = 10) +
  geom_text(x = 5, y = 1.045, label = "Intensity", colour = "#72758d", size = 8) +
  labs(title = "Inaccurate") +
  sbsvoid_theme()
p1

df <- data.frame(
  x = c(1,1,1,
        5,5,5),
  y = c(1,1,1,
        35,7,50),
  group = c("a","b", "c",
            "a","b", "c")
)

p2 <- ggplot(df) +
  geom_line(aes(x = x, y = y, group = group)) +
  scale_y_continuous(limits = c(0, 70)) +
  geom_text(x = 4.5, y = 10, label = "1", colour = "white", size = 10) +
  geom_text(x = 4.5, y = 38, label = "?", colour = "white", size = 10) +
  geom_text(x = 4.5, y = 53, label = "10", colour = "white", size = 10) +
  geom_text(x = 3, y = 70, label = "Angle", colour = "#72758d", size = 8) +
  sbsvoid_theme()
p2

df <- data.frame(
  x = c(4,5,6),
  y = c(1,1,1),
  z = c(1,3,10)
)

p3 <- ggplot(df) +
  geom_point(aes(x = x, y = y, size = z),
             show.legend = FALSE) +
  scale_size(range = c(2,5)) +
  scale_x_continuous(limits = c(3,7)) +
  geom_text(x = 4, y = 1.01, label = "1", colour = "white", size = 10) +
  geom_text(x = 5, y = 1.01, label = "?", colour = "white", size = 10) +
  geom_text(x = 6, y = 1.01, label = "10", colour = "white", size = 10) +
  geom_text(x = 5, y = 1.045, label = "Size", colour = "#72758d", size = 8) +
  sbsvoid_theme()
p3

df <- data.frame(
  x = c(4,5,6),
  y = c(1,1,1),
  z = c(1,4,10)
)

p4 <- ggplot(df) +
  geom_col(aes(x = x, y = z),
           show.legend = FALSE,
           fill = "#72758d") +
  scale_size(range = c(10,15)) +
  scale_x_continuous(limits = c(3,7)) +
  scale_y_continuous(limits = c(0,12)) +
  geom_text(x = 4, y = 2, label = "1", colour = "white", size = 10) +
  geom_text(x = 5, y = 5, label = "?", colour = "white", size = 10) +
  geom_text(x = 6, y = 11, label = "10", colour = "white", size = 10) +
  geom_text(x = 5, y = 12, label = "Length", colour = "#72758d", size = 8) +
  sbsvoid_theme()
p4

df <- data.frame(
  y = c(1,8,10),
  x = c(1,1,1)
)

p5 <- ggplot(df) +
  geom_point(aes(x = y, y = x), size = 5,
             show.legend = FALSE) +
  scale_y_continuous(limits = c(0,2)) +
  scale_x_continuous(limits = c(0,11)) +
  geom_text(y = 1.2, x = 1, label = "1", colour = "white", size = 10) +
  geom_text(y = 1.2, x = 8, label = "?", colour = "white", size = 10) +
  geom_text(y = 1.2, x = 10, label = "10", colour = "white", size = 10) +
  geom_text(x = 5, y = 2, label = "Position", colour = "#72758d", size = 8) +
  labs(title = "Accurate") +
  sbsvoid_theme()
p5

design <- "
ABCDE
"

p6 <- p1 + p2 + p3 + p4 + p5 + plot_layout(design = design)
p6
ggsave(here("Lecture 3 - data_vis/Figures", file = "accuracy_compare.png"), plot = p6, width = 1400/72, height = 650/72, dpi = 72)


# Example of where intensity is correct -----------------------------------


# Using Ana's/bioss' ascot package to get spatially correlated landscape with temporal trend
# remotes::install_git("https://gitlab.bioss.ac.uk/ukceh-bioss-framework/dev/ascot")
library(ascot)
library(lubridate)

# Generate landscape ------------------------------------------------------
set.seed(1988)

# Takes a couple of mins to run - reduce x and y to increase speed
spatial_field <- define_pattern_s(n_x = 20, n_y = 20, 
                                  autocorr = TRUE,
                                  range = 2.5, amplitude = 5)

# 25 temporal units - called days but treated as years
time_df <- set_timeline(14, "days")

# Little bit of temporal noise in landscape + sin wave
time_series <- define_pattern_t(time_df, weekly_amplitude = 0.5, weekly_noise = 1)

#Simulate spatio-temporal data
truth <- simulate_spacetime(temporal_pattern = time_series,
                            field = spatial_field,
                            pattern_adherence = 0)
truth$time <- interval(min(truth$date), truth$date) %/% days(1)
p1 <- ggplot() +
  geom_tile(data = truth[truth$time == 0,], aes(x = x, y = y, fill = value),
            show.legend = FALSE) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Biodiversity Index",
       x = "Longitude",
       y = "Latitude") +
  coord_fixed() +
  sbs_theme()

ggsave(here("Lecture 3 - data_vis/Figures", file = "intensity_correct.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)



# Intensity fail ----------------------------------------------------------

df <- expand.grid(
  x = 0:100,
  y = 0:100
)
df$value <- ifelse(df$y > 50, 0.5, 1.5)

df1 <- data.frame(
  x = c(50, 50),
  y = c(25, 75),
  value = c(1,1)
)

p1 <- ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_raster(show.legend = FALSE) +
  geom_point(data = df1, aes(x = x, y = y),
             size = 40, show.legend = FALSE, colour = "grey60") +
  scale_fill_gradient(low = "grey40", high = "lightgrey") +
  coord_fixed() +
  sbsvoid_theme()
p1
ggsave(here("Lecture 3 - data_vis/Figures", file = "intensity_fail.png"), plot = p1, width = 650/72, height = 775/72, dpi = 72)

p2 <- ggplot() +
  #geom_raster(show.legend = FALSE) +
  geom_point(data = df1, aes(x = x, y = y),
             size = 40, show.legend = FALSE, , colour = "grey60") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(0, 100)) +
  coord_fixed() +
  sbsvoid_theme()
p2
ggsave(here("Lecture 3 - data_vis/Figures", file = "intensity_reveal.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)



# aNGLE -------------------------------------------------------------------

# Slope of 2
# slope of 0.5
# slope of -1
df <- data.frame(
  x = c(
    -2,0,
    5,3,
    9,7,
    13,11),
  y = c(
    0,2,
    14,8,
    16,10,
    12,6),
  group = c(
    "a","b",
    "a","b",
    "a","b",
    "a","b"),
  col = c(
    "lightgrey", "lightgrey",
    "white", "white",
    "b", "b",
    "b", "b"
  )
)

p2 <- ggplot(df) +
  geom_line(aes(x = x, y = y, group = group, colour = col),
            show.legend = FALSE, size = 3) +
  scale_colour_brewer(palette = "Dark2") +
  sbs_theme()
p2
ggsave(here("Lecture 3 - data_vis/Figures", file = "angle_fail.png"), plot = p2, width = 650/72, height = 775/72, dpi = 72)


# Length ------------------------------------------------------------------

df <- data.frame(
  x = c(4,5,6, 4,5,6, 4,5,6, 4,5,6, 4,5,6),
  z = c(1,4,10, 7,4,2, 8,3,4, 8,7,8, 2,1,5),
  y = factor(c(1,1,1, 2,2,2, 3,3,3, 4,4,4, 5,5,5))
)

p4 <- ggplot(df) +
  geom_col(aes(x = x, y = z, fill = y),
           show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  sbsvoid_theme()
p4
ggsave(here("Lecture 3 - data_vis/Figures", file = "length_fail.png"), plot = p4, width = 650/72, height = 775/72, dpi = 72)

p4 <- ggplot(df) +
  geom_col(aes(x = x, y = z, fill = y),
           show.legend = FALSE, position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  sbsvoid_theme()
p4
ggsave(here("Lecture 3 - data_vis/Figures", file = "length_fail_correct.png"), plot = p4, width = 650/72, height = 775/72, dpi = 72)

