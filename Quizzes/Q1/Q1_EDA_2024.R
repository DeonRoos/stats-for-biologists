df <- read.csv("C:/BI3010/Quiz 1/Q1.csv", header = TRUE)

df$prop <- df$Auto.Score / df$Possible.Points

df$category[df$Question == unique(df$Question)[1]] <- "EDA"
df$category[df$Question == unique(df$Question)[14]] <- "EDA"
df$category[df$Question == unique(df$Question)[16]] <- "EDA"
df$category[df$Question == unique(df$Question)[18]] <- "EDA"
df$category[df$Question == unique(df$Question)[2]] <- "Assumptions"
df$category[df$Question == unique(df$Question)[3]] <- "Assumptions"
df$category[df$Question == unique(df$Question)[10]] <- "Assumptions"
df$category[df$Question == unique(df$Question)[13]] <- "Assumptions"
df$category[df$Question == unique(df$Question)[4]] <- "Statistics"
df$category[df$Question == unique(df$Question)[5]] <- "Statistics"
df$category[df$Question == unique(df$Question)[9]] <- "Statistics"
df$category[df$Question == unique(df$Question)[11]] <- "Statistics"
df$category[df$Question == unique(df$Question)[12]] <- "Statistics"
df$category[df$Question == unique(df$Question)[15]] <- "Statistics"
df$category[df$Question == unique(df$Question)[6]] <- "Coding"
df$category[df$Question == unique(df$Question)[7]] <- "Coding"
df$category[df$Question == unique(df$Question)[8]] <- "Coding"
df$category[df$Question == unique(df$Question)[17]] <- NA
df$category[df$Question == unique(df$Question)[19]] <- NA

unique(df$Question[is.na(df$category)])

df$category2 <- ifelse(df$category == "EDA", "EDA (n = 4)",
                       ifelse(df$category == "Assumptions", "Assumptions (n = 4)",
                              ifelse(df$category == "Statistics", "Statistics (n = 6)",
                                     "Coding (n = 3)")))

df <- df[!is.na(df$category),]

ggplot(df) +
  geom_density(aes(x = prop, fill = category2)) +
  facet_wrap(~category) +
  labs(x = "Proportion of points",
       y = "Density",
       fill = "Question\nCategory") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

ggplot(df) +
  geom_density(aes(x = prop, fill = category2)) +
  facet_wrap(~category) +
  labs(x = "Score",
       y = "Density",
       fill = "Question\nCategory") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

hist(rbeta(n = 10000, shape1 = 8, shape2 = 4))


transform_to_beta <- function(prop_values) {
  # Rank the data and scale it to be uniformly distributed between 0 and 1
  uniform_values <- rank(prop_values) / (length(prop_values) + 1)
  
  # Apply the Beta(8, 4) inverse CDF (quantile function)
  alpha <- 4
  beta_param <- 8
  transformed_values <- qbeta(uniform_values, shape1 = alpha, shape2 = beta_param)
  
  return(transformed_values)
}

df_transformed <- df %>%
  group_by(category) %>%
  mutate(prop_transformed = transform_to_beta(prop))

ggplot(df_transformed) +
  geom_density(aes(x = prop_transformed, fill = category2)) +
  facet_wrap(~category) +
  labs(x = "Score",
       y = "Density",
       fill = "Question\nCategory") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

df_transformed <- df_transformed %>%
  group_by(Username) %>%
  summarise(ideal_grade = sum(prop_transformed * Possible.Points),
            true_grade = sum(prop * Possible.Points))

ggplot(df_transformed) +
  geom_histogram(aes(x = ideal_grade), fill = "red") +
  geom_histogram(aes(x = true_grade)) +
  labs(x = "Score",
       y = "Density",
       fill = "Question\nCategory") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()


df_transformed %>%
  group_by(Username) %>%
  summarise(true_grade = sum(Auto.Score))
