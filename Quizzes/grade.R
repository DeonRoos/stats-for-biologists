# Load libraries
library(dplyr)
library(tidyverse)
library(lubridate)


# Step 1: Load and process the lecture watchtime data
watchtime_data <- read.csv("Quizzes/2024_lecture_watchtime.csv")

# Convert date column to datetime and extract day
watchtime_data <- watchtime_data %>%
  mutate(date = dmy_hms(date),       # Convert to datetime format
         day = as.Date(date))        # Extract only the day (year-month-day)

# Aggregate total minutes by video, name, and day
aggregated_data <- watchtime_data %>%
  group_by(video, name, id, day) %>%
  summarise(total_minutes = sum(minutes, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Define lecture-to-quiz mapping, accounting for course iterativeness
aggregated_data <- aggregated_data %>%
  mutate(
    quiz = case_when(
      grepl("Lecture (1|1\\.5|2|2\\.5|3|3\\.5|4)", video) ~ "Q1",
      grepl("Lecture (5|5\\.5)", video) ~ "Q2",
      grepl("Lecture (7|7\\.5|8)", video) ~ "Q3",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(quiz))  # Keep only rows associated with quizzes

# Extend the mapping to account for lecture relevance across quizzes
# Q3 should include all previous lectures
aggregated_data <- aggregated_data %>%
  mutate(
    quiz = case_when(
      quiz == "Q1" ~ "Q1",
      quiz == "Q2" ~ "Q2",
      quiz == "Q3" ~ "Q3",
      #quiz == "Q1" & grepl("Lecture (1|1\\.5|2|2\\.5|3|3\\.5|4)", video) ~ "Q3",
      #quiz == "Q2" & grepl("Lecture (5|5\\.5)", video) ~ "Q3",
      TRUE ~ quiz
    )
  )

# Step 3: Aggregate total minutes watched by student and quiz
watchtime_summary <- aggregated_data %>%
  group_by(name, quiz) %>%
  summarise(total_minutes_watched = sum(total_minutes, na.rm = TRUE)) %>%
  ungroup()

# Step 4: Load and process quiz data
quiz_data <- read.csv("Quizzes/q1q2q3_2024.csv")

# Convert quiz data to long format
quiz_long <- quiz_data %>%
  pivot_longer(cols = Q1:Q3, names_to = "quiz", values_to = "grade") %>%
  mutate(quiz = factor(quiz, levels = c("Q1", "Q2", "Q3")),
         name = paste(first, last))  # Optional: create full name for reference

# Step 5: Merge quiz data with watchtime summary
df <- quiz_long %>%
  left_join(watchtime_summary, by = c("name", "quiz")) %>%
  mutate(total_minutes_watched = ifelse(is.na(total_minutes_watched), 0, total_minutes_watched))

ggplot(df) +
  geom_point(aes(x = total_minutes_watched, y = grade, colour = quiz)) +
  geom_smooth(aes(x = total_minutes_watched, y = grade), method  = "lm", colour = "black") +
  labs(y = "Grade in quiz",
       x = "Minutes spent watching lectures",
       colour = "Quiz",
       caption = "Lectures split into quiz topics") +
  theme_minimal()

df$grade_rnd <- round(df$grade, digits = 0)
df$max_grd <- 22

m1 <- glm(cbind(grade_rnd, max_grd - grade_rnd) ~ total_minutes_watched * quiz, 
          family = binomial,
          data = df)

plot(ggeffects::ggpredict(m1, terms = c("total_minutes_watched", "quiz")))

fake <- expand.grid(
  quiz = c("Q1", "Q2", "Q3"),
  total_minutes_watched = seq(from = min(df$total_minutes_watched),
                              to = max(df$total_minutes_watched),
                              by = 5))

preds <- predict(m1, newdata = fake, se.fit = TRUE)

fake$fit <- plogis(preds$fit)
fake$low <- plogis(preds$fit - preds$se.fit * 1.96)
fake$upp <- plogis(preds$fit + preds$se.fit * 1.96)

fake$fit <- fake$fit * 22
fake$low <- fake$low * 22
fake$upp <- fake$upp * 22

ggplot(fake) +
  geom_hline(yintercept = 9, linetype = 2) +
  geom_hline(yintercept = 12, linetype = 2) +
  geom_hline(yintercept = 15, linetype = 2) +
  geom_hline(yintercept = 18, linetype = 2) +
  geom_ribbon(aes(x = total_minutes_watched, ymin = low, ymax = upp, fill = quiz), alpha = 0.3) +
  geom_line(aes(x = total_minutes_watched, y = fit, colour = quiz)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(9,22), labels = c(9, 12, 15, 18, 22), breaks = c(9, 12, 15, 18, 22)) +
  labs(x = "Minutes of relevant lectures watched",
       colour = "Quiz",
       fill = "Quiz",
       y = "Predicted grade") +
  theme_minimal()
