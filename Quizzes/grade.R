# Analysis of BI3010 2024 grades based on lecture and practical 
# attendance, and minutes of lectures watched online.
# Deon Roos
# 31/10/2024


# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggeffects)
library(patchwork)

# Lecture recording watch time --------------------------------------------
watchtime_data <- read.csv("Quizzes/2024_lecture_watchtime.csv")

watchtime_data <- watchtime_data  |> 
  mutate(date = dmy_hms(date), day = as.Date(date)) |> 
  group_by(video, name, id, day) |> 
  summarise(total_minutes = sum(minutes, na.rm = TRUE)) |> 
  ungroup()

aggregated_data <- watchtime_data  |> 
  mutate(
    quiz = case_when(
      grepl("Lecture (1|1\\.5|2|2\\.5|3|3\\.5|4)", video) ~ "Q1",
      grepl("Lecture (5|5\\.5)", video) ~ "Q2",
      grepl("Lecture (7|7\\.5|8)", video) ~ "Q3",
      grepl("Lecture (9|9\\.5|10|10\\.5)", video) ~ "Q4",
      TRUE ~ NA_character_
    )
  ) |> 
  filter(!is.na(quiz))

watchtime_summary <- aggregated_data |> 
  group_by(name, quiz) |> 
  summarise(total_minutes_watched = sum(total_minutes, na.rm = TRUE)) |> 
  ungroup()


# Grades data -------------------------------------------------------------
quiz_data <- read.csv("Quizzes/quiz_grades_2024.csv")

quiz_long <- quiz_data |> 
  pivot_longer(cols = Q1:Q4, names_to = "quiz", values_to = "grade") |> 
  mutate(quiz = factor(quiz, levels = c("Q1", "Q2", "Q3", "Q4")),
         name = paste(first, last))


# Merge watchtime with grades ---------------------------------------------
df <- quiz_long |> 
  left_join(watchtime_summary, by = c("name", "quiz")) |> 
  mutate(total_minutes_watched = ifelse(is.na(total_minutes_watched), 0, total_minutes_watched))


# Attendance data ---------------------------------------------------------
attendance_data <- read.csv("Quizzes/2024_attendance.csv")

attendance_data <- attendance_data |> 
  separate(Name, into = c("last", "first"), sep = ", ", remove = FALSE) |> 
  mutate(name = paste(first, last)) |> 
  mutate(Visa.Status = ifelse(Visa.Status == "", "No visa", Visa.Status))

attendance_data <- attendance_data |> 
  rowwise() |> 
  mutate(
    lecture_attendance_Q1 = sum(str_count(c(L1, L2), "P|PSR|P,CC|PSR,CC")),
    lecture_attendance_Q2 = sum(str_count(c(L3), "P|PSR|P,CC|PSR,CC")),
    lecture_attendance_Q3 = sum(str_count(c(L4), "P|PSR|P,CC|PSR,CC")),
    lecture_attendance_Q4 = sum(str_count(c(L5), "P|PSR|P,CC|PSR,CC")),
    practical_attendance_Q1 = sum(str_count(c(CP1, CP2, P1, P2), "P|PSR|P,CC|PSR,CC")),
    practical_attendance_Q2 = sum(str_count(c(CP3, P3), "P|PSR|P,CC|PSR,CC")),
    practical_attendance_Q3 = sum(str_count(c(CP4, P4), "P|PSR|P,CC|PSR,CC")),
    practical_attendance_Q4 = sum(str_count(c(CP5, P5), "P|PSR|P,CC|PSR,CC"))
  ) |> 
  ungroup()

attendance_long <- attendance_data |> 
  select(name, id = ID, Visa.Status, starts_with("lecture_attendance"), 
         starts_with("practical_attendance")) |> 
  pivot_longer(cols = starts_with("lecture_attendance_"), 
               names_to = "quiz", 
               values_to = "lecture_attendance",
               names_prefix = "lecture_attendance_")  |> 
  pivot_longer(cols = starts_with("practical_attendance_"), 
               names_to = "quiz_prac", 
               values_to = "practical_attendance",
               names_prefix = "practical_attendance_") |> 
  filter(quiz == quiz_prac) |> 
  select(-quiz_prac)


# Merge attendance with grades --------------------------------------------
df <- df |> 
  left_join(attendance_long, by = c("name", "quiz")) |> 
  mutate(lecture_attendance = ifelse(is.na(lecture_attendance), 0, lecture_attendance),
         practical_attendance = ifelse(is.na(practical_attendance), 0, practical_attendance))

df$Visa.Status[is.na(df$Visa.Status)] <- "No visa"
df$grade[is.na(df$grade)] <- 0

# EDA ---------------------------------------------------------------------
ggplot(df) +
  geom_point(aes(x = total_minutes_watched, y = grade, colour = quiz), 
             show.legend = FALSE) +
  labs(y = "Grade in quiz",
       x = "Minutes spent watching relevant lecture recordings",
       colour = "Quiz",
       caption = "Lectures split into quiz topics") +
  theme_bw() +
  facet_wrap(~quiz, scales = "free_x")

ggplot(df) +
  geom_point(aes(x = lecture_attendance, y = grade, colour = quiz), 
             show.legend = FALSE) +
  labs(y = "Grade in quiz",
       x = "Lectures attended",
       colour = "Quiz",
       caption = "Lectures split into quiz topics") +
  theme_bw() +
  facet_wrap(~quiz, scales = "free_x")

ggplot(df) +
  geom_point(aes(x = practical_attendance, y = grade, colour = quiz), 
             show.legend = FALSE) +
  labs(y = "Grade in quiz",
       x = "Practicals attended",
       colour = "Quiz",
       caption = "Lectures split into quiz topics") +
  theme_bw() +
  facet_wrap(~quiz, scales = "free_x")

ggplot(df) +
  geom_boxplot(aes(x = Visa.Status, y = grade, fill = quiz)) +
  labs(y = "Grade in quiz",
       x = "International student",
       fill = "Quiz") +
  theme_minimal()

# Model: Three 2-way interactions -----------------------------------------

# Modelled as binomial with rounded grade

df$grade_rnd <- round(df$grade, digits = 0)
df$max_grd <- 22

mod_2ways <- glm(cbind(grade_rnd, max_grd - grade_rnd) ~ total_minutes_watched * quiz + 
                   lecture_attendance * quiz + 
                   practical_attendance * quiz, 
                 family = binomial,
                 data = df)

par(mfrow = c(2,2))
plot(mod_2ways)
summary(mod_2ways)
# Model fairly well behaved. Little bit of hetero.

# Manual figures ----------------------------------------------------------

# Minutes watching lecture recordings
fake <- expand.grid(
  quiz = c("Q1", "Q2", "Q3", "Q4"),
  total_minutes_watched = seq(from = min(df$total_minutes_watched),
                              to = max(df$total_minutes_watched),
                              by = 5),
  lecture_attendance = mean(df$lecture_attendance, na.rm = TRUE),
  practical_attendance = mean(df$practical_attendance, na.rm = TRUE)
)

preds <- predict(mod_2ways, newdata = fake, se.fit = TRUE)
fake$fit <- plogis(preds$fit) * 22
fake$low <- plogis(preds$fit - preds$se.fit * 1.96) * 22
fake$upp <- plogis(preds$fit + preds$se.fit * 1.96) * 22

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
  labs(x = "Minutes of relevant recorded lectures watched",
       colour = "Quiz",
       fill = "Quiz",
       y = "Predicted grade",
       caption = "Does not include time watching lectures relevant to previous quizzes") +
  theme_minimal() +
  facet_wrap(~quiz)

# Lectures
fake <- expand.grid(
  quiz = c("Q1", "Q2", "Q3", "Q4"),
  total_minutes_watched = mean(df$total_minutes_watched, na.rm = TRUE),
  lecture_attendance = seq(from = min(df$lecture_attendance, na.rm = TRUE),
                           to = max(df$lecture_attendance, na.rm = TRUE),
                           by = 1),
  practical_attendance = mean(df$practical_attendance, na.rm = TRUE)
)

preds <- predict(mod_2ways, newdata = fake, se.fit = TRUE)
fake$fit <- plogis(preds$fit) * 22
fake$low <- plogis(preds$fit - preds$se.fit * 1.96) * 22
fake$upp <- plogis(preds$fit + preds$se.fit * 1.96) * 22

ggplot(fake) +
  geom_hline(yintercept = 9, linetype = 2) +
  geom_hline(yintercept = 12, linetype = 2) +
  geom_hline(yintercept = 15, linetype = 2) +
  geom_hline(yintercept = 18, linetype = 2) +
  geom_ribbon(aes(x = lecture_attendance, ymin = low, ymax = upp, fill = quiz), alpha = 0.3) +
  geom_line(aes(x = lecture_attendance, y = fit, colour = quiz)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(7,22), labels = c(9, 12, 15, 18, 22), breaks = c(9, 12, 15, 18, 22)) +
  labs(x = "Number of lectures attended",
       colour = "Quiz",
       fill = "Quiz",
       y = "Predicted grade",
       caption = "Does not include lectures attended for previous quizzes") +
  theme_minimal() +
  facet_wrap(~quiz)

# Practicals
fake <- expand.grid(
  quiz = c("Q1", "Q2", "Q3", "Q4"),
  total_minutes_watched = mean(df$total_minutes_watched, na.rm = TRUE),
  lecture_attendance = mean(df$lecture_attendance, na.rm = TRUE),
  practical_attendance = seq(from = min(df$practical_attendance, na.rm = TRUE),
                             to = max(df$practical_attendance, na.rm = TRUE),
                             by = 1)
)

preds <- predict(mod_2ways, newdata = fake, se.fit = TRUE)
fake$fit <- plogis(preds$fit) * 22
fake$low <- plogis(preds$fit - preds$se.fit * 1.96) * 22
fake$upp <- plogis(preds$fit + preds$se.fit * 1.96) * 22

ggplot(fake) +
  geom_hline(yintercept = 9, linetype = 2) +
  geom_hline(yintercept = 12, linetype = 2) +
  geom_hline(yintercept = 15, linetype = 2) +
  geom_hline(yintercept = 18, linetype = 2) +
  geom_ribbon(aes(x = practical_attendance, ymin = low, ymax = upp, fill = quiz), alpha = 0.3) +
  geom_line(aes(x = practical_attendance, y = fit, colour = quiz)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(9,22), labels = c(9, 12, 15, 18, 22), breaks = c(9, 12, 15, 18, 22)) +
  labs(x = "Number of practicals attended",
       colour = "Quiz",
       fill = "Quiz",
       y = "Predicted grade",
       caption = "Does not include practicals attended for previous quizzes") +
  theme_minimal() +
  facet_wrap(~quiz)


# Cumulative time model ---------------------------------------------------

df <- df  |> 
  arrange(name, quiz)  |> 
  group_by(name)  |> 
  mutate(
    cumulative_lecture_attendance = cumsum(lecture_attendance),
    cumulative_practical_attendance = cumsum(practical_attendance),
    cumulative_total_minutes_watched = cumsum(total_minutes_watched)
  ) |> 
  ungroup()

mod_totaltime <- glm(cbind(grade_rnd, max_grd - grade_rnd) ~ cumulative_total_minutes_watched * quiz + 
            cumulative_lecture_attendance * quiz + 
            cumulative_practical_attendance * quiz, 
          family = binomial,
          data = df)

plot(mod_totaltime)
summary(mod_totaltime)
# As before - better than expected

plot(ggeffects::ggpredict(m1, terms = c("cumulative_total_minutes_watched", "quiz")))
plot(ggeffects::ggpredict(m1, terms = c("cumulative_lecture_attendance", "quiz")))
plot(ggeffects::ggpredict(m1, terms = c("cumulative_practical_attendance", "quiz")))

max_values <- df  |> 
  group_by(quiz) |> 
  summarize(
    max_cumulative_total_minutes_watched = max(cumulative_total_minutes_watched, na.rm = TRUE),
    max_cumulative_lecture_attendance = max(cumulative_lecture_attendance, na.rm = TRUE),
    max_cumulative_practical_attendance = max(cumulative_practical_attendance, na.rm = TRUE)
  )

# Total time watched
fake <- expand.grid(
  quiz = c("Q1", "Q2", "Q3", "Q4"),
  cumulative_total_minutes_watched = seq(
    from = min(df$cumulative_total_minutes_watched, na.rm = TRUE),
    to = max(df$cumulative_total_minutes_watched, na.rm = TRUE),
    by = 5
  )
)

fake <- fake |> 
  left_join(max_values, by = "quiz")

fake <- fake |> 
  mutate(
    cumulative_total_minutes_watched = pmin(cumulative_total_minutes_watched, max_cumulative_total_minutes_watched),
    cumulative_lecture_attendance = max_cumulative_lecture_attendance,
    cumulative_practical_attendance = max_cumulative_practical_attendance
  )  |> 
  select(-max_cumulative_total_minutes_watched, -max_cumulative_lecture_attendance, -max_cumulative_practical_attendance)

preds <- predict(mod_totaltime, newdata = fake, se.fit = TRUE)
fake$fit <- plogis(preds$fit) * 22
fake$low <- plogis(preds$fit - preds$se.fit * 1.96) * 22
fake$upp <- plogis(preds$fit + preds$se.fit * 1.96) * 22

ggplot(fake) +
  geom_hline(yintercept = 9, linetype = 2) +
  geom_hline(yintercept = 12, linetype = 2) +
  geom_hline(yintercept = 15, linetype = 2) +
  geom_hline(yintercept = 18, linetype = 2) +
  geom_ribbon(aes(x = cumulative_total_minutes_watched, ymin = low, ymax = upp, fill = quiz), alpha = 0.3) +
  geom_line(aes(x = cumulative_total_minutes_watched, y = fit, colour = quiz)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(9,22), labels = c(9, 12, 15, 18, 22), breaks = c(9, 12, 15, 18, 22)) +
  labs(x = "Cumulative minutes of relevant lectures watched",
       colour = "Quiz",
       fill = "Quiz",
       y = "Predicted grade") +
  theme_minimal() +
  facet_wrap(~quiz)

# Lecture attendance
fake <- expand.grid(
  quiz = c("Q1", "Q2", "Q3", "Q4"),
  cumulative_total_minutes_watched = mean(df$cumulative_total_minutes_watched, na.rm = TRUE),
  cumulative_lecture_attendance = seq(from = min(df$cumulative_lecture_attendance, na.rm = TRUE),
                                      to = max(df$cumulative_lecture_attendance, na.rm = TRUE),
                                      by = 1),
  cumulative_practical_attendance = mean(df$cumulative_practical_attendance, na.rm = TRUE)
)

fake <- fake |> 
  left_join(max_values, by = "quiz")

fake <- fake |> 
  mutate(
    cumulative_lecture_attendance = pmin(cumulative_lecture_attendance, max_cumulative_lecture_attendance),
    cumulative_total_minutes_watched = max_cumulative_total_minutes_watched,
    cumulative_practical_attendance = max_cumulative_practical_attendance
  ) |> 
  select(-max_cumulative_lecture_attendance, -max_cumulative_total_minutes_watched, -max_cumulative_practical_attendance)

preds <- predict(mod_totaltime, newdata = fake, se.fit = TRUE)
fake$fit <- plogis(preds$fit) * 22
fake$low <- plogis(preds$fit - preds$se.fit * 1.96) * 22
fake$upp <- plogis(preds$fit + preds$se.fit * 1.96) * 22

ggplot(fake) +
  geom_hline(yintercept = 9, linetype = 2) +
  geom_hline(yintercept = 12, linetype = 2) +
  geom_hline(yintercept = 15, linetype = 2) +
  geom_hline(yintercept = 18, linetype = 2) +
  geom_ribbon(aes(x = cumulative_lecture_attendance, ymin = low, ymax = upp, fill = quiz), alpha = 0.3) +
  geom_line(aes(x = cumulative_lecture_attendance, y = fit, colour = quiz)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(9,22), labels = c(9, 12, 15, 18, 22), breaks = c(9, 12, 15, 18, 22)) +
  labs(x = "Cumulative number of lectures attended",
       colour = "Quiz",
       fill = "Quiz",
       y = "Predicted grade") +
  theme_minimal() +
  facet_wrap(~quiz)

# Practicals
fake <- expand.grid(
  quiz = c("Q1", "Q2", "Q3", "Q4"),
  cumulative_total_minutes_watched = mean(df$cumulative_total_minutes_watched, na.rm = TRUE),
  cumulative_lecture_attendance = mean(df$cumulative_lecture_attendance, na.rm = TRUE),
  cumulative_practical_attendance = seq(from = min(df$cumulative_practical_attendance, na.rm = TRUE),
                                        to = max(df$cumulative_practical_attendance, na.rm = TRUE),
                                        by = 1)
)

fake <- fake |> 
  left_join(max_values, by = "quiz")

fake <- fake |> 
  mutate(
    cumulative_practical_attendance = pmin(cumulative_practical_attendance, max_cumulative_practical_attendance),
    cumulative_total_minutes_watched = max_cumulative_total_minutes_watched,
    cumulative_lecture_attendance = max_cumulative_lecture_attendance
  ) |> 
  select(-max_cumulative_lecture_attendance, -max_cumulative_total_minutes_watched, -max_cumulative_practical_attendance)

preds <- predict(mod_totaltime, newdata = fake, se.fit = TRUE)
fake$fit <- plogis(preds$fit) * 22
fake$low <- plogis(preds$fit - preds$se.fit * 1.96) * 22
fake$upp <- plogis(preds$fit + preds$se.fit * 1.96) * 22

ggplot(fake) +
  geom_hline(yintercept = 9, linetype = 2) +
  geom_hline(yintercept = 12, linetype = 2) +
  geom_hline(yintercept = 15, linetype = 2) +
  geom_hline(yintercept = 18, linetype = 2) +
  geom_ribbon(aes(x = cumulative_practical_attendance, ymin = low, ymax = upp, fill = quiz), alpha = 0.3) +
  geom_line(aes(x = cumulative_practical_attendance, y = fit, colour = quiz)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(9,22), labels = c(9, 12, 15, 18, 22), breaks = c(9, 12, 15, 18, 22)) +
  labs(x = "Cumulative number of practicals attended",
       colour = "Quiz",
       fill = "Quiz",
       y = "Predicted grade") +
  theme_bw() +
  facet_wrap(~quiz)




# Final grade -------------------------------------------------------------

grades_data <- read.csv("Quizzes/course_grades_2024.csv")

grades_data <- grades_data |> 
  mutate(name = paste(first, last))  |> 
  select(-first, -last)

totals_summary <- df |> 
  group_by(name) |> 
  summarise(
    total_minutes_watched = sum(total_minutes_watched, na.rm = TRUE),
    total_lecture_attendance = sum(lecture_attendance, na.rm = TRUE),
    total_practical_attendance = sum(practical_attendance, na.rm = TRUE),
    visa = unique(Visa.Status)
  )

grade_df <- grades_data |> 
  left_join(totals_summary, by = "name")

grade_df$grade[is.na(grade_df$grade)] <- 0
grade_df$grade_rnd <- round(grade_df$grade, digits = 0)
grade_df$max_grd <- 22

final_mod <- glm(cbind(grade_rnd, max_grd - grade_rnd) ~ total_minutes_watched + 
                   total_lecture_attendance + 
                   total_practical_attendance, 
                 family = binomial,
                 data = grade_df)

grade_df$predicted <- round(plogis(predict(final_mod)) * 22, digits = 0)
grade_df$resids <- grade_df$grade_rnd - grade_df$predicted

ggplot(grade_df) +
  geom_point(aes(x = predicted, y = resids))

ggplot(grade_df) +
  geom_boxplot(aes(x = visa, y = grade_rnd))

# Minutes watching lecture recordings
fake <- expand.grid(
  total_minutes_watched = seq(from = min(grade_df$total_minutes_watched),
                              to = max(grade_df$total_minutes_watched),
                              by = 5),
  total_lecture_attendance = median(grade_df$total_lecture_attendance, na.rm = TRUE),
  total_practical_attendance = median(grade_df$total_practical_attendance, na.rm = TRUE)
)

preds <- predict(final_mod, newdata = fake, se.fit = TRUE)
fake$fit <- plogis(preds$fit) * 22
fake$low <- plogis(preds$fit - preds$se.fit * 1.96) * 22
fake$upp <- plogis(preds$fit + preds$se.fit * 1.96) * 22

p1 <- ggplot(fake) +
  geom_hline(yintercept = 9, linetype = 2) +
  geom_hline(yintercept = 12, linetype = 2) +
  geom_hline(yintercept = 15, linetype = 2) +
  geom_hline(yintercept = 18, linetype = 2) +
  geom_ribbon(aes(x = total_minutes_watched, ymin = low, ymax = upp), alpha = 0.3) +
  geom_line(aes(x = total_minutes_watched, y = fit)) +
  scale_y_continuous(limits = c(8,22), labels = c(9, 12, 15, 18, 22), breaks = c(9, 12, 15, 18, 22)) +
  labs(x = "Total time spent watching\nrecorded lectures (mins)",
       y = "Predicted final grade",
       caption = paste("Assuming student attends", 
                       median(grade_df$total_lecture_attendance, na.rm = TRUE),
                       "lectures and",
                       median(grade_df$total_practical_attendance, na.rm = TRUE),
                       "practicals")) +
  theme_bw()

fake <- expand.grid(
  total_minutes_watched = median(grade_df$total_minutes_watched, na.rm = TRUE),
  total_lecture_attendance = seq(from = min(grade_df$total_lecture_attendance),
                                 to = max(grade_df$total_lecture_attendance),
                                 by = 1),
  total_practical_attendance = median(grade_df$total_practical_attendance, na.rm = TRUE)
)

preds <- predict(final_mod, newdata = fake, se.fit = TRUE)
fake$fit <- plogis(preds$fit) * 22
fake$low <- plogis(preds$fit - preds$se.fit * 1.96) * 22
fake$upp <- plogis(preds$fit + preds$se.fit * 1.96) * 22

p2 <- ggplot(fake) +
  geom_hline(yintercept = 9, linetype = 2) +
  geom_hline(yintercept = 12, linetype = 2) +
  geom_hline(yintercept = 15, linetype = 2) +
  geom_hline(yintercept = 18, linetype = 2) +
  geom_ribbon(aes(x = total_lecture_attendance, ymin = low, ymax = upp), alpha = 0.3) +
  geom_line(aes(x = total_lecture_attendance, y = fit)) +
  scale_y_continuous(limits = c(8,22), labels = c(9, 12, 15, 18, 22), breaks = c(9, 12, 15, 18, 22)) +
  labs(x = "Total lectures attended",
       y = "Predicted final grade",
       caption = paste("Assuming student attends", 
                       median(grade_df$total_practical_attendance, na.rm = TRUE),
                       "practicals\nand watches",
                       round(median(grade_df$total_minutes_watched, na.rm = TRUE), digits = 1),
                       "of lecture recordings")) +
  theme_bw()


fake <- expand.grid(
  total_minutes_watched = median(grade_df$total_minutes_watched, na.rm = TRUE),
  total_practical_attendance = seq(from = min(grade_df$total_practical_attendance),
                                 to = max(grade_df$total_practical_attendance),
                                 by = 1),
  total_lecture_attendance = median(grade_df$total_lecture_attendance, na.rm = TRUE)
)

preds <- predict(final_mod, newdata = fake, se.fit = TRUE)
fake$fit <- plogis(preds$fit) * 22
fake$low <- plogis(preds$fit - preds$se.fit * 1.96) * 22
fake$upp <- plogis(preds$fit + preds$se.fit * 1.96) * 22

p3 <- ggplot(fake) +
  geom_hline(yintercept = 9, linetype = 2) +
  geom_hline(yintercept = 12, linetype = 2) +
  geom_hline(yintercept = 15, linetype = 2) +
  geom_hline(yintercept = 18, linetype = 2) +
  geom_ribbon(aes(x = total_practical_attendance, ymin = low, ymax = upp), alpha = 0.3) +
  geom_line(aes(x = total_practical_attendance, y = fit)) +
  scale_y_continuous(limits = c(8,22), labels = c(9, 12, 15, 18, 22), breaks = c(9, 12, 15, 18, 22)) +
  labs(x = "Total practicals attended",
       y = "Predicted final grade",
       caption = paste("Assuming student attends", 
                       median(grade_df$total_lecture_attendance, na.rm = TRUE),
                       "lectures and\nwatches",
                       round(median(grade_df$total_minutes_watched, na.rm = TRUE), digits = 1),
                       "of lecture recordings")) +
  theme_bw()

p1 / p2 / p3


grade_df[grade_df$grade >= quantile(grade_df$grade, probs = 0.9, na.rm = TRUE),]
grade_df[grade_df$grade <= quantile(grade_df$grade, probs = 0.1, na.rm = TRUE),]
grade_df[grade_df$name == "JACK MACDONALD",]
