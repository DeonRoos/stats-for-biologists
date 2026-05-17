library(ggplot2)
theme_set(theme_classic())
library(tidyr)
library(dplyr)
library(mgcv)
library(patchwork)

# Data --------------------------------------------------------------------

df <- read.csv("C:/006-BI3010/Grades/2025_FullGrades.csv", header = TRUE)
df1 <- read.csv("C:/006-BI3010/Grades/2024_FullGrades.csv", header = TRUE)

head(df)
head(df1)

colnames(df) <- c("last", "first", "user", "id", "access", "avail", "grade", "q1", "q2", "q3", "q4")
colnames(df1) <- c("last", "first", "user", "id", "access", "avail", "grade", "q1", "q2", "q4", "q3")

df$year <- 2025
df1$year <- 2024

df$grade[df$grade == "MC"] <- NA
df$grade[df$grade == ""] <- NA
df$grade <- as.numeric(df$grade)
df$grade[df$grade == 0] <- NA

df <- bind_rows(df, df1)

p1 <- ggplot(df) +
  geom_histogram(aes(x = grade, fill = factor(year)), position = position_dodge(), colour = "white") +
  labs(x = "Final grade", y = "Frequency", fill = "Year") +
  scale_fill_brewer(palette = "Dark2")

df <- df |>
  pivot_longer(
    cols = q1:q4,
    names_to = "quiz",
    values_to = "score"
  ) |>
  mutate(quiz = case_when(
    quiz == "q1" ~ 1,
    quiz == "q2" ~ 2,
    quiz == "q3" ~ 3,
    quiz == "q4" ~ 4
  ))

df$score_r <- round(df$score, digits = 0)
df$quiz_f <- factor(df$quiz)
df$year_f <- factor(df$year)

grade_labels <- data.frame(
  y = c(9, 12, 15, 18),
  label = c("D", "C", "B", "A")
)

# EDA ---------------------------------------------------------------------

p2 <- ggplot(df) +
  geom_violin(
    aes(x = factor(quiz), y = score, fill = factor(year_f)),
    draw_quantiles = c(0.25, 0.5, 0.75),
    show.legend = FALSE,
    alpha = 0.9
  ) +
  
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "Quiz", y = "Grade", fill = "Year",
    caption = "Solid lines are the 25%, 50% and 75% percentiles."
  ) +
  facet_wrap(~year, ncol = 1)

# Models ------------------------------------------------------------------

m <- glm(score_r ~ quiz_f * year,
  family = poisson,
  data = df
)

nu <- expand.grid(
  quiz_f = unique(df$quiz_f),
  year = 2024:2025
)

pred <- predict(m, newdata = nu, se.fit = TRUE)

nu$fit <- exp(pred$fit)
nu$low <- exp(pred$fit - pred$se.fit * 1.96)
nu$upp <- exp(pred$fit + pred$se.fit * 1.96)

p3 <- ggplot(nu) +
  geom_errorbar(
    aes(x = quiz_f, ymin = low, ymax = upp, color = factor(year)),
    position = position_dodge(width = 0.4),
    width = 0,
    alpha = 0.8, 
    show.legend = FALSE
  ) +
  geom_point(
    aes(x = quiz_f, y = fit, color = factor(year)),
    position = position_dodge(width = 0.4),
    size = 2, 
    show.legend = FALSE
  ) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Quiz", y = "Predicted grade", color = "Year") +
  theme_classic()

df$grade_r <- round(df$grade, digits = 0)

m1 <- glm(grade_r ~ year_f,
          family = poisson,
          data = df
)

nu <- expand.grid(
  year_f = unique(df$year_f)
)

pred <- predict(m1, newdata = nu, se.fit = TRUE)

nu$fit <- exp(pred$fit)
nu$low <- exp(pred$fit - pred$se.fit * 1.96)
nu$upp <- exp(pred$fit + pred$se.fit * 1.96)

p4 <- ggplot(nu) +
  geom_errorbar(
    aes(x = year_f, ymin = low, ymax = upp,
        colour = year_f),
    width = 0,
    alpha = 0.8, 
    show.legend = FALSE
  ) +
  geom_point(
    aes(x = year_f, y = fit, colour = year_f),
    size = 2, 
    show.legend = FALSE
  ) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Year", 
       colour = "Year",
       y = "Predicted final grade") +
  theme_classic()


p5 <- (p1 + p2) / (p4 + p3) + plot_layout(guides = "collect")

ggsave("C:/006-BI3010/Grades/2024+2025.png", p5, width = 10, height = 10, dpi = 300)












# Effect of material ------------------------------------------------------

grade_24 <- read.csv("C:/006-BI3010/Grades/Data/2024_FullGrades.csv", header = TRUE)
grade_25 <- read.csv("C:/006-BI3010/Grades/Data/2025_FullGrades.csv", header = TRUE)

colnames(grade_24) <- c("last", "first", "user", "id", "access", "avail", "grade", "q1", "q2", "q4", "q3")
colnames(grade_25) <- c("last", "first", "user", "id", "access", "avail", "grade", "q1", "q2", "q3", "q4")

grade_24$year <- 2024
grade_25$year <- 2025

grade_24$grade[grade_24$grade == "MC"] <- NA
grade_24$grade[grade_24$grade == ""] <- NA
grade_24$grade <- as.numeric(grade_24$grade)
grade_24$grade[grade_24$grade == 0] <- NA

grade_25$grade[grade_25$grade == "MC"] <- NA
grade_25$grade[grade_25$grade == ""] <- NA
grade_25$grade <- as.numeric(grade_25$grade)
grade_25$grade[grade_25$grade == 0] <- NA

attend_24 <- read.csv("C:/006-BI3010/Grades/Data/BI3010_attendance_2024_25.csv", header = TRUE)
lecture_24 <- read.csv("C:/006-BI3010/Grades/Data/BI3010_lecture_2024_25.csv", header = TRUE)
attend_25 <- read.csv("C:/006-BI3010/Grades/Data/BI3010_attendance_2025_26.csv", header = TRUE)
lecture_25 <- read.csv("C:/006-BI3010/Grades/Data/BI3010_lecture_2025_26.csv", header = TRUE)

grade_24$year <- 2024
attend_24$year <- 2024
lecture_24$year <- 2024

grade_25$year <- 2025
attend_25$year <- 2025
lecture_25$year <- 2025


# Attendance processing ---------------------------------------------------
attendance_summary <- function(d) {
  present_tokens <- c("P", "PSR", "L")
  count_pa <- function(x) {
    if (is.na(x) || x == "") return(c(present = 0L, absent = 0L))
    tok <- strsplit(x, ",", fixed = TRUE)[[1]] |> trimws()
    tok <- tok[tok != ""]
    if (!length(tok)) return(c(present = 0L, absent = 0L))
    c(present = sum(tok %in% present_tokens),
      absent  = sum(tok == "UA" | startsWith(tok, "AA")))
  }
  sum_over <- function(cols, which) {
    if (!length(cols)) return(rep(0L, nrow(d)))
    parts <- lapply(cols, \(nm) sapply(d[[nm]], \(z) count_pa(z)[which]))
    Reduce(`+`, parts)
  }
  
  l_cols  <- grep("^L(9|1[0-3])$", names(d), value = TRUE)
  p_cols  <- grep("^P(9|1[0-3])$", names(d), value = TRUE)
  cp_cols <- grep("^CP(9|1[0-3])$", names(d), value = TRUE)
  pr_cols <- c(p_cols, cp_cols)
  
  d |>
    mutate(
      lec_present  = sum_over(l_cols,  "present"),
      lec_absent   = sum_over(l_cols,  "absent"),
      prac_present = sum_over(pr_cols, "present"),
      prac_absent  = sum_over(pr_cols, "absent")
    ) |>
    select(Name, ID, year, lec_present, lec_absent, prac_present, prac_absent)
}

df_2024 <- attendance_summary(attend_24)
df_2025 <- attendance_summary(attend_25)

canon_name <- function(x, style = c("last_first", "first_last")) {
  style <- match.arg(style)
  y <- trimws(tolower(x))
  y[y %in% c("", "anonymous")] <- NA
  out <- rep(NA_character_, length(y))
  if (style == "last_first") {
    has <- !is.na(y) & grepl(",", y, fixed = TRUE)
    parts <- strsplit(y[has], ",", fixed = TRUE)
    lf <- vapply(parts, \(p) {
      last <- trimws(p[1]); rest <- trimws(p[2])
      first <- strsplit(rest, "\\s+")[[1]][1]
      paste(first, last)
    }, character(1))
    out[has] <- lf
  } else {
    has <- !is.na(y)
    parts <- strsplit(y[has], "\\s+")
    fl <- vapply(parts, \(p) if (length(p) >= 2) paste(p[1], p[length(p)]) else NA_character_, character(1))
    out[has] <- fl
  }
  out
}

combine_by_name <- function(att, lec, att_name_col = "Name", lec_name_col = "Name") {
  stopifnot(all(c(att_name_col, "year") %in% names(att)),
            all(c(lec_name_col, "Minutes.Delivered", "year") %in% names(lec)))
  att2 <- att |>
    mutate(join_name = canon_name(.data[[att_name_col]], "last_first"))
  lec2 <- lec |>
    transmute(
      join_name = canon_name(.data[[lec_name_col]], "first_last"),
      minutes_delivered = Minutes.Delivered,
      year = year
    )
  att2 |>
    left_join(lec2, by = c("join_name", "year")) |>
    select(-join_name)
}

df_2025_joined <- combine_by_name(df_2025, lecture_25, 
                                  att_name_col = "Name", 
                                  lec_name_col = "Name")

df_2024_joined <- combine_by_name(df_2024, lecture_24, 
                                  att_name_col = "Name", 
                                  lec_name_col = "Name")

df <- rbind(df_2025_joined, df_2024_joined)

prep_grade <- function(d) {
  d |>
    transmute(
      ID = as.integer(id),
      year = as.integer(year),
      grade, q1, q2, q3, q4
    )
}

grades <- dplyr::bind_rows(prep_grade(grade_24), prep_grade(grade_25))

df <- df |>
  dplyr::left_join(grades, by = c("ID", "year"))

df$grade <- round(df$grade, digits = 0)
df$year_f <- factor(df$year)

# Replace NAs in watchtime with zeros (anonymous will be incorrectly assigned zero)
df$minutes_delivered[is.na(df$minutes_delivered)] <- 0


# Analysis ----------------------------------------------------------------

library(lme4)

mod <- glm(cbind(grade, 22 - grade) ~ year_f * lec_present + year_f * prac_present + year_f * minutes_delivered,
             data = df,
             family = binomial)


nu <- data.frame(
  year_f = unique(df$year_f),
  lec_present = median(df$lec_present),
  prac_present = median(df$prac_present),
  minutes_delivered = median(df$minutes_delivered, na.rm = TRUE)
)

preds <- predict(mod, newdata = nu, se.fit = TRUE)

nu$fit <- plogis(preds$fit) * 22
nu$low <- plogis(preds$fit - 1.96 * preds$se.fit) * 22
nu$upp <- plogis(preds$fit +  1.96 * preds$se.fit) * 22

p1 <- ggplot(nu, aes(x = year_f, ymin = low, ymax = upp, y = fit)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = c(9, 12, 15, 18, 22),
             alpha = 0.2) +
  scale_y_continuous(limits = c(8, 22), labels = c(9, 12, 15, 18, 22), breaks = c(9, 12, 15, 18, 22)) +
  labs(x = "Year", 
       y = "Predicted grade")



nu <- expand.grid(
  year_f = unique(df$year_f),
  lec_present = seq(from = min(df$lec_present), 
                    to = max(df$lec_present),
                    length.out = 20),
  prac_present = median(df$prac_present),
  minutes_delivered = median(df$minutes_delivered, na.rm = TRUE)
)

preds <- predict(mod, newdata = nu, se.fit = TRUE)

nu$fit <- plogis(preds$fit) * 22
nu$low <- plogis(preds$fit - 1.96 * preds$se.fit) * 22
nu$upp <- plogis(preds$fit +  1.96 * preds$se.fit) * 22

p2 <- ggplot(nu, aes(x = lec_present, ymin = low, ymax = upp, y = fit,
                     colour = year_f, fill = year_f)) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(alpha = 0.3, colour = NA) +
  geom_hline(yintercept = c(9, 12, 15, 18, 22),
             alpha = 0.2) +
  scale_y_continuous(limits = c(8, 22), labels = c(9, 12, 15, 18, 22), 
                     breaks = c(9, 12, 15, 18, 22)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Lectures present for", 
       y = "Predicted grade",
       colour = "Year",
       fill = "Year")


nu <- expand.grid(
  year_f = unique(df$year_f),
  lec_present = median(df$lec_present),
  prac_present = seq(from = min(df$prac_present), 
                     to = max(df$prac_present),
                     length.out = 20),
  minutes_delivered = median(df$minutes_delivered, na.rm = TRUE)
)

preds <- predict(mod, newdata = nu, se.fit = TRUE)

nu$fit <- plogis(preds$fit) * 22
nu$low <- plogis(preds$fit - 1.96 * preds$se.fit) * 22
nu$upp <- plogis(preds$fit +  1.96 * preds$se.fit) * 22

p3 <- ggplot(nu, aes(x = prac_present, ymin = low, ymax = upp, y = fit,
               colour = year_f, fill = year_f)) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(alpha = 0.3, colour = NA) +
  geom_hline(yintercept = c(9, 12, 15, 18, 22),
             alpha = 0.2) +
  scale_y_continuous(limits = c(8, 22), labels = c(9, 12, 15, 18, 22), 
                     breaks = c(9, 12, 15, 18, 22)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Practicals present for", 
       y = "Predicted grade",
       colour = "Year",
       fill = "Year")



nu <- expand.grid(
  year_f = unique(df$year_f),
  lec_present = median(df$lec_present),
  prac_present = median(df$prac_present),
  minutes_delivered = seq(from = min(df$minutes_delivered, na.rm = TRUE), 
                          to = max(df$minutes_delivered, na.rm = TRUE),
                          length.out = 20)
)

preds <- predict(mod, newdata = nu, se.fit = TRUE)

nu$fit <- plogis(preds$fit) * 22
nu$low <- plogis(preds$fit - 1.96 * preds$se.fit) * 22
nu$upp <- plogis(preds$fit +  1.96 * preds$se.fit) * 22

p4 <- ggplot(nu, aes(x = minutes_delivered, ymin = low, ymax = upp, y = fit,
               colour = year_f, fill = year_f)) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(alpha = 0.3, colour = NA) +
  geom_hline(yintercept = c(9, 12, 15, 18, 22),
             alpha = 0.2) +
  scale_y_continuous(limits = c(8, 22), labels = c(9, 12, 15, 18, 22), 
                     breaks = c(9, 12, 15, 18, 22)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Minutes of recordings watched", 
       y = "Predicted grade",
       colour = "Year",
       fill = "Year")

(p1 + p2) / (p3 + p4) + 
  plot_layout(guides = "collect") +
  plot_annotation(
    caption = "N = 265.\nN.B.12 students who watched lectures anonymously are assigned zero watchtime"
  )




df <- read.csv("C:/006-BI3010/Reviews/2025_comments.csv", header = TRUE)

library(dplyr)
library(tidytext)
library(wordcloud)
library(tm)

df <- df |>
  select(!starts_with("X")) |>
  pivot_longer(everything(), names_to = "question", values_to = "text") |>
  filter(!is.na(text), text != "")

custom_stop <- c("maybe", "like", "just", "really", "can", "get", "much", "many", "also", "well",
                 "instead", "similar", "time", "think", "things", "one", "less", "example", "instead",
                 "ones", "work", "better", "lot", "run", "working", "super", "bit", "quite", "sessions",
                 "sometimes", "actually", "need", "first", "way", "doesn")

words <- df |>
  unnest_tokens(word, text) |>
  filter(!word %in% stopwords::stopwords("en"),
         !word %in% custom_stop,
         !grepl("^[0-9]+$", word),
         nchar(word) > 2) |>
  count(word, sort = TRUE)

words <- words |>
  filter(!word %in% stopwords("en"))

wordcloud(words = words$word, freq = words$n, max.words = 50)
