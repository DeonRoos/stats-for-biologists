library(pdftools)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)

read_lines <- function(path) {
  pdf_text(path) |> paste(collapse = "\n") |> str_split("\n") |> unlist() |> str_squish()
}

first_after <- function(lines, start, pattern) {
  w <- lines[seq(start + 1, length(lines))]
  out <- w[str_detect(w, pattern)]
  if (length(out)) out[1] else NA_character_
}

parse_item <- function(lines, q_regex) {
  idx <- str_which(lines, q_regex)
  if (!length(idx)) return(NULL)
  i <- idx[1]
  perc_line <- first_after(lines, i, "(\\d+\\.?\\d*)%")
  labs_line <- first_after(lines, i, "(Totally|Not at all)")
  if (is.na(perc_line)) return(NULL)
  
  pct <- str_extract_all(perc_line, "(\\d+\\.?\\d*)%")[[1]] |>
    str_remove("%") |> as.numeric()
  
  labs <- if (!is.na(labs_line)) {
    str_split(str_squish(labs_line), "\\s{2,}")[[1]]
  } else {
    paste0("Cat_", seq_along(pct))
  }
  labs <- labs[seq_along(pct)]
  
  tibble(
    question = lines[i],
    option_order = seq_along(pct),
    option = labs,
    percent = pct / 100
  )
}

extract_likert_items <- function(path, question_patterns) {
  lines <- read_lines(path)
  map_dfr(question_patterns, ~parse_item(lines, .x))
}

extract_completion <- function(path) {
  txt <- pdf_text(path) |> paste(collapse = "\n")
  m <- str_match(txt, "Completion:\\s*(\\d+)\\/(\\d+)\\s*\\((\\d+\\.?\\d*)%\\)")
  tibble(
    n_completed = as.integer(m[2]),
    n_total = as.integer(m[3]),
    completion_rate = as.numeric(m[4]) / 100
  )
}

extract_academic_year <- function(path) {
  txt <- pdf_text(path) |> paste(collapse = "\n")
  m <- str_match(txt, "(20\\d{2})\\s*[–-]\\s*(\\d{2})")
  if (!is.na(m[1,1])) return(paste0(m[2], "-", m[3]))
  fn <- basename(path)
  m2 <- str_match(fn, "(20\\d{2})-(\\d{2})")
  if (!is.na(m2[1,1])) return(paste0(m2[2], "-", m2[3]))
  NA_character_
}

extract_course_meta <- function(path) {
  lines <- read_lines(path)
  i <- str_which(lines, "^[A-Z]{2}\\d{4}\\s+.+")[1]
  if (is.na(i)) return(tibble(course_code = NA_character_, course_title = NA_character_))
  s <- lines[i]
  code <- str_extract(s, "^[A-Z]{2}\\d{4}")
  title <- str_trim(str_remove(s, "^[A-Z]{2}\\d{4}\\s+"))
  tibble(course_code = code, course_title = title)
}

process_pdf <- function(path, question_patterns) {
  lk <- extract_likert_items(path, question_patterns)
  meta <- bind_cols(
    extract_course_meta(path),
    extract_completion(path),
    tibble(academic_year = extract_academic_year(path),
           file_id = basename(path),
           file_path = normalizePath(path, mustWork = FALSE))
  )
  bind_cols(lk, meta[rep(1, nrow(lk)),])
}

process_folder <- function(dir, question_patterns, recurse = FALSE) {
  files <- list.files(dir, pattern = "\\.pdf$", full.names = TRUE, recursive = recurse)
  map_dfr(files, ~process_pdf(.x, question_patterns))
}

largest_remainder <- function(p, n) {
  x <- p * n
  base <- floor(x)
  r <- x - base
  add <- rep(0L, length(p))
  if (sum(base) < n) {
    k <- n - sum(base)
    add[order(r, decreasing = TRUE)[seq_len(k)]] <- 1L
  }
  base + add
}

reconstruct_item <- function(df_item, n_resp) {
  counts <- largest_remainder(df_item$percent, n_resp)
  tibble(
    response = rep(df_item$option, counts)
  ) |>
    mutate(response = factor(response, levels = df_item$option, ordered = TRUE))
}

reconstruct_pseudo_individuals <- function(combined_df) {
  combined_df |>
    group_by(file_id, academic_year, course_code, course_title, question) |>
    arrange(option_order, .by_group = TRUE) |>
    group_modify(\(d, k) {
      n_k <- unique(d$n_completed)
      out <- reconstruct_item(d, n_k)
      out |> mutate(respondent_id = seq_len(nrow(out)))
    }) |>
    ungroup()
}

# Patterns tailored to this report family (extend/tweak as needed)
question_patterns <- c(
  "^Teaching was effective\\.$",
  "^Staff are good at explaining things\\.$",
  "^Staff have made the subject interesting\\.$",
  "^The course is intellectually stimulating\\.$",
  "^My course has challenged me to achieve my best work\\.$",
  "^Did you enjoy the course\\?$",
  "^How well has your course developed your knowledge and skills",
  "^My course has provided me with opportunities to explore ideas or concepts",
  "^My course has provided me with opportunities to apply what I have learnt",
  "^The criteria used in marking have been clear in advance\\.$",
  "^Marking and assessment has been fair\\.$",
  "^Feedback on my work has been timely\\.$",
  "^How often does feedback help you to improve your work\\?$",
  "^I have been able to contact staff when I needed to\\.$",
  "^The course is well organised and running smoothly\\.$",
  "^The timetable works efficiently for me\\.$",
  "^Any changes in the course or teaching have been communicated",
  "^I have had the right opportunities to provide feedback on my course\\.$",
  "^Staff value students' views and opinions about the course\\.$",
  "^It is clear how students' feedback on the course has been acted on\\.$"
)

# Example usage:
folder <- "C:/006-BI3010/Reviews"
combined <- process_folder(folder, question_patterns)
pseudo_df <- reconstruct_pseudo_individuals(combined)

# Example plot
combined |>
  filter(question == "Teaching was effective.") |>
  ggplot(aes(x = option, y = percent, fill = academic_year)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Proportion")
