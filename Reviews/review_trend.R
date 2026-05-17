library(shiny)
library(miniUI)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(readr)
library(stringr)

largest_remainder <- function(p, n) {
  x <- p * n
  base <- floor(x)
  r <- x - base
  need <- n - sum(base)
  if (need > 0) base[order(r, decreasing = TRUE)[seq_len(need)]] <- base[order(r, decreasing = TRUE)[seq_len(need)]] + 1L
  base
}

reconstruct_item <- function(levels, perc, n) {
  perc <- replace_na(perc, 0)
  counts <- largest_remainder(perc / 100, n)
  tibble(response = rep(levels, counts)) |>
    mutate(response = factor(response, levels = levels, ordered = TRUE))
}

make_question_panel <- function(id, q, levels) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      h4(q),
      div(
        style = "display:flex; gap:12px; flex-wrap:wrap;",
        !!!setNames(
          lapply(seq_along(levels), function(i) {
            numericInput(
              inputId = ns(paste0("p_", i)),
              label = levels[i],
              value = 0, min = 0, max = 100, step = 0.01, width = "140px"
            )
          }),
          NULL
        ),
        span(textOutput(ns("sum_txt")), style = "margin-left:8px; font-weight:600;")
      )
    )
  )
}

question_server <- function(id, q, levels) {
  moduleServer(id, function(input, output, session) {
    vals <- reactive({
      setNames(
        vapply(seq_along(levels), function(i) as.numeric(input[[paste0("p_", i)]]), numeric(1)),
        levels
      )
    })
    output$sum_txt <- renderText({
      s <- sum(vals(), na.rm = TRUE)
      paste0("Sum: ", sprintf("%.2f", s), "%")
    })
    list(
      question = reactive(q),
      perc = vals
    )
  })
}

launch_crf_gadget <- function(questions, likert_levels) {
  ui <- miniPage(
    gadgetTitleBar("CRF entry"),
    miniContentPanel(
      fluidRow(
        column(12,
               textInput("course", "Course code (e.g., BI3010)", "", width = "220px"),
               textInput("academic_year", "Academic year (e.g., 2024-25)", "", width = "220px"),
               numericInput("n_total", "Total students", value = NA, min = 1, step = 1, width = "180px"),
               numericInput("n_completed", "Respondents", value = NA, min = 0, step = 1, width = "180px")
        )
      ),
      tags$hr(),
      uiOutput("question_blocks"),
      tags$hr(),
      fluidRow(
        column(12,
               downloadButton("download_summary", "Download summary CSV"),
               downloadButton("download_pseudo", "Download pseudo individuals CSV"),
               verbatimTextOutput("check_msg")
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    mods <- reactiveVal(NULL)
    
    output$question_blocks <- renderUI({
      panels <- lapply(seq_along(questions), function(i) {
        make_question_panel(paste0("q", i), questions[i], likert_levels)
      })
      do.call(tagList, panels)
    })
    
    observeEvent(TRUE, {
      modules <- lapply(seq_along(questions), function(i) {
        question_server(paste0("q", i), questions[i], likert_levels)
      })
      mods(modules)
    }, once = TRUE)
    
    summary_df <- reactive({
      req(mods())
      n_comp <- req(as.integer(input$n_completed))
      mods() |>
        imap(~{
          tibble(
            question = .x$question(),
            option = names(.x$perc()),
            percent = unname(.x$perc())
          ) |>
            mutate(
              option_order = match(option, likert_levels),
              n_completed = n_comp,
              n_total = as.integer(input$n_total),
              course = input$course,
              academic_year = input$academic_year
            )
        }) |>
        list_rbind() |>
        arrange(question, option_order)
    })
    
    pseudo_df <- reactive({
      s <- summary_df()
      req(nrow(s))
      
      s |>
        group_by(question) |>
        group_modify(\(d, k) {
          levels   <- likert_levels
          perc_vec <- d$percent[match(levels, d$option)]
          perc_vec[is.na(perc_vec)] <- 0
          n_k <- unique(d$n_completed)
          
          reconstruct_item(levels = levels, perc = perc_vec, n = n_k) |>
            mutate(respondent_id = row_number())
        }) |>
        ungroup() |>
        mutate(academic_year = input$academic_year, .before = 1) |>
        relocate(question, respondent_id, response, academic_year)
    })
    
    output$check_msg <- renderText({
      s <- summary_df()
      if (!nrow(s)) return("")
      chk <- s |>
        group_by(question) |>
        summarise(sum_pct = sum(replace_na(percent, 0)), .groups = "drop")
      bad <- chk |> filter(abs(sum_pct - 100) > 0.5)
      if (nrow(bad)) {
        paste0(
          "Warning: sums not ~100% for:\n",
          paste0(" - ", bad$question, " (", sprintf("%.2f", bad$sum_pct), "%)", collapse = "\n")
        )
      } else {
        "All question totals ≈ 100%."
      }
    })
    
    output$download_summary <- downloadHandler(
      filename = function() paste0("crf_summary_", Sys.Date(), ".csv"),
      content = function(file) write_csv(summary_df(), file)
    )
    
    output$download_pseudo <- downloadHandler(
      filename = function() paste0("crf_pseudo_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(pseudo_df(), file)
    )
    
    observeEvent(input$done, {
      stopApp(invisible(list(summary = summary_df(), pseudo = pseudo_df())))
    })
    observeEvent(input$cancel, { stopApp(invisible(NULL)) })
  }
  
  viewer <- dialogViewer("CRF entry", width = 1000, height = 700)
  runGadget(ui, server, viewer = viewer)
}

result <- launch_crf_gadget(
  questions = c(
    # "Teaching was effective.",
    "Staff are good at explaining things.",
    "Staff have made the subject interesting.",
    "The course is intellectually stimulating.",
    "My course has challenged me to achieve my best work.",
    # "Did you enjoy the course?",
    "My course has provided me with opportunities to explore ideas or concepts in depth."
  ),
  likert_levels = c("Totally","4","3","2","Not at all")
)

# After pressing 'Done' in the pop-up
names(result)
# [1] "summary" "pseudo"

# Access the data frames
result$summary |> dplyr::glimpse()
result$pseudo  |> dplyr::count(question, response)

yr25 <- read.csv("C:/006-BI3010/Reviews/crf_pseudo_2025.csv", header = TRUE)
yr24 <- read.csv("C:/006-BI3010/Reviews/crf_pseudo_2024.csv", header = TRUE)
yr23 <- read.csv("C:/006-BI3010/Reviews/crf_pseudo_2023.csv", header = TRUE)
yr22 <- read.csv("C:/006-BI3010/Reviews/crf_pseudo_2022.csv", header = TRUE)
yr21 <- read.csv("C:/006-BI3010/Reviews/crf_pseudo_2021.csv", header = TRUE)
yr20 <- read.csv("C:/006-BI3010/Reviews/crf_pseudo_2020.csv", header = TRUE)
yr18 <- read.csv("C:/006-BI3010/Reviews/crf_pseudo_2018.csv", header = TRUE)

df <- rbind(yr18, yr20, yr21, yr22, yr23, yr24, yr25)

head(df)

questions_to_drop <- c(
  "Teaching was effective.",
  "Did you enjoy the course?"
)

df <- df |>
  dplyr::filter(!question %in% questions_to_drop)

library(ggplot2)
library(dplyr)
library(patchwork)
library(nnet)

df <- df |>
  mutate(
    year_mid = as.numeric(str_sub(academic_year, 1, 4)) + 0.5,
    response_num = as.numeric(response),
    question = factor(question)
  )

df_plot <- df |>
  count(question, academic_year, response, name = "n_resp") |>
  left_join(df |> 
              count(question, academic_year, name = "n_responded"), by = c("question", "academic_year")) |>
  mutate(
    prop = n_resp / n_responded,
    year_mid = as.numeric(substr(academic_year, 1, 4))
  )

df_plot$response <- factor(df_plot$response, levels = c("Not at all", "2", "3", "4", "Totally"))

p1 <- ggplot(df_plot, aes(x = factor(year_mid), y = prop, fill = response)) +
  geom_col() +
  facet_wrap(~question, ncol = 2) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D") +
  labs(x = "", 
       y = "Percent of respondants",
       fill = "Likert\nscale") +
  theme_bw()

p2 <- ggplot(df_plot, aes(x = year_mid, y = prop, group = response)) +
  geom_line(aes(colour = response), linewidth = 1) +
  geom_point(aes(colour = response)) +
  geom_vline(xintercept = 2023.5, linetype = "dashed", colour = "black") +
  facet_wrap(~question, ncol = 2) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_viridis_d(option = "D") +
  labs(x = "", 
       y = "Percent of respondants",
       colour = "Likert\nscale") +
  theme_bw()

# agg <- df |>
#   count(question, academic_year, response) |>
#   tidyr::pivot_wider(names_from = response, values_from = n, values_fill = 0) |> 
#   mutate(year_mid = as.numeric(substr(academic_year, 1, 4)))
# 
# m_multi <- multinom(cbind(`Not at all`, `2`, `3`, `4`, `Totally`) ~ year_mid + question, data = agg)
# 
# grid <- expand_grid(
#   question = unique(agg$question),
#   year_mid = seq(min(agg$year_mid), max(agg$year_mid), length.out = 200)
# )
# 
# lvl <- c("Not at all", "2", "3", "4", "Totally")
# 
# pred <- predict(m_multi, newdata = grid, type = "probs") |>
#   as_tibble() |>
#   bind_cols(grid) |>
#   pivot_longer(cols = all_of(lvl), names_to = "response", values_to = "prob") |>
#   mutate(response = factor(response, levels = lvl, ordered = TRUE))
# 
# p3 <- ggplot(pred, aes(x = year_mid, y = prob, colour = response)) +
#   geom_line(linewidth = 1.5) +
#   facet_wrap(~ question, scales = "free_y") +
#   scale_colour_viridis_d(option = "D") +
#   scale_y_continuous(labels = scales::percent) +
#   labs(x = "", y = "Predicted probability", colour = "Likert\nscale") +
#   theme_bw()

design <- "
AB
"
p1 + p2 + plot_layout(design = design) + plot_annotation(tag_levels = "A")















# 2025 student led questionaire -------------------------------------------

df <- read.csv("C:/006-BI3010/Reviews/2025_student_form.csv", header = TRUE)

colnames(df) <- c("Lectures effective",
                  "Workshops effective",
                  "Material useful",
                  "Walkthroughs useful",
                  "Staff supportive",
                  "Study notes useful",
                  "assessment")

df <- df |>
  tidyr::pivot_longer(
    cols = where(is.integer),
    names_to = "question",
    values_to = "score"
  )

df_plot <- df |>
  count(question, score, name = "n_resp") |>
  left_join(df |> 
              count(question, name = "n_responded"), by = c("question")) |>
  mutate(
    prop = n_resp / n_responded
  )


ggplot(df_plot, aes(x = question, y = prop, fill = factor(score))) +
  geom_col() +
  #facet_wrap(~question, ncol = 2) +
  scale_y_continuous(labels = scales::percent, na.value = "null") +
  scale_fill_viridis_d(option = "D") +
  labs(x = "", 
       y = "Percent of respondants",
       fill = "Likert\nscale") +
  theme_bw()
