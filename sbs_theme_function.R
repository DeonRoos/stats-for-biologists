sbs_theme <- function() {
  
  update_geom_defaults("point", list(colour = "white", size = 3))
  update_geom_defaults("line", list(colour = "white", linewidth = 1.25))
  update_geom_defaults("path", list(colour = "white", linewidth = 1.25))
  update_geom_defaults("bar", list(fill = "transparent", colour = "white"))
  
  theme_minimal(base_size = 15) + 
    theme(
      panel.background = element_blank(),
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color = "white", size = 24),
      axis.title = element_text(color = "white", size = 22),
      axis.text = element_text(color = "white", size = 18),
      legend.title = element_text(color = "white", size = 22),
      legend.text = element_text(color = "white", size = 18),
      strip.text = element_text(color = "white", size = 18),
      strip.background = element_rect(color = "#72758d", fill = "black", size = 1),
      panel.border = element_rect(color = "#72758d", fill = NA, size = 1),
      panel.grid.major = element_line(color = "#444654"),
      panel.grid.minor = element_line(color = "#444654")
    )
}

writeLines(capture.output(dput(sbs_theme)), "sbs_theme.R")
