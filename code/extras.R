theme_bjc <- function (base_size = 11, base_family = "Arial") {
  theme_minimal() %+replace% 
    theme(
      plot.background = element_rect(fill = "white", colour = "transparent"),
      legend.title = element_text(color = "black", size = 10),
      legend.text = element_text(color = "black", size = 9),
      axis.title = element_text(color = "black", size = 10),
      axis.text = element_text(color = "black", size = 9)
    )
}
