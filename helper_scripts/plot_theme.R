# a common theme for plots

require(ggplot2, quietly = TRUE)

basic_theme <- list(
  theme(
    plot.title = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line  = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.4, "lines"),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black")
  ),
  guides(
    x = guide_axis(cap = TRUE), 
    y = guide_axis(cap = TRUE)
  )
)

# the end