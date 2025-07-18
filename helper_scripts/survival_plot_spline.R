fc_plot_spline <-
function(pooled_pred_data) {
  
  m_occ <- median(data_comp$ed_occupancy_at_arrival)
  
  spline_plot <- ggplot(pooled_pred_data, aes(x = x, y = HR)) +
      annotate(
        "segment",
        x = -Inf, xend = 174, y = 1, yend = 1,
        linetype = "dotted",
        color = "gray30") +
      annotate(
        "segment",
        x = m_occ, xend = m_occ, y = -Inf, yend = Inf,
        linetype = "dotted",
        color = "gray30"
      ) +
      geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha=0.4, fill="lightblue") +
      geom_line(color="blue3", linewidth=0.75) +
      #geom_vline(xintercept = mean(data_comp$ed_occupancy_at_arrival)) +
      labs(
        x = "ED occupancy at arrival time\n(no. of patients)",
        y = "Hazard ratio: death within 30 days"
      ) +
      scale_x_continuous(limits = c(0,175), breaks = seq(0,175,25)) +
      scale_y_continuous(limits = c(0.7,2), breaks = seq(0.75, 2,0.25)) +
      basic_theme
  
  return(spline_plot)
}
