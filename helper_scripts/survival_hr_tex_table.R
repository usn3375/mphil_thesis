# a function to clean hazard ratio hazard ratio estimates and turn
# them into a LaTeX booktabs table

require(
  data.table,
  tidytable,
  quietly = TRUE
)

fc_estim_latex_tab <- function(esimate_df) {
  
  tex_ready <- esimate_df |> 
    mutate(
      across(where(is.numeric), round, 3),
      ci.95.comb = paste0("(", hr.ci95.low, "-", hr.ci95.upper, ")"),
      `Log-Hazard (Std.Error)` = paste0(estimate, " (", std.error, ")"),
      signif = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ ""
      ),
      p.value = as.character(p.value),
      p.value = fifelse(p.value == "0", "<0.001", p.value),
      `P-Value` = paste0(p.value, signif),
      `Hazard Ratio (95%CI)` = paste(hazard.ratio, ci.95.comb)
    ) |>
    select(term, `Log-Hazard (Std.Error)`, `Hazard Ratio (95%CI)`, `P-Value`) |>
    kable(
      digits = 3,
      format = "latex",
      booktabs = TRUE,
      align = c("l","l","l","l","l")
    )
  
  return(tex_ready)
}

# the end