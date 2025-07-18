fc_pool_rubins <-
function(model_name, prediction_dt) {
  m <- length(model_name)
  
  pooled_pred <- prediction_dt |>
    group_by(x) |>
    summarize(
      theta_bar = mean(fit), # pooled estimate
      U_bar = mean(se^2), # within-imputation variance
      B = var(fit) # between-imputation variance
    ) |>
    ungroup() |>
    mutate(
      T_var = U_bar + (1 + 1 / m) * B, # total variance
      se_total = sqrt(T_var), # total standard error
      HR = exp(theta_bar), 
      lower_CI = exp(theta_bar - 1.96 * se_total),
      upper_CI = exp(theta_bar + 1.96 * se_total)
    )
  
  return(pooled_pred)
}
