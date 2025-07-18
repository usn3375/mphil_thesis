fc_crowd_spline_pred <-
function(model_name, newdata = ref_data) {
  
  imp_models <- model_name$analyses
  pred_list <- vector("list", length(imp_models))

  crowd_ref_value <- median(data_comp$ed_occupancy_at_arrival)
  
  # Create reference row for centering
  ref_row <- newdata[1, ]
  ref_row$ed_occupancy_at_arrival <- crowd_ref_value
  
  # Loop over imputations
  for (i in seq_along(imp_models)) {
    mod <- imp_models[[i]]
    
    # predict spline for full grid
    pr_grid <- predict(
      mod,
      newdata = newdata,
      type    = "terms",
      se.fit  = TRUE
    )
    
    # save spline term
    term_lab <- grep(
      "ed_occupancy_at_arrival",
      colnames(pr_grid$fit),
      value = TRUE
    )
    
    # Predict spline for ref
    pr_ref <- predict(
      mod,
      newdata = ref_row,
      type    = "terms"
    )
    fit_ref <- pr_ref[, term_lab]
    
    # Center predictions at reference value
    centered_fit <- pr_grid$fit[, term_lab] - fit_ref
    
    pred_list[[i]] <- data.table(
      imp = i,
      x   = newdata$ed_occupancy_at_arrival,
      fit = centered_fit,
      se  = pr_grid$se.fit[, term_lab],
      crowd_ref_value = crowd_ref_value 
    )
  }
  
  prediction_dt <- rbindlist(pred_list)
  return(prediction_dt)
}
