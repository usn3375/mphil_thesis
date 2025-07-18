# a function to pool and tidy the regression estimates from the survival model

require(
  stringr,
  data.table,
  tidytable,
  quietly = TRUE
)

fc_tidy_estim <- function(model_mids) {
  
  term_levels <- c(
    "ICU Treatment",
    "Hospital admission",
    "EDLOS (hours)",
    "Nighttime arrival",
    "Weekend arrival",
    "Care home resident",
    "High risk condition",
    "Acuity score: 3-4",
    "Acuity score: 1-2",
    "NEWS2 score",
    "Ambulance arrival",
    "Physician referral",
    "Age (years)",
    "Male gender"
  )
  
  tidy_estim <- mice::pool(model_mids) |>
    tidy() |>
    as.data.table() |>
    na.omit() |>
    mutate(
      hazard.ratio  = exp(estimate),
      hr.ci95.upper = exp(estimate + 1.96 * std.error),
      hr.ci95.low   = exp(estimate - 1.96 * std.error),
      term = case_when(
        str_detect(term, "Gender") ~ "Male gender",
        str_detect(term, "Age") ~ "Age (years)",
        str_detect(term, "Referral") ~ "Physician referral",
        str_detect(term, "ArrivalMode") ~ "Ambulance arrival",
        str_detect(term, "Arearesus") ~ "Treated in resuscitation area",
        str_detect(term, "NEWS") ~ "NEWS2 score",
        str_detect(term, "AcuityScore3") ~ "Acuity score: 3-4",
        str_detect(term, "AcuityScore1") ~ "Acuity score: 1-2",
        str_detect(term, "att_reason_mort") ~ "High risk condition",
        str_detect(term, "CareHome")  ~ "Care home resident",
        str_detect(term, "catweeken") ~ "Weekend arrival",
        str_detect(term, "time_cat")  ~ "Nighttime arrival",
        str_detect(term, "EDLOS_hours$") ~ "EDLOS (hours)",
        str_detect(term, "EDLOS_hours") ~ "EDLOS (hours) : Admission",
        str_detect(term, "AdmittedNonAdmitted") ~ "Hospital admission",
        str_detect(term, "icu_admission") ~ "ICU Treatment",
        TRUE ~ term
      ),
      term = as.character(term)
    ) |>
    mutate(
      term = if (all(unique(term) %in% term_levels)) {
        factor(term, levels = term_levels, ordered = TRUE)
      } else {
        factor(term)
      }
    ) |>
    select(
      term,
      estimate,
      std.error,
      hazard.ratio,
      hr.ci95.upper,
      hr.ci95.low,
      p.value
    )
  
  return(tidy_estim)
}


# the end