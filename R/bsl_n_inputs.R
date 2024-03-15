#' N inputs in the baseline
#'
#' @param data field data
#' @param databases Databases
#'
#' @return data final data
#'
#' @export

bsl_n_inputs <- function(data, databases) {
  # Synthetic fertilizer application rate (msf) (t/ha) in the baseline

  data$field_def_country_filter <- ifelse(data$field_def_country == "DK", "DK",
    ifelse(data$field_def_country == "UK", "UK", "Other")
  )

  # Use default values from the 'baseline_fertilizer' table
  baseline_fertilizers <- read_excel(databases, sheet = "N2O_baseline_fertiliser")
  baseline_fertilizers <- as.data.frame(baseline_fertilizers)

  data <- merge(data, baseline_fertilizers,
    by.x = c("field_def_country_filter", "field_def_fertilisers_mixed"),
    by.y = c("Country", "Fertilizer"), all.x = TRUE
  )

  names(data)[names(data) == "synthetic_fertilizer_application_rate_t"] <- "bsl_msf"
  names(data)[names(data) == "organic_fertilizer_application_rate_t"] <- "bsl_mof"

  data$bsl_msf <- as.numeric(data$bsl_msf)
  data$bsl_mof <- as.numeric(data$bsl_mof)

  # N content of synthetic fertilizer (ncsf) in the baseline
  data$bsl_ncsf <- ifelse(data$bsl_msf == 0, 0, 1)

  # N content of organic fertilizer (ncof) in the baseline
  data$bsl_ncof <- ifelse(data$bsl_mof == 0, 0, 0.0039)

  data <- data %>%
    mutate(
      bsl_fsn = bsl_msf * bsl_ncsf * predicted_area, # Eq.20
      bsl_fon = bsl_mof * bsl_ncof * predicted_area # Eq.21
    )

  return(data)
}
