#' N inputs in the baseline
#'
#' @param data field data
#' @param databases Databases
#'
#' @return data final data
#' @export
#'
#' @examples A
bsl_fert_rates <- function(data, databases) {
  # Synthetic fertilizer application rate (msf) (t/ha) in the baseline
  # Use default values from the 'baseline_fertilizer' table
  baseline_fertilizers <- read_excel(databases, sheet = "N2O_baseline_fertiliser")
  baseline_fertilizers <- as.data.frame(baseline_fertilizers)

  data$bsl_msf <- ifelse(data$field_def_country_filter == "DK" & data$field_def_fertilisers_mixed == "only synthetic", baseline_fertilizers[1, 3],
                         ifelse(data$field_def_country_filter == "DK" & data$field_def_fertilisers_mixed == "mixed", baseline_fertilizers[2, 3],
                                ifelse(data$field_def_country_filter == "UK" & data$field_def_fertilisers_mixed == "only synthetic", baseline_fertilizers[4, 3],
                                       ifelse(data$field_def_country_filter == "UK" & data$field_def_fertilisers_mixed == "mixed", baseline_fertilizers[5, 3],
                                              ifelse(data$field_def_country_filter == "Other" & data$field_def_fertilisers_mixed == "only synthetic", baseline_fertilizers[7, 3],
                                                     ifelse(data$field_def_country_filter == "Other" & data$field_def_fertilisers_mixed == "mixed", baseline_fertilizers[8, 3], 0)
                                              )
                                       )
                                )
                         )
  )

  data$bsl_msf <- as.numeric(data$bsl_msf)

  # N content of synthetic fertilizer (ncsf) in the baseline
  data$bsl_ncsf <- ifelse(data$bsl_msf == 0, 0, 1)


  # Organic fertilizer application rate (mof) (t/ha) in the baseline
  # Use default values from the 'Baseline_Fertilizer' table
  data$bsl_mof <- ifelse(data$field_def_country_filter == "DK" & data$field_def_fertilisers_mixed == "mixed", baseline_fertilizers[2, 4],
                         ifelse(data$field_def_country_filter == "DK" & data$field_def_fertilisers_mixed == "only organic", baseline_fertilizers[3, 4],
                                ifelse(data$field_def_country_filter == "UK" & data$field_def_fertilisers_mixed == "mixed", baseline_fertilizers[5, 4],
                                       ifelse(data$field_def_country_filter == "UK" & data$field_def_fertilisers_mixed == "only organic", baseline_fertilizers[6, 4],
                                              ifelse(data$field_def_country_filter == "Other" & data$field_def_fertilisers_mixed == "mixed", baseline_fertilizers[8, 4],
                                                     ifelse(data$field_def_country_filter == "Other" & data$field_def_fertilisers_mixed == "only organic", baseline_fertilizers[9, 4], 0)
                                              )
                                       )
                                )
                         )
  )

  data$bsl_mof <- as.numeric(data$bsl_mof)

  # N content of organic fertilizer (ncof) in the baseline
  data$bsl_ncof <- ifelse(data$bsl_mof == 0, 0, 0.0039)


  data <- data %>%
    mutate(
      bsl_fsn = bsl_msf * bsl_ncsf * predicted_area, # Eq.20
      bsl_fon = bsl_mof * bsl_ncof * predicted_area # Eq.21
    )

  return(data)
}
