#' N2O emissions from soil
#'
#' @param data field data
#' @param scenario Baseline or Actual harvest year
#' @param databases Database
#'
#' @return data final data
#' @export
#'
#' @examples A
n2o_emissions_fert_cal <- function(data, scenario, databases) {
  emission_factors <- read_excel(databases, sheet = "N2O_Emission Factors")
  emission_factors <- as.data.frame(emission_factors)
  ef_n_direct <- emission_factors[1, "Value"]
  gwp_n2o <- emission_factors[7, "Value"]
  frac_gasf <- emission_factors[8, "Value"]
  frac_gasm <- emission_factors[9, "Value"]
  ef_n_volat <- emission_factors[10, "Value"]
  frac_leach <- emission_factors[12, "Value"]
  ef_n_leach <- emission_factors[13, "Value"]

  if (scenario == "bsl") {
    fsn <- data$bsl_fsn
    fon <- data$bsl_fon
  } else if (scenario == "act") {
    fsn <- data$act_fsn
    fon <- data$act_fon
  } else {
    print("Please choose bsl as baseline or act as actual harvest year for the scenario")
  }

  area <- data$predicted_area
  field_id <- data$field_id

  processed_data <- data.frame(field_id, area, fsn, fon)

  out <- processed_data %>%
    mutate(

      # 44/28 is the conversion factor from N to n2o
      # gwp_n2o is the conversion factor from n2o to CO2

      # Direct n2o emissions
      n2o_fert_direct = (fsn + fon) * ef_n_direct * (44 / 28) * gwp_n2o / area, # Eq.19

      # Indirect n2o emissions
      n2o_fert_volat = (fsn * frac_gasf + fon * frac_gasm) * ef_n_volat * (44 / 28) * gwp_n2o, # Eq.23
      n2o_fert_leach = (fsn + fon) * frac_leach * ef_n_leach * (44 / 28) * gwp_n2o, # Eq.24
      n2o_fert_indirect = (n2o_fert_volat + n2o_fert_leach) / area, # Eq.22

      # Total n2o emissions due to fertilizer use
      n2o_fert = n2o_fert_direct + n2o_fert_indirect,
    )

  result <- out[, 5:9]
  colnames(result) <- paste0(scenario, "_", colnames(result))
  data <- cbind(data, result)
  return(data)
}
