#' Calculation for N2O emissions from soil
#'
#' @param data field dataset
#' @param scenario Baseline or Actual harvest year
#' @param databases Databases include necessary default values for the calculation
#'
#' @return Results of N2O emissions from soil
#' @export

n2o_emissions_fert <- function(data, scenario, databases) {
  # data<-data_bsl
  emission_factors <- read_excel(databases, sheet = "N2O_Emission Factors")
  emission_factors <- as.data.frame(emission_factors)
  ef_n_direct <- emission_factors$Value[emission_factors$EF == "EF_N_direct" & emission_factors$Climate == "Default"]
  gwp_n2o <- emission_factors$Value[emission_factors$EF == "GWP_N2O"]
  frac_gasf <- emission_factors$Value[emission_factors$EF == "Frac_GASF"]
  frac_gasm <- emission_factors$Value[emission_factors$EF == "Frac_GASM"]
  ef_n_volat <- emission_factors$Value[emission_factors$EF == "EF_N_volat"]
  frac_leach <- emission_factors$Value[emission_factors$EF == "Frac_leach" & emission_factors$Climate == "Moist"]
  ef_n_leach <- emission_factors$Value[emission_factors$EF == "EF_N_leach"]

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

      # 44/28 is the conversion factor from N to N2O
      # gwp_n2o is the conversion factor from N2O to CO2

      # Direct N2O emissions
      n2o_fert_direct = (fsn + fon) * ef_n_direct * (44 / 28) * gwp_n2o / area, # Eq.19

      # Indirect N2O emissions
      n2o_fert_volat = (fsn * frac_gasf + fon * frac_gasm) * ef_n_volat * (44 / 28) * gwp_n2o, # Eq.23
      n2o_fert_leach = (fsn + fon) * frac_leach * ef_n_leach * (44 / 28) * gwp_n2o, # Eq.24
      n2o_fert_indirect = (n2o_fert_volat + n2o_fert_leach) / area, # Eq.22

      # Total N2O emissions due to fertilizer use
      n2o_fert = n2o_fert_direct + n2o_fert_indirect,
    )

  result <- out[, c("n2o_fert_direct", "n2o_fert_volat", "n2o_fert_leach", "n2o_fert_indirect", "n2o_fert")]
  colnames(result) <- paste0(scenario, "_", colnames(result))
  data <- cbind(data, result)
  return(data)
}
