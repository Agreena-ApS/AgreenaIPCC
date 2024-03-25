#' IPCC Calculation
#'
#' @description
#' Main function to make the IPCC calculation for N2O emissions from soil, C2O emissions due to fuel consumption and lime application.
#'
#' @param data Field dataset
#' @param harvest_year Target harvest year
#' @param databases Databases include necessary default values for the calculation
#'
#' @return A final IPCC result table including key results of baseline_fuel_emissions, actual_fuel_emissions, baseline_soil_n2o_emissions, actual_soil_n2o_emissions, baseline_lime_co2_emissions_(5y) and actual_lime_emissions
#'
#' @import dplyr
#' @import tidyr
#' @import readxl
#' @export

AgreenaIPCC <- function(data, harvest_year, databases) {
  ### Data processing ---------------------------------------------------------
  # Select target harvest year
  data <- subset(data, data$actual_harvest_year == harvest_year)

  # Get predicted_areas based on field_id in the two tables
  # We don't have predicted_area for 2023.
  if (harvest_year == 2022) {
    vvb_field_areas <- readxl::read_excel(databases, sheet = "VVB_Field areas_(LINKED)")
    data <- dplyr::left_join(data, vvb_field_areas[, c("field_id", "predicted_area")], by = c("field_id" = "field_id"))
  } else {
    data$predicted_area <- data$field_size_ha
  }

  ### Baseline ----------------------------------------------------------------

  #### Baseline: Soil N2O emissions -------------------------------------------
  # Soil N2O emissions = N2O emissions due to fertilizer use + N-fixing species

  # Calculate N2O emissions due to fertilizer use
  data_bsl <- bsl_n_inputs(data, databases) # Calculate N inputs from fertilizers
  scenario <- "bsl" # bsl means baseline
  data_bsl <- n2o_emissions_fert(data_bsl, scenario, databases)

  # We assumed no N-fixing species were used in the baseline
  # Calculate soil N2O emissions

  data_bsl <- data_bsl %>%
    dplyr::mutate(
      bsl_n2o_n_fix = 0, # 2024 could have N-fixing species
      baseline_soil_n2o_emissions = bsl_n2o_fert + bsl_n2o_n_fix # Eq.17
    )

  #### Baseline: CO2 emissions due to fuel consumption  ------------------------
  co2_fuel_emissions <- readxl::read_excel(databases, sheet = "CO2_Fuel emissions")
  co2_fuel_emissions <- as.data.frame(co2_fuel_emissions)

  # Obtain the default values of EFs according to the energy source
  diesel <- co2_fuel_emissions$EF[co2_fuel_emissions$Energy_source == "Diesel (L)"]
  petrol <- co2_fuel_emissions$EF[co2_fuel_emissions$Energy_source == "Petrol (L)"]
  bioethanol <- co2_fuel_emissions$EF[co2_fuel_emissions$Energy_source == "Bioethanol (L)"]

  stopifnot(data_bsl$field_def_energy_consumption_energy_source
    %in% c("Diesel (L)", "Petrol (L)", "Bioethanol (L)"))

  # CO2 emissions (tCO2e/ha) = energy_consumption_amount_ha (L/ha) * EF (tCO2e/L)
  data_bsl$baseline_fuel_emissions <- ifelse(data_bsl$field_def_energy_consumption_energy_source == "Diesel (L)",
    (data_bsl$field_def_energy_consumption_amount_ha) * diesel,
    ifelse(data_bsl$field_def_energy_consumption_energy_source == "Petrol (L)",
      (data_bsl$field_def_energy_consumption_amount_ha) * petrol,
      ifelse(data_bsl$field_def_energy_consumption_energy_source == "Bioethanol (L)",
        (data_bsl$field_def_energy_consumption_amount_ha) * bioethanol, NA
      )
    )
  )

  #### Baseline: CO2 emissions due to lime application  ------------------------
  # Obtain default values from the CO2_lime_emissions table
  # 2024 should have actual lime data (Farmers may or may not report them.)
  # Lack of data for 10 fields in Serbia (RS) (Ask Macdara for help)
  co2_lime_emissions <- readxl::read_excel(databases, sheet = "CO2_Lime emissions")
  co2_lime_emissions <- as.data.frame(co2_lime_emissions)

  stopifnot(data_bsl$field_def_country %in% co2_lime_emissions$field_def_country)

  baseline_lime_emissions <- paste0((harvest_year - 5), "_", (harvest_year - 1), "_", "baseline_lime_emissions_tCO2e/ha")

  data_bsl <- data_bsl %>%
    dplyr::left_join(co2_lime_emissions[, c("field_def_country", baseline_lime_emissions)],
      by = c("field_def_country" = "field_def_country")
    ) %>%
    dplyr::select(everything(), "baseline_lime_co2_emissions_(5y)" = all_of(baseline_lime_emissions))


  ### Actual harvest year -----------------------------------------------------

  ##### Actual: Soil N2O emissions ----------------------------------------------

  # Calculate N inputs from synthetic and organic fertilizers

  if (harvest_year == 2023) {
    data_bsl_act <- data_bsl %>%
      mutate(
        act_fsn = actual_fertilisers_summary_nitrogen_synthetic_kg_ha / 1000 * predicted_area,
        act_fon = actual_fertilisers_summary_nitrogen_organic_kg_ha / 1000 * predicted_area
      )
  } else {
    data_bsl_act <- act_n_inputs(data_bsl, databases)
  }

  # N2O emissions due to fertilizer use
  scenario <- "act" # act means actual harvest year
  data_bsl_act <- n2o_emissions_fert(data_bsl_act, scenario, databases)

  # N2O emissions due to N-fixing species
  data_bsl_act <- act_n_fixing(data_bsl_act, harvest_year, databases)

  # Soil N2O emissions = N2O emissions due to fertilizer use + N-fixing species
  data_bsl_act$actual_soil_n2o_emissions <- data_bsl_act$act_n2o_fert + data_bsl_act$act_n2o_n_fix

  #### Actual: CO2 emissions due to fuel consumption ---------------------------
  co2_fuel_emissions <- readxl::read_excel(databases, sheet = "CO2_Fuel emissions")
  co2_fuel_emissions <- as.data.frame(co2_fuel_emissions)

  # Obtain the default values of EFs according to the energy source
  diesel <- co2_fuel_emissions$EF[co2_fuel_emissions$Energy_source == "Diesel (L)"]
  petrol <- co2_fuel_emissions$EF[co2_fuel_emissions$Energy_source == "Petrol (L)"]
  bioethanol <- co2_fuel_emissions$EF[co2_fuel_emissions$Energy_source == "Bioethanol (L)"]

  act_energy_source <- data_bsl_act[data_bsl_act$actual_tilling != "Fallow", ]$actual_energy_consumption_energy_source
  stopifnot(act_energy_source %in% c("Diesel (L)", "Petrol (L)", "Bioethanol (L)"))

  # CO2 emissions (tCO2e/ha) = energy_consumption_amount_ha (L/ha) * EF (tCO2e/L)
  data_bsl_act$actual_fuel_emissions <- ifelse(data_bsl_act$actual_energy_consumption_energy_source == "Diesel (L)",
    (data_bsl_act$actual_energy_consumption_amount_ha) * diesel,
    ifelse(data_bsl_act$actual_energy_consumption_energy_source == "Petrol (L)",
      (data_bsl_act$actual_energy_consumption_amount_ha) * petrol,
      ifelse(data_bsl_act$actual_energy_consumption_energy_source == "Bioethanol (L)",
        (data_bsl_act$actual_energy_consumption_amount_ha) * bioethanol, NA
      )
    )
  )

  #### Actual: CO2 emissions due to lime application ---------------------------
  # Obtain default values from the co2_lime_emissions table
  # We will have actual data for 2024

  co2_lime_emissions <- read_excel(databases, sheet = "CO2_Lime emissions")
  co2_lime_emissions <- as.data.frame(co2_lime_emissions)

  actual_lime_emissions <- paste0(harvest_year, "_actual_lime_emissions_tCO2e/ha")

  data_bsl_act <- data_bsl_act %>%
    left_join(co2_lime_emissions[, c("field_def_country", actual_lime_emissions)],
      by = c("field_def_country" = "field_def_country")
    ) %>%
    dplyr::select(everything(), "actual_lime_emissions" = all_of(actual_lime_emissions))

  return(data_bsl_act)
}
