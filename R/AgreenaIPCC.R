#' AgreenaIPPC model
#'
#' @param data Field data from the platform
#' @param harvest_year Define the target harvest year
#' @param databases Databases include necessary default values for the calculation
#'
#' @return A final result table
#'
#' @examples
#' library(readxl)
#' library(dplyr)
#' library(tidyr)
#' setwd("G:/My Drive/Work/Projects/Models/IPCC model/20240313_R package_2022")
#' data <- read.csv(file = "All_Baseline_field_data_aws_23.csv")
#' harvest_year <- 2022
#' databases <- "2022_Databases.xlsx"
#' result <- AgreenaIPCC(data, harvest_year, databases)
#'
#' @import dplyr
#' @import tidyr
#' @import readxl
#' @export

AgreenaIPCC <- function(data, harvest_year, databases) {
  ### Data processing ---------------------------------------------------------
  # Select target harvest year
  data <- subset(data, actual_harvest_year == harvest_year)

  # Get predicted_areas based on field_id in the two tables
  # We don't have predicted_area for 2023.
  vvb_field_areas <- read_excel(databases, sheet = "VVB_Field areas_(LINKED)")
  data <- left_join(data, vvb_field_areas[, c("field_id", "predicted_area")], by = c("field_id" = "field_id"))

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
    mutate(
      bsl_n2o_n_fix = 0, # 2024 could have N-fixing species
      bsl_n2o_soil = bsl_n2o_fert + bsl_n2o_n_fix # Eq.17
    )

  #### Baseline: CO2 emissions due to fuel consumption  ------------------------
  data_bsl$bsl_fuel_emissions <- 0.002886 * (data_bsl$field_def_energy_consumption_amount_ha)

  co2_fuel_emissions <- read_excel(databases, sheet = "CO2_Fuel emissions")
  co2_fuel_emissions <- as.data.frame(co2_fuel_emissions)

  # Obtain the default values of EFs according to the energy source
  diesel <- co2_fuel_emissions$EF[co2_fuel_emissions$Energy_source == "Diesel (L)"]
  petrol <- co2_fuel_emissions$EF[co2_fuel_emissions$Energy_source == "Petrol (L)"]
  bioethanol <- co2_fuel_emissions$EF[co2_fuel_emissions$Energy_source == "Bioethanol (L)"]

  # CO2 emissions (tCO2e/ha) = energy_consumption_amount_ha (L/ha) * EF (tCO2e/L)
  data_bsl$bsl_fuel_emissions <- ifelse(data_bsl$field_def_energy_consumption_energy_source == "Diesel (L)",
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
  co2_lime_emissions <- read_excel(databases, sheet = "CO2_Lime emissions")
  co2_lime_emissions <- as.data.frame(co2_lime_emissions)

  data_bsl <- data_bsl %>%
    left_join(co2_lime_emissions[, c("field_def_country", "2017_2021_baseline_lime_emissions_tCO2e/ha")],
      by = c("field_def_country" = "field_def_country")
    ) %>%
    dplyr::select(everything(), "bsl_lime_emissions" = "2017_2021_baseline_lime_emissions_tCO2e/ha")


  ### Actual harvest year -----------------------------------------------------

  ##### Actual: Soil N2O emissions ----------------------------------------------

  # Calculate N inputs from synthetic and organic fertilizers
  data_bsl_act <- act_n_inputs(data_bsl, databases)

  # N2O emissions due to fertilizer use
  scenario <- "act" # act means actual harvest year
  data_bsl_act <- n2o_emissions_fert(data_bsl_act, scenario, databases)

  # N2O emissions due to N-fixing species
  data_bsl_act <- act_n_fixing(data_bsl_act, databases)

  # Soil N2O emissions = N2O emissions due to fertilizer use + N-fixing species
  data_bsl_act$act_n2o_soil <- data_bsl_act$act_n2o_fert + data_bsl_act$act_n2o_n_fix

  #### Actual: CO2 emissions due to fuel consumption ---------------------------
  # Obtain default values from the co2_fuel_emissions table
  co2_fuel_emissions <- read_excel(databases, sheet = "CO2_Fuel emissions")
  co2_fuel_emissions <- as.data.frame(co2_fuel_emissions)
  co2_fuel_emissions$EF <- as.numeric(co2_fuel_emissions$EF)

  data_bsl_act$act_fuel_emissions <- ifelse(data_bsl_act$actual_energy_consumption_energy_source == "Diesel (L)",
    (data_bsl_act$actual_energy_consumption_amount_ha) * co2_fuel_emissions[4, "EF"],
    ifelse(data_bsl_act$actual_energy_consumption_energy_source == "Petrol (L)",
      (data_bsl_act$actual_energy_consumption_amount_ha) * co2_fuel_emissions[5, "EF"],
      ifelse(data_bsl_act$actual_energy_consumption_energy_source == "Bioethanol (L)",
        (data_bsl_act$actual_energy_consumption_amount_ha) * co2_fuel_emissions[6, "EF"], NA
      )
    )
  )

  #### Actual: CO2 emissions due to lime application ---------------------------
  # Obtain default values from the co2_lime_emissions table
  # We will have actual data for 2024

  co2_lime_emissions <- read_excel(databases, sheet = "CO2_Lime emissions")
  co2_lime_emissions <- as.data.frame(co2_lime_emissions)

  data_bsl_act <- data_bsl_act %>%
    left_join(co2_lime_emissions[, c("field_def_country", "2022_actual_lime_emissions_tCO2e/ha")], by = c("field_def_country" = "field_def_country")) %>%
    dplyr::select(everything(), "act_lime_emissions" = "2022_actual_lime_emissions_tCO2e/ha")

  return(data_bsl_act)
}
