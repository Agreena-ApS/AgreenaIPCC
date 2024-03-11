#' AgreenaIPPC model
#'
#' @param data Field data from the platform
#' @param harvest_year Define the target harvest year
#' @param databases Databases include necessary default values for the calculation
#'
#' @return data_bsl_act Final result table
#' @export
#'
#' @examples
#' library(readxl)
#' library(dplyr)
#' library(tidyr)
#' setwd("G:/My Drive/Work/Projects/Models/IPCC model/20240222_R models/3_Model")
#' data <- read.csv(file = "All_Baseline_field_data_aws_23.csv")
#' harvest_year <- 2022
#' databases <- "Databases.xlsx"
#' result <- AgreenaIPCC(data, harvest_year, databases)
AgreenaIPCC <- function(data, harvest_year, databases) {
  ### Data processing ---------------------------------------------------------
  # Select target harvest year
  data <- subset(data, actual_harvest_year == harvest_year)

  # Get predicted_areas based on field_id in the two tables
  # We don't have predicted_area for 2023.

  vvb_field_areas <- read_excel(databases, sheet = "VVB_Field areas_(LINKED)")

  data <- left_join(data, vvb_field_areas[, c("field_id", "predicted_area")], by = c("field_id" = "field_id"))

  data$field_def_country_filter <- ifelse(data$field_def_country == "DK", "DK",
                                          ifelse(data$field_def_country == "UK", "UK", "Other")
  )

  ### Baseline ----------------------------------------------------------------

  #### Baseline: Soil n2o emissions -------------------------------------------
  # Soil n2o emissions = n2o emissions due to fertilizer use + N-fixing species

  # Calculate n2o emissions due to fertilizer use
  data_bsl <- bsl_fert_rates(data, databases) # Calculate N inputs from fertilizers
  scenario <- "bsl" # bsl means baseline
  data_bsl <- n2o_emissions_fert_cal(data_bsl, scenario, databases)

  # We assumed no N-fixing species were used in the baseline
  # Calculate soil n2o emissions

  data_bsl <- data_bsl %>%
    mutate(
      bsl_n2o_n_fix = 0, # 2024 could have N-fixing species
      bsl_n2o_soil = bsl_n2o_fert + bsl_n2o_n_fix # Eq.17
    )


  #### Baseline: CO2 emissions due to fuel consumption  ------------------------
  # 0.002886 is the default value derived from the "co2_fuel_emissions" table
  data_bsl$bsl_fuel_emissions <- 0.002886 * (data_bsl$field_def_energy_consumption_amount_ha)

  # Check if energy_consumption_amount_ha 100 for conventional; 80 for reduced; 60 for no tilling. Yes
  # colnames(data)
  # unique(data$field_def_tilling)
  # check_c <- subset(data, field_def_tilling == "Conventional tillage")
  # unique(check_c$field_def_energy_consumption_amount_ha)
  # check_r <- subset(data, field_def_tilling == "Reduced tillage")
  # unique(check_r$field_def_energy_consumption_amount_ha)
  # check_n <- subset(data, field_def_tilling == "No tillage")
  # unique(check_n$field_def_energy_consumption_amount_ha)

  #### Baseline: CO2 emissions due to lime application  ------------------------
  # Obtain default values from the co2_lime_emissions table
  # 2024 should have actual lime data
  co2_lime_emissions <- read_excel(databases, sheet = "CO2_Lime emissions")
  co2_lime_emissions <- as.data.frame(co2_lime_emissions)

  data_bsl <- data_bsl %>%
    left_join(co2_lime_emissions[, c("field_def_country", "2017_2021_baseline_lime_emissions_tCO2e/ha")],
              by = c("field_def_country" = "field_def_country")
    ) %>%
    dplyr::select(everything(), "bsl_lime_emissions" = "2017_2021_baseline_lime_emissions_tCO2e/ha")



  ### actual harvest year -----------------------------------------------------

  ##### actual: Soil n2o emissions ----------------------------------------------

  # Calculate N inputs from synthetic and organic fertilizers
  data_bsl_act <- act_fert_rates(data_bsl, databases)

  # n2o emissions due to fertilizer use
  scenario <- "act" # act means actual harvest year
  data_bsl_act <- n2o_emissions_fert_cal(data_bsl_act, scenario, databases)

  # n2o emissions due to N-fixing species
  data_bsl_act <- act_n_fixing(data_bsl_act, databases)

  # Soil n2o emissions = n2o emissions due to fertilizer use + N-fixing species
  data_bsl_act$act_n2o_soil <- data_bsl_act$act_n2o_fert + data_bsl_act$act_n2o_n_fix

  #### actual: CO2 emissions due to fuel consumption ---------------------------
  # Obtain default values from the co2_fuel_emissions table

  # There are some rows using "Bioethanol (L)" in 2023. However, the database doesn't have data for "Bioethanol (L)".
  # check<-subset(dataset,actual_harvest_year==2023)
  # unique(check$actual_energy_consumption_energy_source)

  co2_fuel_emissions <- read_excel(databases, sheet = "CO2_Fuel emissions")
  co2_fuel_emissions <- as.data.frame(co2_fuel_emissions)
  co2_fuel_emissions$EF <- as.numeric(co2_fuel_emissions$EF)

  data_bsl_act$act_fuel_emissions <- ifelse(data_bsl_act$actual_energy_consumption_energy_source == "Diesel (L)",
                                            (data_bsl_act$actual_energy_consumption_amount_ha) * co2_fuel_emissions[7, "EF"],
                                            ifelse(data_bsl_act$actual_energy_consumption_energy_source == "Petrol (L)",
                                                   (data_bsl_act$actual_energy_consumption_amount_ha) * co2_fuel_emissions[8, "EF"],
                                                   ifelse(data_bsl_act$actual_energy_consumption_energy_source == "Bioethanol (L)",
                                                          "Need data for Bioethanol (L)", NA
                                                   )
                                            )
  )


  #### actual: CO2 emissions due to lime application ---------------------------
  # Obtain default values from the co2_lime_emissions table
  # We will have actual data for 2024

  co2_lime_emissions <- read_excel(databases, sheet = "CO2_Lime emissions")
  co2_lime_emissions <- as.data.frame(co2_lime_emissions)

  data_bsl_act <- data_bsl_act %>%
    left_join(co2_lime_emissions[, c("field_def_country", "2022_actual_lime_emissions_tCO2e/ha")], by = c("field_def_country" = "field_def_country")) %>%
    dplyr::select(everything(), "act_lime_emissions" = "2022_actual_lime_emissions_tCO2e/ha")

  return(data_bsl_act)
}
