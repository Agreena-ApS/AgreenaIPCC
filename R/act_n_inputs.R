#' Actual N inputs from fertilizers
#'
#' @param data Field data
#' @param databases Databases
#'
#' @return N inputs from synthetic and organic fertilizers
#' @export

act_n_inputs <- function(data, databases) {
  # Separate values in column "actual_fertilisers_summary_names" based on semicolon delimiter
  # Be careful: more fertilizer names (I think mainly synthetic fertilizers but I am not sure) in other harvest years

  max_fert_number <- max(data$actual_fertilisers_count, na.rm = TRUE)
  data <- separate(data, "actual_fertilisers_summary_names",
                       into = paste0("actual_fertilisers_summary_names", "_", 1:max_fert_number),
                       sep = "; "
  )

  data <- separate(data, "actual_fertilisers_summary_application_rates",
                       into = paste0("actual_fertilisers_summary_application_rates", "_", 1:max_fert_number),
                       sep = "; "
  )

  # Change all columns with "actual_fertilisers_summary_application_rates_" as numeric
  # And fill 0 into NA to fix the warning above
  application_rates_columns <- grepl("^actual_fertilisers_summary_application_rates_", names(data)) # Identify columns starting with "actual_fertilisers_summary_application_rates_"
  data[application_rates_columns] <- lapply(data[application_rates_columns], as.numeric)

  data <- data %>%
    mutate(across(starts_with("actual_fertilisers_summary_application_rates_"), ~ ifelse(is.na(.), 0, .)))

  # Obtain N content of organic fertilizer 1-5, respectively,
  # Default value derived from agreena_fertilizers table
  for (i in 1:max_fert_number) {
    col_name <- paste0("act_ncof_", i)
    data[[col_name]] <- NA
  }

  agreena_fertilizers <- read_excel(databases, sheet = "N2O_Agreena fertilisers_table")
  agreena_fertilizers <- as.data.frame(agreena_fertilizers)

  # Loop through each row of data
  for (i in 1:nrow(data)) {
    # Loop through each column of data
    for (j in 1:max_fert_number) {
      # Find the corresponding value in agreena_fertilizers table
      # If we have IDs for fertilizers in the field data table, we can change the codes here to make the matches between IDs
      matching_row <- which(agreena_fertilizers$`Current names (as on platform)`
                            == data[i, paste0("actual_fertilisers_summary_names_", j)])
      if (length(matching_row) > 0) {
        # Assign the corresponding E value from agreena_fertilizers table to data
        data[i, paste0("act_ncof_", j)] <- agreena_fertilizers$N_frac[matching_row]
      } else {
        data[i, paste0("act_ncof_", j)] <- 0
      }
    }
  }

  # Calculate total N inputs (t) from organic fertilizer 1-5, respectively
  # Based on Eq.21
  for (i in 1:max_fert_number) {
    col_name <- paste0("act_fon_", i)
    data[[col_name]] <- NA
  }

  for (i in 1:nrow(data)) {
    for (j in 1:max_fert_number) {
      actual_fertilisers_summary_application_rates <- data[i, paste0("actual_fertilisers_summary_application_rates_", j)]
      act_ncof <- data[i, paste0("act_ncof_", j)]
      field_size_ha <- data[i, "predicted_area"]
      act_fon <- ((actual_fertilisers_summary_application_rates) / 1000) * (act_ncof) * (field_size_ha) # Eq.21
      data[i, paste0("act_fon_", j)] <- act_fon
    }
  }

  # Obtain N inputs per hectare (kg/ha) from organic and syntheric fertilizer
  # We calculated N inputs from organic fertilizer (kg/ha)
  data$act_fon_kg_ha <- (rowSums(data[, grepl("^act_fon_", names(data))])) * 1000 / (data$predicted_area)

  # We obtained N inputs from synthetic fertilizer from Constantine (Harvest year 2022)
  # actual_n_application_rates table was from Constantine
  # How should we obtained N inputs per hectare (kg/ha) from synthetic fertilizer for Harvest year 2023?

  actual_n_application_rates <- read_excel(databases, sheet = "N Rates_(LINKED)")
  actual_n_application_rates <- as.data.frame(actual_n_application_rates)

  data <- data %>%
    left_join(actual_n_application_rates[, c("field_id", "FSN")],
              by = c("field_id" = "field_id")
    ) %>%
    dplyr::select(everything(), act_fsn_kg_ha = FSN)

  # Total N inputs (tN) from synthetic and organic fertilizers in the fields
  data <- data %>%
    mutate(
      act_FN_kg_ha = act_fon_kg_ha + act_fsn_kg_ha,
      act_fsn = act_fsn_kg_ha / 1000 * predicted_area,
      act_fon = act_fon_kg_ha / 1000 * predicted_area
    )

  return(data)
}
