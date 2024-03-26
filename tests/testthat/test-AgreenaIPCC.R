test_that("Output of AgreenaIPCC() matches expected ERR22 results", {
  data <- read.csv(file = "G:/My Drive/Work/Projects/Models/IPCC model/20240314_R package_2023/All_Baseline_field_data_aws_23.csv")
  example_data <- data[data$actual_harvest_year == 2022 & data$field_id %in% c(2874, 1753, 1691, 5168, 6893, 11401), ]
  databases_2022 <- "G:/My Drive/Work/Projects/Models/IPCC model/20240313_R package_2022/2022_Databases.xlsx"
  harvest_year <- 2022
  databases <- databases_2022

  result <- AgreenaIPCC(example_data, harvest_year, databases)
  selected_result <- result[, c(
    "field_id", "baseline_soil_n2o_emissions", "baseline_fuel_emissions", "baseline_lime_co2_emissions_(5y)",
    "actual_soil_n2o_emissions", "actual_fuel_emissions", "actual_lime_emissions"
  )]
  colnames(selected_result)[colnames(selected_result) == "baseline_lime_co2_emissions_(5y)"] <- "baseline_lime_co2_emissions"

  # expected_result data are selected from Macdara' excel table for IPCC calculation 2022
  expected_result <- data.frame(
    field_id = c(2874, 6893, 1691, 11401, 5168, 1753),
    baseline_soil_n2o_emissions = c(1.05094, 0.97269, 0.97269, 0.97269, 0.97269, 1.31600),
    baseline_fuel_emissions = c(0.23088, 0.2886, 0.17316, 0.2886, 0.2886, 0.23088),
    baseline_lime_co2_emissions = c(0.41000, 0.02, 0.05, 0.05319, 0.14, 1.43),
    actual_soil_n2o_emissions = c(1.81935, 0.34330, 0.37283, 0.11443, 1.20156, 0.80104),
    actual_fuel_emissions = c(0.14430, 0.10390, 0.14719, 0.08658, 0.1443, 0.11544),
    actual_lime_emissions = c(0.51500, 0.025, 0.08, 0.08008, 0.285, 1.655)
  )

  selected_result[, c(2:7)] <- round(selected_result[, c(2:7)], 3)
  expected_result[, c(2:7)] <- round(expected_result[, c(2:7)], 3)

  expect_equal(selected_result, expected_result)
})
