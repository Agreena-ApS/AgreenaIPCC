## code to prepare `example_data` dataset goes here
data <- read.csv(file = "G:/My Drive/Work/Projects/Models/IPCC model/20240314_R package_2023/All_Baseline_field_data_aws_23.csv")
example_data <- data[data$actual_harvest_year == 2022 & data$field_id %in% c(2874, 1753, 1691, 5168, 6893, 11401), ]
databases_2022 <- "G:/My Drive/Work/Projects/Models/IPCC model/20240313_R package_2022/2022_Databases.xlsx"
usethis::use_data(example_data, databases_2022, overwrite = TRUE)
