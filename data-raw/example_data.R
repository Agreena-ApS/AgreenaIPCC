## code to prepare `example_data` dataset goes here
data <- read.csv(file = "G:/My Drive/Work/Projects/Models/IPCC model/20240314_R package_2023/All_Baseline_field_data_aws_23.csv")
example_data <- data[8:12,]
databases_2023 <- "G:/My Drive/Work/Projects/Models/IPCC model/20240314_R package_2023/2023_Databases.xlsx"
usethis::use_data(example_data, databases_2023, overwrite = TRUE)
