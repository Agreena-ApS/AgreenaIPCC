## code to prepare `sysdata` dataset goes here
data <- read.csv(file = "G:/My Drive/Work/Projects/Models/IPCC model/20240314_R package_2023/All_Baseline_field_data_aws_23.csv")
databases_2023 <- "G:/My Drive/Work/Projects/Models/IPCC model/20240314_R package_2023/2023_Databases.xlsx"
databases_2022 <- "G:/My Drive/Work/Projects/Models/IPCC model/20240313_R package_2022/2022_Databases.xlsx"
usethis::use_data(data, databases_2022, databases_2023, overwrite = TRUE, internal = TRUE)
