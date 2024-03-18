#' N-fixing species
#'
#' @param data field data
#' @param databases Databases
#'
#' @return Final data
#'
#' @export

act_n_fixing <- function(data, databases) {

  n_fixing_species <- read_excel(databases, sheet = "N2O_N_fixing")
  n_fixing_species <- as.data.frame(n_fixing_species)

  n_fixing_species <- n_fixing_species %>%
    rename(
      n_fix = "N-fix",
      id = "Agreena crop identifier 2022 M2",
      nag = "N content of above-ground residues (N AG(T))",
      nbg = "N content of below-ground residues (N BG(T))",
      ratio_above_product = "Ratio of above-ground residue dry matter to harvested yield (RAG (T))",
      ratio_below_above = "Ratio of below-ground biomass to above-ground biomass (RS (T))",
      dry_product = "Dry matter fraction of harvested product (DRY)",
      slope = "Slope (T)",
      intercept = "Intercept (T)"
    )

  n_fixing_species$id <- ifelse(is.na(n_fixing_species$id), 0, n_fixing_species$id)

  data <- data %>%
    left_join(
      n_fixing_species[, c(
        "n_fix", "id", "nag", "nbg", "ratio_above_product",
        "ratio_below_above", "dry_product", "slope", "intercept"
      )],
      by = c("actual_crop_type_id" = "id")
    )

  # Same IDs correspond to different species in the N-fixing specie table
  # I have changed the names in Agreena crop identifier 2022 M2 of the N-fixing specie table to avoid duplicate name IDs

  emission_factors <- read_excel(databases, sheet = "N2O_Emission Factors")
  emission_factors <- as.data.frame(emission_factors)
  ef_n_direct <- emission_factors$Value[emission_factors$EF == "EF_N_direct" & emission_factors$Climate == "Default"]
  gwp_n2o <- emission_factors$Value[emission_factors$EF == "GWP_N2O"]


  data <- data %>%
    mutate(
      # N amount (t) in belowground biomass (MBbg)
      act_MBbg = ((actual_crop_gross_yield * 1000 * dry_product * slope + intercept) +
                    actual_crop_gross_yield * 1000 * dry_product) / 1000 *
        n_fix * ratio_below_above * nbg * predicted_area,

      # N amount (t) in aboveground biomass (MBag)
      act_MBag = if_else(actual_crop_residue_management_logic_app == "Mulched",
                         ((actual_crop_gross_yield * 1000 * dry_product * slope + intercept) / 1000 * n_fix * nag * predicted_area), 0
      ),

      # N2O emissions (tCO2e/ha) from crop residues due to the use of N-fixing species

      act_n2o_n_fix = (act_MBbg + act_MBag) * ef_n_direct * (44 / 28) * gwp_n2o / predicted_area
    )
  # Alfalfa deserves further attention

  data$act_n2o_n_fix[is.na(data$act_n2o_n_fix)] <- 0

  return(data)
}
