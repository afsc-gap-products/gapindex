snowfall_sims <- function(boot) {
  # library(sampling)

  i <- boot

  set.seed(i)

  cpue_base <- gapindex::calc_cpue(gapdata = gapdata)
  samplesizes <- table(cpue_base$STRATUM)
  cpue_sort <- cpue_base[order(cpue_base$STRATUM, decreasing = FALSE), ]
  # x is the sampling frame
  x <- sampling::strata(goa_cpue_sort, stratanames = "STRATUM", size = samplesizes, method = "srswr")

  # oneboot is a single stratified resampling of the haul table using the strata and sample sizes defined in x:
  oneboot <- sampling::getdata(data = goa_cpue_sort, m = x)
  oneboot <- oneboot |> dplyr::select(-ID_unit, -Prob, -Stratum)

  # check sample sizes just to be sure
  # table(oneboot$STRATUM)

  # Stratum biomass
  boot_biomass_stratum <-
    gapindex::calc_biomass_stratum(
      racebase_tables = goa_data,
      cpue = oneboot # bootstrapped cpues
    )

  boot_biomass_stratum$bootstrap <- i
  boot_biomass_stratum$species_code <- names(species_codes[s])
  boot_biomass_stratum$year <- year.vec[y]

  # Total biomass
  boot_biomass <- boot_biomass_stratum |>
    dplyr::summarise(
      TOTAL_BIO = sum(BIOMASS_MT),
      TOTAL_POP = sum(POPULATION_COUNT)
    )

  boot_biomass$bootstrap <- i
  boot_biomass$species_code <- names(species_codes[s])
  boot_biomass$year <- year.vec[y]

  # CPUE by station (for VAST)
  oneboot$bootstrap <- i

  return(list(
    boot_biomass = boot_biomass,
    boot_biomass_stratum = boot_biomass_stratum,
    boot_cpue = oneboot
  ))
}
