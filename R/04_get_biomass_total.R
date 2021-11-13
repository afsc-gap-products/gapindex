
# Total CPUE for species and year (whole survey region)
# RACEBASE equivalent table: BIOMASS_TOTAL
# BIOMASS_TOTAL has SURVEY_AREA, YEAR, SPECIES CODE, HAUL_COUNT, CATCH_COUNT, MEAN_WGT_CPUE, VAR_WGT_CPUE, MEAN_NUM_CPUE, VAR_NUM_CPUE, TOTAL_BIOMASS, BIOMASS_VAR, MIN_BIOMASS, MAX_BIOMASS, TOTAL_POP, POP_VAR, MIN_POP, MAX_POP

get_biomass_total <- function(racebase_tables = list(
                                cruisedat = cruisedat,
                                haul = haul,
                                catch = catch
                              ),
                              speciescode = 30060, #POP
                              survey_area = "AI",
                              vulnerability = 1,
                              strata = switch(survey_area,
                                              "GOA" = goa_strata,
                                              "AI" = ai_strata
                              )) {
  At <- sum(strata$area)
  
  biomass_stratum <- get_biomass_stratum(racebase_tables = racebase_tables, 
                                         speciescode = speciescode, 
                                         survey_area = survey_area, 
                                         vulnerability = vulnerability)
  
  
 
  biomass_total <- biomass_stratum %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
      haul_count = sum(haul_count),
      catch_count = sum(catch_count),
      mean_wgt_cpue = sum(mean_wgt_cpue * area, na.rm = TRUE) / At, # weighted avg cpue across stata * total area
      mean_num_cpue = sum(mean_num_cpue * area, na.rm = TRUE) / At,
      var_wgt_cpue = sum(stratum_ratio^2 * var_wgt_cpue, na.rm = TRUE),
      var_num_cpue = sum(stratum_ratio^2 * var_num_cpue, na.rm = TRUE),
      total_biomass = sum(stratum_biomass), # checked - ok
      biomass_var = sum(biomass_var), # checked - ok
      # min_biomass = ,
      # max_biomass = ,
      total_pop = sum(stratum_pop), # checked
      pop_var = sum(pop_var) # checked
      # min_pop = ,
      # max_pop =
      #
      # same for total_population
    )


  return(biomass_total)
}