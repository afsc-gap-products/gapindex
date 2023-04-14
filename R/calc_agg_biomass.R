#' Calculate index of total biomass across aggregated subareas
#'
#' @param racebase_tables data object created from `gapindex::get_data()``
#' @param biomass_strata a dataframe of stratum biomass, result object from 
#'                       `gapindex::calc_biomass_stratum()`
#'
#' @return dataframe of biomass and population abundance estimates across 
#'         subareas and across the region, along with variances.
#' @export
#' 

calc_agg_biomass <- function(racebase_tables,
                             biomass_strata = NULL) {
  
  ## Which survey designs to pull from
  subarea_biomass <- data.frame()
  survey_designs <- racebase_tables$survey
  strata <- racebase_tables$strata
  
  ## Add DESIGN_YEAR to biomass_strata
  biomass_strata <- merge(x = biomass_strata,
                          y = gapindex::design_table,
                          by = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR"))
  
  ## Add stratum area to biomass_strata
  biomass_strata <- merge(x = biomass_strata,
                          y = strata[, c("SURVEY_DEFINITION_ID", "SURVEY", 
                                         "STRATUM", "AREA_KM2")],
                          by = c("SURVEY_DEFINITION_ID", "SURVEY", "STRATUM"))
  
  for (isurvey in 1:nrow(x = survey_designs)) {
    
    subareas <- subset(x = gapindex::area_table,
                       subset = SURVEY_DEFINITION_ID == 
                         survey_designs$SURVEY_DEFINITION_ID[isurvey] &
                         TYPE != "STRATUM" &
                         DESIGN_YEAR == survey_designs$DESIGN_YEAR[isurvey])
    
    for (isubarea in 1:nrow(x = subareas)) {
      strata_in_subarea <- 
        subset(x = gapindex::stratum_groupings,
               subset = AREA_ID %in% subareas$AREA_ID[isubarea])
      
      if (nrow(x = strata_in_subarea) > 0) {
        
        subarea_biomass_by_stratrum <- 
          merge(x = strata_in_subarea, 
                y = biomass_strata, 
                by = c("DESIGN_YEAR", "SURVEY_DEFINITION_ID", "STRATUM"))
        
        subarea_summed_biomass <- 
          stats::aggregate(cbind(BIOMASS_MT, 
                                 BIOMASS_VAR,
                                 POPULATION_COUNT, 
                                 POPULATION_VAR) ~
                             SPECIES_CODE + YEAR,
                           data = subarea_biomass_by_stratrum,
                           FUN = sum)
        
        subarea_mean_cpue <- 
          do.call(what = rbind, 
                  args = lapply(
                    split(x = subarea_biomass_by_stratrum, 
                          f = list(subarea_biomass_by_stratrum$SPECIES_CODE,
                                   subarea_biomass_by_stratrum$YEAR)),
                    FUN = function(x) { 
                      cbind(
                        SPECIES_CODE = unique(x$SPECIES_CODE),
                        YEAR = unique(x$YEAR),
                        TOT_AREA = sum(x$AREA_KM2),
                        CPUE_KGKM2_MEAN = weighted.mean(x = x$CPUE_KGKM2_MEAN, 
                                                        w = x$AREA_KM2),
                        CPUE_NOKM2_MEAN = weighted.mean(x = x$CPUE_NOKM2_MEAN, 
                                                        w = x$AREA_KM2))} ))
        
        subarea_summed_biomass <- merge(x = subarea_summed_biomass,
                                        y = subarea_mean_cpue,
                                        by = c("SPECIES_CODE", "YEAR"))
        
        subarea_summed_biomass$CPUE_KGKM2_VAR <- 
          subarea_summed_biomass$BIOMASS_VAR / 
          subarea_summed_biomass$TOT_AREA^2 * 1e6
         
        subarea_summed_biomass$CPUE_NOKM2_VAR <- 
          subarea_summed_biomass$POPULATION_VAR / 
          subarea_summed_biomass$TOT_AREA^2 
        
        subarea_biomass <- 
          rbind(subarea_biomass,
                cbind(data.frame(SURVEY_DEFINITION_ID = 
                                   subareas$SURVEY_DEFINITION_ID[isubarea], 
                                 AREA_ID = subareas$AREA_ID[isubarea]),
                      subset(x = subarea_summed_biomass,
                             select = c(SPECIES_CODE, YEAR, 
                                        CPUE_KGKM2_MEAN, CPUE_KGKM2_VAR,
                                        CPUE_NOKM2_MEAN, CPUE_NOKM2_VAR,
                                        BIOMASS_MT, BIOMASS_VAR, 
                                        POPULATION_COUNT, POPULATION_VAR))))
      }
    }
  }
  
  ## Warning Messages
  if ( 47 %in% subarea_biomass$SURVEY_DEFINITION_ID & 
       2023 %in% subarea_biomass$YEAR) {
    warning("The GOA total biomass across INPFC area and across depth zones
              only includes years 1987-2023. Starting from 2023, total biomass
              across NMFS areas will only be reported.")
  }
  
  ## Remove EBS + NW strata pre-1987 as these aren't used. Test that I don't need this bit of code. 
  # if (any(subarea_biomass$YEAR < 1987 & 
  #         subarea_biomass$SURVEY_DEFINITION_ID == 98)) {
  #   warning("The (EBS + NW) output only includes years 1987-present.
  #     Years 1982-1986 are NOT included for the (EBS + NW) output because
  #     essentially no stations within strata 82 & 90 (subarea 8 & 9)
  #     were sampled during those years. Biomass/Abundance estimates for 
  #     these early years were removed.")
  #   
  #   EBS_PLUSNW_subareas <- grep(x = subarea_biomass$DESCRIPTION, 
  #                               pattern = "EBS Standard Plus NW Region", 
  #                               value = TRUE)
  #   
  #   subarea_biomass <- 
  #     subset(x = subarea_biomass, 
  #            subset = !(SURVEY == "EBS" & 
  #                         YEAR < 1987 & 
  #                         DESCRIPTION %in% EBS_PLUSNW_subareas))
  # }
  
  return(subarea_biomass)
}
