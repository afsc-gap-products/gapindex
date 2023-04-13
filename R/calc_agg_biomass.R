#' Calculate index of total biomass across aggregated subareas
#'
#' @param racebase_tables data object created from `AFSC.GAP.DBE::get_data()``
#' @param biomass_strata a dataframe of stratum biomass, result object from 
#'                       `AFSC.GAP.DBE::calc_biomass_stratum()`
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
  
  ## Add DESIGN_YEAR to biomass_strata
  biomass_strata <- merge(x = biomass_strata,
                          y = AFSC.GAP.DBE::design_table,
                          by = c("SURVEY_DEFINITION_ID", "YEAR"))
  
  for (isurvey in 1:nrow(x = survey_designs)) {
    
    subareas <- subset(x = AFSC.GAP.DBE::new_stratum_table,
                       subset = SURVEY_DEFINITION_ID == 
                         survey_designs$SURVEY_DEFINITION_ID[isurvey] &
                         TYPE != "STRATUM" &
                         DESIGN_YEAR == survey_designs$DESIGN_YEAR[isurvey])
    
    for (isubarea in 1:nrow(x = subareas)) {
      strata_in_subarea <- 
        subset(x = AFSC.GAP.DBE::new_stratum_groupings,
               subset = AREA_ID %in% subareas$AREA_ID[isubarea])
      
      if (nrow(x = strata_in_subarea) > 0) {
        subarea_biomass_by_stratrum <- 
          merge(x = strata_in_subarea, 
                by.x = c("DESIGN_YEAR", "SURVEY_DEFINITION_ID", "STRATUM"),
                y = biomass_strata, 
                by.y = c("DESIGN_YEAR", "SURVEY_DEFINITION_ID", "AREA_ID"))
        
        subarea_summed_biomass <- 
          stats::aggregate(cbind(BIOMASS_MT, 
                                 BIOMASS_VAR,
                                 POPULATION_COUNT, 
                                 POPULATION_VAR) ~
                             SPECIES_CODE + YEAR,
                           data = subarea_biomass_by_stratrum,
                           FUN = sum)
        
        subarea_biomass <- 
          rbind(subarea_biomass,
                cbind(data.frame(SURVEY_DEFINITION_ID = 
                                   subareas$SURVEY_DEFINITION_ID[isubarea], 
                                 AREA_ID = subareas$AREA_ID[isubarea]),
                      subarea_summed_biomass))
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
