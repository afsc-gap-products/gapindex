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

calc_biomass_subarea <- function(racebase_tables = NULL,
                                 biomass_strata = NULL) {
  
  ## Argument checks
  if (is.null(x = racebase_tables))
    stop("Must provide argument `racebase_tables` a named list from 
         gapindex::get_data().")
  
  if (is.null(x = biomass_strata))
    stop("Must provide argument `biomass_strata` dataframe from
         gapindex::calc_biomass_stratum().")
  
  ## Which survey designs to pull from
  subarea_biomass <- data.frame()
  survey_designs <- racebase_tables$survey
  strata <- racebase_tables$strata
  
  ## Add DESIGN_YEAR to biomass_strata
  biomass_strata <- merge(x = biomass_strata,
                          y = racebase_tables$survey_design,
                          by = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR"))
  
  ## Add stratum area to biomass_strata
  biomass_strata <- merge(x = biomass_strata,
                          y = strata[, c("SURVEY_DEFINITION_ID", "SURVEY", 
                                         "STRATUM", "AREA_KM2")],
                          by = c("SURVEY_DEFINITION_ID", "SURVEY", "STRATUM"))
  
  for (isurvey in 1:nrow(x = survey_designs)) {
    
    subareas <- subset(x = racebase_tables$subarea,
                       subset = SURVEY_DEFINITION_ID == 
                         survey_designs$SURVEY_DEFINITION_ID[isurvey] &
                         DESIGN_YEAR == survey_designs$DESIGN_YEAR[isurvey])
    
    for (isubarea in 1:nrow(x = subareas)) {
      strata_in_subarea <- 
        subset(x = racebase_tables$stratum_groups,
               subset = AREA_ID %in% subareas$AREA_ID[isubarea])
      
      if (nrow(x = strata_in_subarea) > 0) {
        
        subarea_biomass_by_stratrum <- 
          merge(x = strata_in_subarea, 
                y = biomass_strata, 
                by = c("DESIGN_YEAR", "SURVEY_DEFINITION_ID", "STRATUM"))
        
        subarea_summed_biomass <- 
          stats::aggregate(cbind(BIOMASS_MT, 
                                 POPULATION_COUNT) ~
                             SPECIES_CODE + YEAR,
                           data = subarea_biomass_by_stratrum,
                           FUN = sum)
        
        subarea_summed_variance <- 
          stats::aggregate(cbind(CPUE_KGKM2_VAR, CPUE_NOKM2_VAR, 
                                 BIOMASS_VAR, POPULATION_VAR) ~
                             SPECIES_CODE + YEAR,
                           data = subarea_biomass_by_stratrum,
                           FUN = sum,
                           na.rm = TRUE)
        
        subarea_summed_hauls <- 
          stats::aggregate(cbind(COUNT_HAUL, COUNT_CATCH, COUNT_NUMBER) ~
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
        
        subarea_summed_biomass <- merge(y = subarea_summed_variance,
                                        x = subarea_summed_biomass,
                                        by = c("SPECIES_CODE", "YEAR"))
        
        subarea_summed_biomass <- merge(y = subarea_summed_hauls,
                                        x = subarea_summed_biomass,
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
                                        COUNT_HAUL, COUNT_CATCH, COUNT_NUMBER,
                                        CPUE_KGKM2_MEAN, CPUE_KGKM2_VAR,
                                        CPUE_NOKM2_MEAN, CPUE_NOKM2_VAR,
                                        BIOMASS_MT, BIOMASS_VAR, 
                                        POPULATION_COUNT, POPULATION_VAR) )))
      }
    }
  }
  
  ## Warning Messages
  if ( 47 %in% subarea_biomass$SURVEY_DEFINITION_ID & 
       2025 %in% subarea_biomass$YEAR) {
    warning("The GOA total biomass across INPFC area and across depth zones
              only includes years 1987-2023. Starting from 2025, only total 
              biomass across NMFS areas will be reported.")
  }
  
  return(subarea_biomass)
}
