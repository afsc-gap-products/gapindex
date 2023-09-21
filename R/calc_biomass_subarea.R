#' Calculate index of total biomass across aggregated subareas
#'
#' @param racebase_tables data object created from `gapindex::get_data()``
#' @param biomass_strata a dataframe of stratum biomass, result object from 
#'                       `gapindex::calc_biomass_stratum()`
#'
#' @eval c("@return", get_table_metadata("inst/extdata/metadata.csv", 
#' select = c("SURVEY_DEFINITION_ID", "SURVEY", "AREA_ID", "SPECIES_CODE" ,
#' "YEAR", "N_HAUL", "N_WEIGHT", "N_COUNT", "N_LENGTH", "CPUE_KGKM2_MEAN",
#' "CPUE_KGKM2_VAR", "CPUE_NOKM2_MEAN", "CPUE_NOKM2_VAR", "BIOMASS_MT",
#' "BIOMASS_VAR", "POPULATION_COUNT", "POPULATION_VAR")))
#' 
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
  survey_designs <- racebase_tables$survey_design
  strata <- racebase_tables$strata
  subareas <- racebase_tables$subarea
  unique_surveys <- racebase_tables$survey
  
  ## Attach "DESIGN_YEAR" from `survey_designs` to `biomass_strata` using 
  ## "SURVEY_DEFINITION_ID", "SURVEY", "YEAR" as a composite key.
  biomass_strata <- merge(x = biomass_strata,
                          y = survey_designs,
                          by = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR"))
  
  ## Attach "AREA_KM2" from `strata` to `biomass_strata` using 
  ## "SURVEY_DEFINITION_ID", "SURVEY", "STRATUM" as a composite key.
  biomass_strata <- merge(x = biomass_strata,
                          y = strata[, c("SURVEY_DEFINITION_ID", "SURVEY", 
                                         "STRATUM", "AREA_KM2")],
                          by = c("SURVEY_DEFINITION_ID", "SURVEY", "STRATUM"))
  
  for (isurvey in 1:nrow(x = unique_surveys)) { ## Loop over surveys -- start
    
    ## Extract survey name of isurvey
    subareas$SURVEY <- unique_surveys$SURVEY[isurvey]
    
    ## Subset the set of subareas given the survey and design year. From 
    ## 2025-on the GOA time series will have two unique survey designs with 
    ## different design years. 
    subareas <- subset(x = racebase_tables$subarea,
                       subset = SURVEY_DEFINITION_ID == 
                         survey_designs$SURVEY_DEFINITION_ID[isurvey] &
                         DESIGN_YEAR == survey_designs$DESIGN_YEAR[isurvey])
    
    for (isubarea in 1:nrow(x = subareas)) { ## Loop over subareas -- start
      
      ## Extract the strata (AREA_ID) that are contained within isubarea
      strata_in_subarea <- 
        subset(x = racebase_tables$stratum_groups,
               subset = AREA_ID %in% subareas$AREA_ID[isubarea])
      
      if (nrow(x = strata_in_subarea) > 0) {
        
        ## Merge all of the biomass, abundance, cpue, n_haul, etc. columns
        ## from `biomass_strata` to `strata_in_subarea` using "DESIGN_YEAR",
        ## "SURVEY_DEFINITION_ID", "STRATUM" as a composite key. 
        subarea_biomass_by_stratrum <- 
          merge(x = strata_in_subarea, 
                y = biomass_strata, 
                by = c("DESIGN_YEAR", "SURVEY_DEFINITION_ID", "STRATUM"))
        
        if (nrow(x = subarea_biomass_by_stratrum) > 0) {
          
          ## Sum the biomass and abundance across strata in isubarea
          subarea_summed_hauls <- 
            stats::aggregate(cbind(N_HAUL, N_WEIGHT, N_COUNT, N_LENGTH) ~
                               SPECIES_CODE + YEAR,
                             data = subarea_biomass_by_stratrum,
                             FUN = sum)
          
          ## Sum the biomass and abundance across strata in isubarea
          subarea_summed_biomass <- 
            stats::aggregate(cbind(BIOMASS_MT, 
                                   POPULATION_COUNT) ~
                               SPECIES_CODE + YEAR,
                             data = subarea_biomass_by_stratrum,
                             FUN = sum)
          
          ## Sum the many types of variances across strata in isubarea
          subarea_summed_variance <- 
            stats::aggregate(cbind(CPUE_KGKM2_VAR, CPUE_NOKM2_VAR, 
                                   BIOMASS_VAR, POPULATION_VAR) ~
                               SPECIES_CODE + YEAR,
                             data = subarea_biomass_by_stratrum,
                             FUN = sum,
                             na.rm = TRUE)
          

          
          subarea_mean_cpue <- 
            do.call(what = rbind, 
                    args = lapply(
                      split(x = subarea_biomass_by_stratrum, 
                            f = list(subarea_biomass_by_stratrum$SPECIES_CODE,
                                     subarea_biomass_by_stratrum$YEAR)),
                      FUN = function(x) { 
                        data.frame(
                          SPECIES_CODE = unique(x$SPECIES_CODE),
                          YEAR = unique(x$YEAR),
                          TOT_AREA = sum(x$AREA_KM2),
                          CPUE_KGKM2_MEAN = weighted.mean(x = x$CPUE_KGKM2_MEAN, 
                                                          w = x$AREA_KM2),
                          CPUE_NOKM2_MEAN = weighted.mean(x = x$CPUE_NOKM2_MEAN, 
                                                          w = x$AREA_KM2),
                          stringsAsFactors = FALSE)} ))
          
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
                                   SURVEY = subareas$SURVEY[isubarea],
                                   AREA_ID = subareas$AREA_ID[isubarea]),
                        subset(x = subarea_summed_biomass,
                               select = c(SPECIES_CODE, YEAR, 
                                          N_HAUL, N_WEIGHT, N_COUNT, N_LENGTH,
                                          CPUE_KGKM2_MEAN, CPUE_KGKM2_VAR,
                                          CPUE_NOKM2_MEAN, CPUE_NOKM2_VAR,
                                          BIOMASS_MT, BIOMASS_VAR, 
                                          POPULATION_COUNT, POPULATION_VAR) )))
          
        }
      }
    } ## Loop over subareas -- end
  }  ## Loop over surveys -- end
  
  ## Warning Messages
  if ( 47 %in% subarea_biomass$SURVEY_DEFINITION_ID & 
       2025 %in% subarea_biomass$YEAR) {
    warning("The GOA total biomass across INPFC area and across depth zones
              only includes years 1987-2023. Starting from 2025, only total 
              biomass across NMFS areas will be reported.")
  }
  
  ## Remove EBS + NW subarea estimates prior to 1987
  if (any(subarea_biomass$YEAR < 1987 & subarea_biomass$AREA_ID == 99900)) {
    warning("The (EBS + NW) output only includes years 1987-present.
      Years 1982-1986 are NOT included for the (EBS + NW) output because
      essentially no stations within strata 82 & 90 (subarea 8 & 9)
      were sampled during those years. Biomass/Abundance estimates for 
      these early years were removed.")
    
    subarea_biomass <- subset(x = subarea_biomass, 
                            subset = !(SURVEY_DEFINITION_ID == 98 & 
                                         YEAR < 1987 & 
                                         AREA_ID %in% c(99900, 
                                                        300, 200, 100, 
                                                        8, 9)) )
  }
  
  return(subarea_biomass)
}
