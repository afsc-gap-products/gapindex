#' Calculate index of total biomass across aggregated subareas
#'
#' @param racebase_tables data object created from `gapindex::get_data()``
#' @param biomass_strata a dataframe of stratum biomass, result object from 
#'                       `gapindex::calc_biomass_stratum()`
#'
#' @return dataframe of biomass and population abundance estimates (with 
#' associated variances) across survey, year, species, and subarea (AREA_ID). 
#'
#' | Field Name           | Description                                                                                                                                                         |
#' |----------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' | SURVEY_DEFINITION_ID | Integer number identifier corresponding to survey region. See   gapindex::survey_ids for a list of relevant survey regions.                                         |
#' | SURVEY               | Survey region.                                                                                                                                                      |
#' | AREA_ID              | Integer identifier for a subarea. See   gapindex::area_table for the full list.                                                                                     |
#' | SPECIES_CODE         | Taxon code. [See the code book for the full   list.](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
#' | YEAR                 | Survey year.                                                                                                                                                        |
#' | N_HAUL               | Number of hauls in stratum.                                                                                                                                         |
#' | N_WEIGHT             | Number of hauls with a positive catch weight.                                                                                                                       |
#' | N_COUNT              | Number of hauls with a positive numerical catch.                                                                                                                    |
#' | N_LENGTH             | Number of hauls with length-frequency data                                                                                                                          |
#' | CPUE_KGKM2_MEAN      | Mean weight catch per area swept (kg/km^2).                                                                                                                         |
#' | CPUE_KGKM2_VAR       | Variance of the mean weight catch per area swept.                                                                                                                   |
#' | CPUE_NOKM2_MEAN      | Mean numerical catch per area swept (no/km^2).                                                                                                                      |
#' | CPUE_NOKM2_VAR       | Variance of the mean weight catch per area swept                                                                                                                    |
#' | BIOMASS_MT           | Estimated total biomass (mt).                                                                                                                                       |
#' | BIOMASS_VAR          | Variance associated with the estimated total biomass.                                                                                                               |
#' | POPULATION_COUNT     | Estimated total numerical abundance (numbers).                                                                                                                      |
#' | POPULATION_VAR       | Variance associated with the estimated total numerical abundance.                                                                                                   |
#' 
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
  survey_designs <- racebase_tables$survey_design
  strata <- racebase_tables$strata
  
  ## Add "SURVEY" column from `racebase_tables$cruise` to `survey_designs` 
  survey_designs <- merge(x = survey_designs,
                          y = subset(x = racebase_tables$survey,
                                     select = c(SURVEY_DEFINITION_ID, SURVEY)),
                          by = "SURVEY_DEFINITION_ID")
  
  strata <- merge(x = strata,
                  y = racebase_tables$survey,
                  by = c( "SURVEY_DEFINITION_ID", "DESIGN_YEAR"))
  
  ## Add DESIGN_YEAR to biomass_strata
  biomass_strata <- merge(x = biomass_strata,
                          y = survey_designs,
                          by = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR"))
  
  ## Add stratum area to biomass_strata
  biomass_strata <- merge(x = biomass_strata,
                          y = strata[, c("SURVEY_DEFINITION_ID", "SURVEY", 
                                         "STRATUM", "AREA_KM2")],
                          by = c("SURVEY_DEFINITION_ID", "SURVEY", "STRATUM"))
  
  unique_surveys <- racebase_tables$survey
  
  for (isurvey in 1:nrow(x = unique_surveys)) {
    
    subareas <- subset(x = racebase_tables$subarea,
                       subset = SURVEY_DEFINITION_ID == 
                         survey_designs$SURVEY_DEFINITION_ID[isurvey] &
                         DESIGN_YEAR == survey_designs$DESIGN_YEAR[isurvey])
    
    subareas$SURVEY <- unique_surveys$SURVEY[isurvey]
    
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
          stats::aggregate(cbind(N_HAUL, N_WEIGHT, N_COUNT, N_LENGTH) ~
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
