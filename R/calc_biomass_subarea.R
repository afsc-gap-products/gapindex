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
  survey_designs <- racebase_tables$survey_design
  unique_surveys <- racebase_tables$survey
  strata <- racebase_tables$strata
  stratum_groups <- racebase_tables$stratum_groups
  
  ## Attach "DESIGN_YEAR" from `survey_designs` to `biomass_strata` using 
  ## "SURVEY_DEFINITION_ID", "SURVEY", "YEAR" as a composite key.
  biomass_strata <- 
    unique_surveys[biomass_strata,
                   on = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR")]
  
  ## Attach "AREA_KM2" from `strata` to `biomass_strata` using 
  ## "SURVEY_DEFINITION_ID", "SURVEY", "STRATUM" as a composite key.
  biomass_strata <-
    strata[, c("SURVEY_DEFINITION_ID", "SURVEY", 
               "STRATUM", "AREA_KM2")
    ][
      biomass_strata,
      on = c("SURVEY_DEFINITION_ID", "SURVEY", "STRATUM")]
  
  ## Create 
  subarea_biomass <- data.table::data.table()
  for (irow in 1:nrow(x = unique_surveys))
    subarea_biomass <- 
    rbind(subarea_biomass,
          cbind(YEAR = unique_surveys$YEAR[irow],
                stratum_groups[
                  SURVEY_DEFINITION_ID == 
                    unique_surveys$SURVEY_DEFINITION_ID[irow] &
                    SURVEY == 
                    unique_surveys$SURVEY[irow] &
                    DESIGN_YEAR == 
                    unique_surveys$DESIGN_YEAR[irow]]))
  
  subarea_biomass <- 
    subarea_biomass[biomass_strata,
                    on = c("SURVEY_DEFINITION_ID", "SURVEY", 
                           "DESIGN_YEAR", "YEAR ", "STRATUM"),
                    allow.cartesian = TRUE]
  
  
  weighted_cpue <- function(x) {
    result_df <- 
      data.table::data.table(
        SPECIES_CODE = unique(x = x$SPECIES_CODE),
        AREA_ID = unique(x = x$AREA_ID),
        YEAR = unique(x = x$YEAR),
        TOT_AREA = sum(x$AREA_KM2),
        CPUE_KGKM2_MEAN = weighted.mean(x = x$CPUE_KGKM2_MEAN,
                                        w = x$AREA_KM2,
                                        na.rm = TRUE),
        CPUE_NOKM2_MEAN = weighted.mean(x = x$CPUE_NOKM2_MEAN,
                                        w = x$AREA_KM2,
                                        na.rm = TRUE),
        N_HAUL = sum(x$N_HAUL), 
        N_WEIGHT = sum(x$N_WEIGHT), 
        N_COUNT = sum(x$N_COUNT), 
        N_LENGTH = sum(x$N_LENGTH),
        BIOMASS_MT = sum(x$BIOMASS_MT, na.rm = TRUE),
        BIOMASS_VAR = sum(x$BIOMASS_VAR, na.rm = TRUE),
        POPULATION_COUNT = round(x = sum(x$POPULATION_COUNT, na.rm = TRUE), 
                                 digits = 0),
        POPULATION_VAR = sum(x$POPULATION_VAR, na.rm = TRUE))
    
    ## Derive the variance associated with the mean weight and 
    ## numerical CPUE from the variances of the total biomass
    ## and total population, respectively. 
    result_df[, 
              c("CPUE_KGKM2_VAR", "CPUE_NOKM2_VAR") := 
                list(BIOMASS_VAR / TOT_AREA^2 * 1E6, 
                     POPULATION_VAR / TOT_AREA^2)]
    
    return(result_df)
  }
  
  subarea_biomass <- 
    subarea_biomass[, 
                    weighted_cpue(.SD), 
                    by = c("SURVEY_DEFINITION_ID", "SURVEY", 
                           "DESIGN_YEAR", "YEAR", "SPECIES_CODE", "AREA_ID")]
  
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
    
    subarea_biomass <- subarea_biomass[!(SURVEY_DEFINITION_ID == 98 & 
                                           YEAR < 1987 & 
                                           AREA_ID %in% c(99900, 
                                                          300, 200, 100, 
                                                          8, 9))] 
  }
  
  return(subarea_biomass[, -c("TOT_AREA", "DESIGN_YEAR")])
}
