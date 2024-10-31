#' Calculate index of total biomass across aggregated subareas
#'
#' @param racebase_tables `r lifecycle::badge("deprecated")` Use the 
#'                        `gapdata` argument instead. 
#' @param gapdata data object created from `gapindex::get_data()`
#' @param biomass_stratum a dataframe of stratum biomass, result object from 
#'                       `gapindex::calc_biomass_stratum()`
#' @param biomass_strata  `r lifecycle::badge("deprecated")` Use the 
#'                        `biomass_stratum` argument instead. 
#'
#' @eval c("@return", get_table_metadata("inst/extdata/metadata.csv", 
#' select = c("SURVEY_DEFINITION_ID", "SURVEY", "AREA_ID", "SPECIES_CODE" ,
#' "YEAR", "N_HAUL", "N_WEIGHT", "N_COUNT", "N_LENGTH", "CPUE_KGKM2_MEAN",
#' "CPUE_KGKM2_VAR", "CPUE_NOKM2_MEAN", "CPUE_NOKM2_VAR", "BIOMASS_MT",
#' "BIOMASS_VAR", "POPULATION_COUNT", "POPULATION_VAR")))
#' 
#' @export
#' 

calc_biomass_subarea <- function(racebase_tables = lifecycle::deprecated(),
                                 gapdata = NULL,
                                 biomass_strata = lifecycle::deprecated(),
                                 biomass_stratum = NULL) {
  
  ## Input checks
  if (lifecycle::is_present(racebase_tables)) {
    lifecycle::deprecate_warn("3.0.0", 
                              "calc_biomass_subarea(racebase_tables)", 
                              "calc_biomass_subarea(gapdata)")
    gapdata <- racebase_tables
  }
  
  if (lifecycle::is_present(biomass_strata)) {
    lifecycle::deprecate_warn("3.0.0", 
                              "calc_biomass_subarea(biomass_strata)", 
                              "calc_biomass_subarea(biomass_stratum)")
    biomass_stratum <- biomass_strata
  }

  for (iarg in c("gapdata", "biomass_stratum"))
    if (is.null(x = get(x = iarg)))
      stop(paste0("Must provide argument `", iarg, "`. ",
                  "See ?gapindex::calc_biomass_subarea for more information"))
  
  ## Which survey designs to pull from
  survey_designs <- gapdata$survey_design
  unique_surveys <- gapdata$survey
  strata <- gapdata$strata
  stratum_groups <- gapdata$stratum_groups
  
  ## Attach "DESIGN_YEAR" from `survey_designs` to `biomass_stratum` using 
  ## "SURVEY_DEFINITION_ID", "SURVEY", "YEAR" as a composite key.
  biomass_stratum <- 
    merge(x =  biomass_stratum,
          y = unique_surveys,
          by = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR"))
  
  ## Attach "AREA_KM2" from `strata` to `biomass_stratum` using 
  ## "SURVEY_DEFINITION_ID", "SURVEY", "STRATUM" as a composite key.
  biomass_stratum <- merge(x = biomass_stratum,
                           y = strata[, c("SURVEY_DEFINITION_ID", "SURVEY", 
                                          "STRATUM", "AREA_KM2")],
                           by = c("SURVEY_DEFINITION_ID", "SURVEY", "STRATUM"))
  
  ## Create set of subareas given the different surveys and design years. From 
  ## 2025-on the GOA time series will have two unique survey designs with 
  ## different design years.
  biomass_subarea <- data.table::data.table()
  for (irow in 1:nrow(x = unique_surveys))
    biomass_subarea <- 
    rbind(biomass_subarea,
          cbind(YEAR = unique_surveys$YEAR[irow],
                stratum_groups[
                  SURVEY_DEFINITION_ID == 
                    unique_surveys$SURVEY_DEFINITION_ID[irow] &
                    SURVEY == 
                    unique_surveys$SURVEY[irow] &
                    DESIGN_YEAR == 
                    unique_surveys$DESIGN_YEAR[irow]]))
  
  biomass_subarea <- 
    merge(x = biomass_subarea,
          y = biomass_stratum,
          by = c("SURVEY_DEFINITION_ID", "SURVEY", 
                 "DESIGN_YEAR", "YEAR", "STRATUM"), 
          allow.cartesian = TRUE)
  
  ## Create a function `weighted_cpue` that calculates combines weighted, 
  ## stratum-level estimates to the subarea level.
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
  
  ## Perform the weighted_cpue across subareas
  biomass_subarea <- 
    biomass_subarea[, 
                    weighted_cpue(.SD), 
                    by = c("SURVEY_DEFINITION_ID", "SURVEY", 
                           "DESIGN_YEAR", "YEAR", "SPECIES_CODE", "AREA_ID")]
  
  ## Warning Messages
  if ( 47 %in% biomass_subarea$SURVEY_DEFINITION_ID & 
       2025 %in% biomass_subarea$YEAR) {
    warning("The GOA total biomass across INPFC area and across depth zones
              only includes years 1987-2023. Starting from 2025, only total 
              biomass across NMFS areas will be reported.")
  }
  
  ## Remove EBS + NW subarea estimates prior to 1987
  if (any(biomass_subarea$YEAR < 1987 & biomass_subarea$AREA_ID == 99900)) {
    warning("The (EBS + NW) output only includes years 1987-present.
      Years 1982-1986 are NOT included for the (EBS + NW) output because
      essentially no stations within strata 82 & 90 (subarea 8 & 9)
      were sampled during those years. Biomass/Abundance estimates for 
      these early years were removed.")
    
    biomass_subarea <- subset(x = biomass_subarea,
                              subset = !(SURVEY_DEFINITION_ID == 98 & 
                                           YEAR < 1987 & 
                                           AREA_ID %in% c(99900, 
                                                          300, 200, 100, 
                                                          8, 9)))
  }
  
  return(data.table::data.table(
    biomass_subarea[, -c("TOT_AREA", "DESIGN_YEAR")],
    key = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR", "SPECIES_CODE", "AREA_ID")) )
}
