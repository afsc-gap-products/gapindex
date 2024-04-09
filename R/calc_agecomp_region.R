#' Calculate region-level age composition and mean/std.dev length at age 
#' 
#' @param racebase_tables `r lifecycle::badge("deprecated")` Use the 
#'                        `gapdata` argument instead. 
#' @param gapdata data object created from `gapindex::get_data()`
#' @param age_comps_stratum `r lifecycle::badge("deprecated")` Use the 
#'                          `agecomp_stratum` argument instead. 
#' @param agecomp_stratum  a named list of stratum age comps and numbers by 
#'                           survey/year/stratum/sex/length, a result object 
#'                           from `gapindex::calc_agecomp_stratum()`. 
#'
#' @return dataframe of age composition and mean/standard deviation of length
#'         at age aggregated across regions. 
#'         
#' @eval c("@return", get_table_metadata("inst/extdata/metadata.csv", 
#' select = c("SURVEY_DEFINITION_ID", "AREA_ID", "AREA_ID_FOOTPRINT", 
#'            "SPECIES_CODE", "YEAR", "SEX", "AGE", "POPULATION_COUNT", 
#'            "LENGTH_MM_MEAN", "LENGTH_MM_SD")))
#' @export
#'

calc_agecomp_region <- function(gapdata = NULL,
                                racebase_tables = lifecycle::deprecated(),
                                agecomp_stratum = NULL,
                                age_comps_stratum = lifecycle::deprecated()) {
  
  ## Input Check
  if (lifecycle::is_present(racebase_tables)) {
    lifecycle::deprecate_warn("2.2.0", 
                              "calc_agecomp_region(racebase_tables)", 
                              "calc_agecomp_region(gapdata)")
    gapdata <- racebase_tables
  }
  if (lifecycle::is_present(age_comps_stratum)) {
    lifecycle::deprecate_warn("2.2.0", 
                              "calc_agecomp_region(size_comp)", 
                              "calc_agecomp_region(sizecomp_stratum)")
    agecomp_stratum <- age_comps_stratum
  }
  
  for (iarg in c("gapdata", "agecomp_stratum"))
    if (is.null(x = get(x = iarg)))
      stop(paste0("Must provide argument `", iarg, "`. ",
                  "See ?gapindex::calc_agecomp_region for more information"))
  
  ## Extract objects within function arguments
  survey_designs <- unique_surveys <- gapdata$survey
  subareas <- gapdata$subarea[AREA_TYPE == 'REGION']
  stratum_groups <- gapdata$stratum_groups[AREA_ID %in% subareas$AREA_ID]
  strata <- gapdata$strata
  
  ## Create set of subareas given the different surveys and design years. From 
  ## 2025-on the GOA time series will have two unique survey designs with 
  ## different design years.
  subarea_age_comp <- data.table::data.table()
  for (irow in 1:nrow(x = unique_surveys))
    subarea_age_comp <- 
    rbind(subarea_age_comp,
          cbind(YEAR = unique_surveys$YEAR[irow],
                stratum_groups[
                  SURVEY_DEFINITION_ID == 
                    unique_surveys$SURVEY_DEFINITION_ID[irow] &
                    SURVEY == 
                    unique_surveys$SURVEY[irow] &
                    DESIGN_YEAR == 
                    unique_surveys$DESIGN_YEAR[irow]]))
  
  ## Append the stratum-level age compositions and mean/sd length @ age
  ## to the `subarea_age_comp` df
  subarea_age_comp <- merge(x = subarea_age_comp,
                            y = agecomp_stratum$length_at_age,
                            by = c("SURVEY_DEFINITION_ID", 
                                   "YEAR", "STRATUM"),
                            all = TRUE, allow.cartesian = TRUE)
  
  ## Aggregate age composition and mean/sd length @ age across region
  subarea_age_comp <-
    subarea_age_comp[
      POPULATION_COUNT > 0
      ,
      .(POPULATION_COUNT = round(x = sum(POPULATION_COUNT), digits = 0), 
        LENGTH_MM_MEAN = round(x = stats::weighted.mean(x = LENGTH_MM, 
                                                        w = POPULATION_COUNT), 
                               digits = 2),
        LENGTH_MM_SD = round(x = sqrt(
          x = sum(POPULATION_COUNT/sum(POPULATION_COUNT) * 
                    (LENGTH_MM - stats::weighted.mean(x = LENGTH_MM, 
                                                      w = POPULATION_COUNT))^2)
        ), 
        digits = 2) ),
      by = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR", "AREA_ID", 
             "SPECIES_CODE", "SEX", "AGE")]
  
  subarea_age_comp[AGE == -99, c("LENGTH_MM_MEAN", "LENGTH_MM_SD")] <- NA
  
  ## Remove EBS + NW subarea estimates prior to 1987
  if (any(subarea_age_comp$YEAR < 1987 & subarea_age_comp$AREA_ID == 99900)) {
    warning("The (EBS + NW) output only includes years 1987-present.
      Years 1982-1986 are NOT included for the (EBS + NW) output because
      essentially no stations within strata 82 & 90 (subarea 8 & 9)
      were sampled during those years. Biomass/Abundance estimates for
      these early years were removed.")
    
    subarea_age_comp <- subset(x = subarea_age_comp,
                               subset = !(SURVEY_DEFINITION_ID == 98 &
                                            YEAR < 1987 & AREA_ID == 99900))
  }
  
  return(
    data.table::data.table(
      subarea_age_comp[order(SURVEY_DEFINITION_ID, AREA_ID, SPECIES_CODE, YEAR,
                             SEX, AGE),
                       -"SURVEY"],
      key = c("SURVEY_DEFINITION_ID", "AREA_ID", "SPECIES_CODE", "YEAR", 
              "SEX", "AGE")
    )
  )
}
