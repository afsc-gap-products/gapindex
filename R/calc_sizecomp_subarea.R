#' Calculate size composition across aggregated subareas
#'
#' @param racebase_tables `r lifecycle::badge("deprecated")` Use the 
#'                     `gapdata` argument instead. 
#' @param gapdata data object created from `gapindex::get_data()`
#' @param size_comps  `r lifecycle::badge("deprecated")` Use the 
#'                    `sizecomp_stratum` argument instead. 
#' @param sizecomp_stratum a dataframe of stratum biomass, result object from either
#'                       `gapindex::calc_sizecomp_aigoa_stratum()` or 
#'                       `gapindex::calc_sizecomp_bs_stratum()`
#'
#' @eval c("@return", get_table_metadata("inst/extdata/metadata.csv", 
#' select = c("SURVEY_DEFINITION_ID", "YEAR", "AREA_ID", "SPECIES_CODE" ,
#' "YEAR", "SEX", "LENGTH_MM", "POPULATION_COUNT")))
#' 
#' @export
#'

calc_sizecomp_subarea <- function(gapdata = NULL,
                                  racebase_tables = lifecycle::deprecated(), 
                                  sizecomp_stratum = NULL,
                                  size_comps = lifecycle::deprecated()) {
  ## Input check
  if (lifecycle::is_present(racebase_tables)) {
    lifecycle::deprecate_warn("2.2.0", 
                              "calc_sizecomp_subarea(racebase_tables)", 
                              "calc_sizecomp_subarea(gapdata)")
    gapdata <- racebase_tables
  }
  if (lifecycle::is_present(size_comps)) {
    lifecycle::deprecate_warn("2.2.0", 
                              "calc_sizecomp_subarea(size_comps)", 
                              "calc_sizecomp_subarea(sizecomp_stratum)")
    sizecomp_stratum <- size_comps
  }
  
  for (iarg in c("gapdata", "sizecomp_stratum"))
    if (is.null(x = get(x = iarg)))
      stop(paste0("Must provide argument `", iarg, "`. ",
                  "See ?gapindex::calc_sizecomp_subarea for more information"))
  
  ## Which survey designs to pull from
  survey_designs <- gapdata$survey
  unique_surveys <- gapdata$survey
  stratum_groups <- gapdata$stratum_groups
  
  ## Add DESIGN_YEAR to sizecomp_stratum
  sizecomp_stratum <- merge(x = sizecomp_stratum,
                            y = unique_surveys,
                            by = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR"))
  
  ## Create set of subareas given the different surveys and design years. From 
  ## 2025-on the GOA time series will have two unique survey designs with 
  ## different design years.
  sizecomp_subarea <- data.table::data.table()
  for (irow in 1:nrow(x = unique_surveys))
    sizecomp_subarea <- 
    rbind(sizecomp_subarea,
          cbind(
            YEAR = unique_surveys$YEAR[irow],
            stratum_groups[
              SURVEY_DEFINITION_ID == 
                unique_surveys$SURVEY_DEFINITION_ID[irow] &
                SURVEY == 
                unique_surveys$SURVEY[irow] &
                DESIGN_YEAR == 
                unique_surveys$DESIGN_YEAR[irow]]))
  
  ## Merge the stratum size comps to the newly created sizecomp_subarea table
  sizecomp_subarea <- merge(x = sizecomp_stratum,
                            y = sizecomp_subarea,
                            by = c("SURVEY_DEFINITION_ID", "SURVEY", 
                                   "DESIGN_YEAR", "YEAR", "STRATUM"),
                            all = TRUE, allow.cartesian = TRUE)
  
  ## Aggregate size comps
  sizecomp_subarea <- 
    sizecomp_subarea[,
                     .(POPULATION_COUNT = sum(POPULATION_COUNT)),
                     by = c("SURVEY_DEFINITION_ID", "SURVEY", "DESIGN_YEAR",
                            "AREA_ID", "YEAR", "SPECIES_CODE", 
                            "SEX", "LENGTH_MM")]
  
  ## Reorder columns, sort, and return
  sizecomp_subarea <- 
    sizecomp_subarea[
      order(SURVEY_DEFINITION_ID, AREA_ID, SPECIES_CODE, YEAR, SEX, LENGTH_MM),
      c("SURVEY_DEFINITION_ID", "YEAR", "AREA_ID", "SPECIES_CODE", 
        "LENGTH_MM", "SEX", "POPULATION_COUNT")]
  
  return(sizecomp_subarea)
}

