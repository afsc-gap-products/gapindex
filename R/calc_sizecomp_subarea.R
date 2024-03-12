#' Calculate size composition across aggregated subareas
#'
#' @param racebase_tables data object created from `gapindex::get_data()``
#' @param size_comps  a dataframe of stratum biomass, result object from either
#'                       `gapindex::calc_sizecomp_aigoa_stratum()` or 
#'                       `gapindex::calc_sizecomp_bs_stratum()`
#'
#' @eval c("@return", get_table_metadata("inst/extdata/metadata.csv", 
#' select = c("SURVEY_DEFINITION_ID", "YEAR", "AREA_ID", "SPECIES_CODE" ,
#' "YEAR", "SEX", "LENGTH_MM", "POPULATION_COUNT")))
#' 
#' @export
#'

calc_sizecomp_subarea <- function(racebase_tables, 
                                  size_comps) {
  
  ## Error Check on function arguments
  if (is.null(x = racebase_tables))
    stop("Must provide argument `racebase_tables` a named list from 
         gapindex::get_data().")
  
  if (is.null(x = size_comps))
    stop("Must provide argument `size_comps` a named list from 
         gapindex::calc_sizecomp_stratum().")
  
  ## Which survey designs to pull from
  subarea_size_comp_df <- data.table::data.table()
  survey_designs <- racebase_tables$survey
  unique_surveys <- racebase_tables$survey
  stratum_groups <- racebase_tables$stratum_groups
  
  ## Add DESIGN_YEAR to size_comps
  size_comps <- unique_surveys[size_comps,
                 on = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR")]
  
  ## Create ## Subset the set of subareas given the survey and design year. From 
  ## 2025-on the GOA time series will have two unique survey designs with 
  ## different design years.
  subarea_size_comp <- data.table::data.table()
  for (irow in 1:nrow(x = unique_surveys))
    subarea_size_comp <- 
    rbind(subarea_size_comp,
          cbind(YEAR = unique_surveys$YEAR[irow],
                stratum_groups[
                  SURVEY_DEFINITION_ID == 
                    unique_surveys$SURVEY_DEFINITION_ID[irow] &
                    SURVEY == 
                    unique_surveys$SURVEY[irow] &
                    DESIGN_YEAR == 
                    unique_surveys$DESIGN_YEAR[irow]]))
  
  subarea_size_comp <- 
    subarea_size_comp[size_comps,
                    on = c("SURVEY_DEFINITION_ID", "SURVEY", 
                           "DESIGN_YEAR", "YEAR ", "STRATUM"),
                    allow.cartesian = TRUE]
  
  ## Aggregate size comps
  subarea_size_comp <- 
    subarea_size_comp[,
                      .(POPULATION_COUNT = sum(POPULATION_COUNT)),
                      by = c("SURVEY_DEFINITION_ID", "SURVEY", "DESIGN_YEAR",
                             "AREA_ID", "YEAR", "SPECIES_CODE", 
                             "SEX", "LENGTH_MM")]
  
 ## Reorder columns, sort, and return
  subarea_size_comp <- 
    subarea_size_comp[
      order(SURVEY_DEFINITION_ID, AREA_ID, SPECIES_CODE, YEAR, SEX, LENGTH_MM),
      c("SURVEY_DEFINITION_ID", "YEAR", "AREA_ID", "SPECIES_CODE", 
        "LENGTH_MM", "SEX", "POPULATION_COUNT")]
  
  return(subarea_size_comp)
}

