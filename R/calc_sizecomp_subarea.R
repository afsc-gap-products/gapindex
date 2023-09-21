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
  subarea_size_comp_df <- data.frame()
  survey_designs <- racebase_tables$survey
  
  for (isurvey in 1:nrow(x = survey_designs)) { ## Loop over surveys -- start
    
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
        
        ## Subset size comps for only the strata in isubarea
        subarea_size_comp <- 
          subset(x = size_comps,
                 subset = SURVEY_DEFINITION_ID == 
                   subareas$SURVEY_DEFINITION_ID[isubarea] &
                   STRATUM %in% strata_in_subarea$STRATUM)
        
        if (nrow(x = subarea_size_comp) == 0) next
        
        ## Aggregate size comps within the isubarea
        subarea_summed_sizecomp <- 
          stats::aggregate(POPULATION_COUNT ~ 
                             YEAR + SPECIES_CODE + SEX + LENGTH_MM,
                           data = subarea_size_comp,
                           FUN = sum)
        
        ## append to result df
        subarea_size_comp_df <- 
          rbind( 
            subarea_size_comp_df,
            cbind(
              data.frame(
                AREA_ID = subareas$AREA_ID[isubarea],
                SURVEY_DEFINITION_ID = subareas$SURVEY_DEFINITION_ID[isubarea]),
              subarea_summed_sizecomp[, c("SPECIES_CODE", "YEAR", 
                                          "SEX", "LENGTH_MM", 
                                          "POPULATION_COUNT")]
            )
          )
      }
      
    } ## Loop over subareas -- end
  } ## Loop over surveys -- end
  
  ## Reorder columns, sort, and return
  subarea_size_comp_df <- 
    subset(x = subarea_size_comp_df,
           select = c(SURVEY_DEFINITION_ID, YEAR, AREA_ID,
                      SPECIES_CODE, LENGTH_MM, SEX, POPULATION_COUNT))
  
  return(subarea_size_comp_df[order(subarea_size_comp_df$SURVEY_DEFINITION_ID,
                                    subarea_size_comp_df$AREA_ID,
                                    subarea_size_comp_df$SPECIES_CODE,
                                    subarea_size_comp_df$YEAR,
                                    subarea_size_comp_df$SEX,
                                    subarea_size_comp_df$LENGTH_MM), ])
}

