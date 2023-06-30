#' Calculate size composition across aggregated subareas
#'
#' @param racebase_tables data object created from `gapindex::get_data()``
#' @param size_comps  a dataframe of stratum biomass, result object from either
#'                       `gapindex::calc_sizecomp_aigoa_stratum()` or 
#'                       `gapindex::calc_sizecomp_bs_stratum()`
#'
#' @return dataframe of numbers-at-length by survey, year, subarea (AREA_ID), species, and sex
#'
#' | Field Name           | Description                                                                                                                                                         |
#' |----------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' | SURVEY_DEFINITION_ID | Integer number identifier corresponding to survey region. See   gapindex::survey_ids for a list of relevant survey regions.                                         |
#' | SURVEY               | Survey region.                                                                                                                                                      |
#' | YEAR                 | Survey year.                                                                                                                                                        |
#' | AREA_ID              | Integer identifier for a subarea. See   gapindex::area_table for the full list.                                                                                     |
#' | SPECIES_CODE         | Taxon code. [See the code book for the full list.](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).   |
#' | LENGTH_MM            | Length bin (mm).                                                                                                                                                    |
#' | SEX                  | Sex code where "1" = "Male", "2" =   "Female", "3" = Unsexed.                                                                                                       |
#' | POPULATION_COUNT     | Total number of individuals.                                                                                                                                        |
#' 
#' @export
#'

calc_sizecomp_subareas <- function(racebase_tables, 
                                   size_comps) {
  
  subarea_size_comp_df <- data.frame()
  survey_designs <- racebase_tables$survey
  
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
        
        subarea_size_comp <- 
          subset(x = size_comps,
                 subset = SURVEY_DEFINITION_ID == 
                   subareas$SURVEY_DEFINITION_ID[isubarea] &
                   STRATUM %in% strata_in_subarea$STRATUM)
        
        if (nrow(x = subarea_size_comp) == 0) next
        
        subarea_summed_sizecomp <- 
          stats::aggregate(POPULATION_COUNT ~ 
                             YEAR + SPECIES_CODE + SEX + LENGTH_MM,
                           data = subarea_size_comp,
                           FUN = sum)
        
        subarea_size_comp_df <- 
          rbind(
            subarea_size_comp_df,
            cbind(data.frame(AREA_ID = subareas$AREA_ID[isubarea],
                             SURVEY_DEFINITION_ID = subareas$SURVEY_DEFINITION_ID[isubarea]),
                  subarea_summed_sizecomp[, c("SPECIES_CODE", "YEAR", 
                                              "SEX", "LENGTH_MM", 
                                              "POPULATION_COUNT")]))
      }
    }
  }
  
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

