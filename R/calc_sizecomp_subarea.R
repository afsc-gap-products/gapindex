#' Calculate size composition across aggregated subareas
#'
#' @param racebase_tables data object created from `gapindex::get_data()``
#' @param size_comps  a dataframe of stratum biomass, result object from either
#'                       `gapindex::calc_size_stratum_AIGOA()` or 
#'                       `gapindex::calc_size_stratum_BS()`
#'
#' @return dataframe of size composition estimates across 
#'         subareas and across region.
#' @export
#'

calc_sizecomp_subareas <- function(racebase_tables, 
                                   size_comps) {
  
  ## Error checks
  # if (is.null(size_comps))
  #   stop("Please supply a size composition dataframe created using 
  #        `gapindex::calc_size_stratum_AIGOA()` or 
  #        `gapindex::calc_size_stratum_BS()`")
  # 
  # if (!region %in% c("EBS_STANDARD", "EBS_PLUSNW", "NBS", "GOA", "AI"))
  #   stop("Argument `region` must be one of these options: 
  #        EBS_STANDARD, EBS_PLUSNW, NBS, GOA, AI. " )
  # 
  # if (region == "GOA") {
  #   if (any(unique(size_comps$YEAR) > 2023))
  #     warning("The GOA total biomass across INPFC area and across depth zones
  #             only includes years 1987-2023. Starting from 2025, total biomass
  #             across NMFS areas will only be reported.")
  # }
  # 
  # if (region == "EBS_PLUSNW") {
  #   if ( any(unique(size_comps$YEAR) < 1987) ){
  #     
  #     stop("The (EBS + NW) output only includes years 1987-present. 
  #     Years 1982-1986 are NOT included for the (EBS + NW) output because 
  #     essentially no stations within strata 82 & 90 (subarea 8 & 9) 
  #     were sampled during those years.")
  #   }
  # } 
  
  subarea_size_comp_df <- data.frame()
  survey_designs <- racebase_tables$survey
  
  for (isurvey in 1:nrow(x = survey_designs)) { 
    
    subareas <- subset(x = gapindex::area_table,
                       subset = SURVEY_DEFINITION_ID == 
                         survey_designs$SURVEY_DEFINITION_ID[isurvey] &
                         TYPE != "STRATUM" &
                         DESIGN_YEAR == survey_designs$DESIGN_YEAR[isurvey])
    
    for (isubarea in 1:nrow(x = subareas)) {
      strata_in_subarea <- 
        subset(x = gapindex::stratum_groupings,
               subset = AREA_ID %in% subareas$AREA_ID[isubarea])
      
      if (nrow(x = strata_in_subarea) > 0) {
        
        subarea_size_comp <- 
          subset(x = size_comps,
                 subset = SURVEY_DEFINITION_ID == 
                   subareas$SURVEY_DEFINITION_ID[isubarea] &
                   STRATUM %in% strata_in_subarea$STRATUM)
        
        subarea_summed_sizecomp <- 
          stats::aggregate(POPULATION_COUNT ~ 
                             YEAR + SPECIES_CODE + SEX + LENGTH_MM,
                           data = subarea_size_comp,
                           FUN = sum)
        
        subarea_size_comp_df <- 
          rbind(
            subarea_size_comp_df,
            cbind(data.frame(AREA_ID = subareas$AREA_ID[isubarea],
                             SURVEY_DEFINITION_ID = subareas$SURVEY[isubarea]),
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

