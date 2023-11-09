#' Calculate region-level age composition and mean/std.dev length at age 
#' 
#' @param racebase_tables data object created from `gapindex::get_data()``
#' @param age_comps_stratum  a named list of stratum age comps and numbers by 
#'                           survey/year/stratum/sex/length, a result object 
#'                           from `gapindex::calc_agecomp_stratum()`. 
#'
#' @return dataframe of age composition and mean/standard deviation of length
#'         at age aggregated across regions. 
#'         
#' @eval c("@return", get_table_metadata("inst/extdata/metadata.csv", 
#' select = c("SURVEY_DEFINITION_ID", "SURVEY", "AREA_ID", "SPECIES_CODE" ,
#' "YEAR", "SEX", "AGE", "POPULATION_COUNT", "LENGTH_MM_MEAN", "LENGTH_MM_SD")))
#'   
#' @export
#'

# racebase_tables <- gapindex_data
# age_comps_stratum <- gapindex_agecomp_stratum

calc_agecomp_region <- function(racebase_tables,
                                age_comps_stratum) {
  
  ## Error checks on function arguments
  if (is.null(x = racebase_tables))
    stop("Must provide argument `racebase_tables` a named list from 
         gapindex::get_data().")
  if (is.null(x = age_comps_stratum))
    stop("Please supply an age composition object creted using
         `gapindex::calc_agecomp_stratum()`")
  
  ## Extract objects within function arguments
  survey_designs <- racebase_tables$survey
  strata <- racebase_tables$strata
  age_comps <- age_comps_stratum$age_comp
  count_length_age <- age_comps_stratum$length_at_age
  
  ## Empty dataframe to append region-specific age composition
  region_age_comp_df <- data.frame()
  
  ## Loop over surveys and regions and aggregate age comps and mena/sd length
  ## over the appropriate strata
  for (isurvey in 1:nrow(x = survey_designs)) { ## Loop over surveys -- start
    
    ## Extract the different regions to calculate over. Currently, age comps
    ## and mean/sd lengths are only being aggregated over regions. This 
    ## excludes calculations across subareas (e.g., Shumagin or 1-100 m). 
    subareas <- subset(x = racebase_tables$subarea,
                       subset = SURVEY_DEFINITION_ID == 
                         survey_designs$SURVEY_DEFINITION_ID[isurvey] &
                         AREA_TYPE == "REGION" &
                         DESIGN_YEAR == survey_designs$DESIGN_YEAR[isurvey])
    
    for (iregion in 1:nrow(x = subareas)) { ## Loop over regions -- start
      
      ## Subset strata contained within the given region
      strata_in_iregion <- 
        subset(x = racebase_tables$stratum_groups,
               subset = AREA_ID %in% subareas$AREA_ID[iregion])$STRATUM
      
      if (length(x = strata_in_iregion) > 0) {
        
        ## subset `age_comp` records to the given survey area and 
        ## appropriate strata
        age_comp_iregion_by_strata <- 
          subset(x = age_comps,
                 subset = SURVEY_DEFINITION_ID == 
                   subareas$SURVEY_DEFINITION_ID[iregion] &
                   STRATUM %in% strata_in_iregion)
        
        ## Total numbers by "SURVEY", "YEAR", "SPECIES_CODE", "SEX", and "AGE",
        ## aggregating over strata
        age_comp_iregion <-
          stats::aggregate(POPULATION_COUNT ~ SURVEY + YEAR + SPECIES_CODE + 
                             SEX + AGE,
                           data = age_comp_iregion_by_strata,
                           FUN = sum)
        
        ##  Calculate mean and sd length at age:
        ##  Calculate the weighted mean and sd of length at age using column 
        ## "POPULATION_COUNT" as the weight.
        mean_length_at_age <- 
          do.call(what = rbind,
                  args = lapply(
                    X = split(
                      x = subset(x = count_length_age,
                                 subset = STRATUM %in% strata_in_iregion),
                      f = with(subset(x = count_length_age,
                                      subset = STRATUM %in% strata_in_iregion), 
                               list(SURVEY, YEAR, SPECIES_CODE, SEX, AGE)
                      )
                    ),
                    ## Within each sublist of split_df, calculate the 
                    ## weighted mean and std.dev of length
                    FUN = function(df) {
                      
                      ## temporary output df
                      output_df <- data.frame()
                      
                      if (nrow(x = df) > 0) {
                        
                        ## record the combo of "SURVEY", "YEAR", 
                        ## "SPECIES_CODE", "SEX", and "AGE" of the sublist. 
                        output_df[1, c("SURVEY", "YEAR", "SPECIES_CODE", 
                                       "SEX", "AGE")] <- 
                          unique(x = subset(x = df, 
                                            select = c(SURVEY, YEAR, 
                                                       SPECIES_CODE, 
                                                       SEX, AGE)))
                        
                        ## weighted mean length
                        mean_length <- stats::weighted.mean(x = df$LENGTH_MM, 
                                                            w = df$AGEPOP)
                        
                        ## weighted std.dev length
                        sd_length <- 
                          sqrt(x = sum(df$AGEPOP/sum(df$AGEPOP) * 
                                         (df$LENGTH_MM - mean_length)^2))
                        
                        ## append `mean_length` and `sd_length` to `output_df` 
                        output_df[1, c("LENGTH_MM_MEAN", "LENGTH_MM_SD")] <-
                          round(x = c(mean_length, sd_length), 
                                digits = 2)
                      } 
                      
                      return(output_df)
                      
                    }))
        rownames(x = mean_length_at_age) <- NULL
        
        ## Merge the mean/sd length at age dataframes to `age_comp_iregion`
        ## using  "SURVEY", "YEAR", "SPECIES_CODE", "SEX", and "AGE"
        ## as a composite key.
        age_comp_iregion <- 
          merge(x = age_comp_iregion,
                y = rbind(mean_length_at_age),
                by = c("SURVEY", "YEAR", "SPECIES_CODE", "SEX", "AGE"))
        
        ## Append AREA_ID and SURVEY_DEFINITION_ID to `age_comp_iregion`
        age_comp_iregion <- cbind(
          data.frame(
            SURVEY = age_comp_iregion$SURVEY,
            SURVEY_DEFINITION_ID = subareas$SURVEY_DEFINITION_ID[iregion],
            AREA_ID = subareas$AREA_ID[iregion]),
          age_comp_iregion[, c("YEAR", "SPECIES_CODE",  "SEX", 
                               "AGE", "POPULATION_COUNT", 
                               "LENGTH_MM_MEAN", "LENGTH_MM_SD")]
        )
        
        ## Append `age_comp_iregion` to `region_age_comp_df`
        region_age_comp_df <- rbind(region_age_comp_df, age_comp_iregion)
        
      }
    } ## Loop over regions -- end
  } ## Loop over surveys -- end
  
  region_age_comp_df[region_age_comp_df$AGE == -99, 
                     c("LENGTH_MM_MEAN", "LENGTH_MM_SD")] <- NA
  
  ## Remove EBS + NW subarea estimates prior to 1987
  if (any(region_age_comp_df$YEAR < 1987 & region_age_comp_df$AREA_ID == 99900)) {
    warning("The (EBS + NW) output only includes years 1987-present.
      Years 1982-1986 are NOT included for the (EBS + NW) output because
      essentially no stations within strata 82 & 90 (subarea 8 & 9)
      were sampled during those years. Biomass/Abundance estimates for 
      these early years were removed.")
    
    region_age_comp_df <- subset(x = region_age_comp_df, 
                              subset = !(SURVEY_DEFINITION_ID == 98 & 
                                           YEAR < 1987 & 
                                           AREA_ID == 99900) )
  }
  
  return(region_age_comp_df[with(region_age_comp_df, 
                                 order(SURVEY_DEFINITION_ID, AREA_ID,
                                       SPECIES_CODE, YEAR, SEX, AGE)), ])
}

