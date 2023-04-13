#' Calculate total numbers at age.  
#' 
#' @description To be used for the GOA/AI regions. Estimates are only for the 
#'                  entire region, not by stratum. 
#' 
#' @param racebase_tables data object created from `gapindex::get_data()`
#' @param size_comp a dataframe of stratum abundances, result 
#'                               object from either `calc_size_stratum_AIGOA()`
#'                               or `calc_size_stratum_BS()` depending on the
#'                               region.
#' 
#' @export
#' 

calc_age_comp <- function(racebase_tables = NULL,
                          size_comp = NULL) {
  
  ## Extract data
  haul <- racebase_tables$haul
  haul$YEAR <- as.numeric(x = format(x = haul$START_TIME, format = "%Y"))
  
  ## Narrow size_comp so that each record refers to a sex/length combination
  size_comp <- 
    rbind(data.frame(size_comp[, c("SURVEY", "YEAR", "STRATUM", 'SPECIES_CODE', 
                                   "LENGTH_MM")],
                     SEX = 1, 
                     sizepop = size_comp$MALES ),
          data.frame(size_comp[, c("SURVEY", "YEAR", "STRATUM", 'SPECIES_CODE', 
                                   "LENGTH_MM")],
                     SEX = 2, 
                     sizepop = size_comp$FEMALES),
          data.frame(size_comp[, c("SURVEY", "YEAR", "STRATUM", 'SPECIES_CODE',
                                   "LENGTH_MM")],
                     SEX = 3,
                     sizepop = size_comp$UNSEXED)
    )
  
  ## remove NA ages from specimen df and then merge HAUL information from the 
  ##    df, joining by HAULJOIN and YEAR
  specimen <- merge(x = subset(x = racebase_tables$specimen,
                               subset = !is.na(AGE)), 
                    y = haul[, c("HAULJOIN", "YEAR")],
                    by = "HAULJOIN",
                    all.x = TRUE)
  
  ## attach Survey name based on CRUISEJOIN
  specimen <-
    merge(x = specimen,
          y = racebase_tables$cruise[, c("CRUISEJOIN", "SURVEY")],
          by = c("CRUISEJOIN"))
  
  ## In AIGOA, specimen from hauls where ABUNDANCE_HAUL == "N" are included in 
  ## the age composition calculation. These will not be included in the standard
  ## call to gapindex::get_data(). After the merge call above, the years for
  ## these non-abundance hauls will be NA. Here, we fill in those year values.
  specimen$YEAR <- ifelse(test = is.na(specimen$YEAR), 
                          no = specimen$YEAR,
                          yes = as.numeric(substr(x = specimen$CRUISE, 
                                                  start = 1, stop = 4)))
  names(specimen)[names(specimen) == "LENGTH"] <- "LENGTH_MM"
  specimen$FREQ <- 1
  
  ## s_ijklm will be the total number of collected ages of species-k, sex-m, 
  ##     and length-l for station-j in stratum-i.
  s_ijklm <- rbind(
    ## Aggregate FEMALES and MALES
    stats::aggregate(
      FREQ ~ SURVEY + YEAR + SPECIES_CODE + SEX + LENGTH_MM + AGE,
      data = specimen,
      FUN = sum),
    
    ## For UNSEXED, we pool MALES and FEMALES TOGETHER
    cbind(
      stats::aggregate(FREQ ~ SURVEY + YEAR + SPECIES_CODE + LENGTH_MM + AGE,
                       data = specimen,
                       FUN = sum), 
      SEX = 3)[, c("SURVEY", "YEAR", "SPECIES_CODE", "SEX", 
                   "LENGTH_MM", "AGE", "FREQ")])
  
  ## Split the s_ijklm df by year, species, sex, and length bin 
  ##    and then calculate the proportion of distribution of ages within 
  ##    each split (p_klm, proportion of age for species-k in length-l,
  ##    and sex-m aggregated across stations and strata). 
  ##    Then, rbind the splitted lists back together into one df.
  p_klm <- 
    do.call(what = rbind, 
            args = lapply(X = split(x = s_ijklm, 
                                    f = list(s_ijklm$SURVEY,
                                             s_ijklm$YEAR, 
                                             s_ijklm$SPECIES_CODE, 
                                             s_ijklm$LENGTH_MM, 
                                             s_ijklm$SEX)),
                          FUN = function(df) {
                            df$age_frac <- df$FREQ / sum(df$FREQ)
                            return(df)
                          }))
  
  ## Merge the numbers at length information in size_comp to p_klm by joinin
  ##    year, species, sex, and length. 
  age_comp <- 
    merge(x = p_klm,
          y = size_comp,
          by = c("SURVEY", "YEAR", "SPECIES_CODE", "SEX", "LENGTH_MM"),
          all.y = TRUE)
  
  ## Age -9 are for sizes where no ages were collected. These individuals are
  ## summed across sizes. 
  age_comp$AGE[is.na(age_comp$AGE)] <- -9
  age_comp$age_frac[is.na(age_comp$age_frac)] <- 1
  
  ## Then the numbers at age is the product of the age_frac and the numbers 
  ##    at length
  
  age_comp$AGEPOP <- age_comp$age_frac * age_comp$sizepop
  
  age_comp <- aggregate(AGEPOP ~ SURVEY + YEAR + SPECIES_CODE + SEX + AGE,
                        data = age_comp,
                        FUN = function(x) round(x = sum(x)))
  
  ## Merge Area Descriptions
  age_comp <- merge(x = age_comp, 
                    y = gapindex::design_table,
                    by = c("SURVEY", "YEAR"))
  
  ## Attach region-specific AREA_IDs
  age_comp <- merge(x = age_comp,
                y = subset(x = gapindex::new_stratum_table,
                           subset = TYPE == "REGION",
                           select = c("SURVEY_DEFINITION_ID", "DESIGN_YEAR", 
                                      "AREA_ID")),
                by = c("SURVEY_DEFINITION_ID", "DESIGN_YEAR"))
  
  ## Reorder
  age_comp <- age_comp[order(age_comp$SPECIES_CODE, age_comp$AREA_ID), ]
  
  return(subset(x = age_comp,
                select = c(AREA_ID, SURVEY_DEFINITION_ID,
                           SPECIES_CODE, YEAR, SEX, AGE, AGEPOP)))
  
}
