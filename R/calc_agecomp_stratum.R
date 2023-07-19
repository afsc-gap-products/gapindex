#' Calculate stratum-level age composition and mean/std.dev length at age 
#' 
#' @param racebase_tables data object created from `gapindex::get_data()`
#' @param alk age-length key (dataframe) created from `gapindex::calc_ALK()`
#' @param size_comp a dataframe of size compositions created from 
#'                  `gapindex::calc_sizecomp_stratum()`.
#'
#' @return A named list with two elements. "age_comp" is a dataframe of numbers 
#' at age by survey, year, stratum, species, and sex. "length_at_age" is a 
#' support table for the gapindex::calc_agecomp_region() function and should
#' not be used.
#' 
#' | Field Name           | Description                                                                                                                                                         |
#' |----------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' | SURVEY_DEFINITION_ID | Integer number identifier corresponding to survey region. See   gapindex::survey_ids for a list of relevant survey regions.                                         |
#' | SURVEY               | Survey region.                                                                                                                                                      |
#' | STRATUM              | Stratum ID. STRATUM = 0 indicates an experimental tow.                                                                                                              |
#' | YEAR                 | Year the survey was conducted in.                                                                                                                                   |
#' | SPECIES_CODE         | Taxon code. [See the code book for the full list.](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
#' | SEX                  | Sex of a specimen where "1" = "Male", "2" =   "Female", "3" = Unsexed.                                                                                              |
#' | AGE                  | Age (years).                                                                                                                                                        |
#' | POPULATION_COUNT     | Total number of individuals.                                                                                                                                        |
#' | LENGTH_MM_MEAN       | Estimated mean length-at-age (mm) weighted by numbers-at-length.                                                                                                    |
#' | LENGTH_MM_SD         | Standard deviation associated with the estimated mean length-at-age.                                                                                                |
#' 
#' @export
#' 

calc_agecomp_stratum <- function(racebase_tables = NULL,
                                 alk = NULL,
                                 size_comp = NULL) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Merge age distribution proportions and population numbers
  ## 
  ## Merge the age distribution proportions in `p_yklm` to the 
  ## numbers at length information in `size_comp` using 
  ## "SURVEY", "YEAR", "SPECIES_CODE", "SEX", and "LENGTH_MM" 
  ## as a composite key. all.x = TRUE will include sizes classes that 
  ## do not have observed age information. These AGES are coded as -9. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  age_comp <- merge(x = size_comp,
                    y = alk,
                    by = c("SURVEY", "YEAR", "SPECIES_CODE", 
                           "SEX", "LENGTH_MM"),
                    all.x = TRUE)
  age_comp$AGE[is.na(age_comp$AGE)] <- -9
  age_comp$AGE_FRAC[is.na(age_comp$AGE_FRAC)] <- 1
  
  ## Calculate numbers at age as the product of the age_frac and the numbers 
  ## at length
  age_comp$AGEPOP <- age_comp$AGE_FRAC * age_comp$POPULATION_COUNT
  
  count_length_age <- age_comp
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  Calculate mean and sd length at age:
  ##  Calculate the weighted mean and sd of length at age using column 
  ## "AGEPOP" as the weight. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  mean_length_at_age <- data.frame()
  
  for (ispp in sort(unique(alk$SPECIES_CODE))){
    
    mean_length_at_age <- 
      rbind(mean_length_at_age,
            do.call(
              what = rbind,
              args = lapply(
                X = split(x = subset(x = age_comp, 
                                     subset = LENGTH_MM > 0 & 
                                       SPECIES_CODE == ispp),
                          f = with(subset(x = age_comp, 
                                          subset = LENGTH_MM > 0 & 
                                            SPECIES_CODE == ispp),
                                   list(SURVEY, YEAR, 
                                        STRATUM, SEX, AGE))),
                ## Within each sublist of split_df, calculate the
                ## weighted mean and std.dev of length
                FUN = function(df) {
                  
                  ## temporary output df
                  output_df <- data.frame()
                  
                  if (nrow(x = df) > 0) {
                    ## record the combo of "SURVEY", "YEAR",
                    ## "SPECIES_CODE", "SEX", and "AGE" of the sublist.
                    output_df[1, c("SURVEY", "YEAR",
                                   "STRATUM", "SEX", "AGE")] <-
                      unique(subset(x = df,
                                    select = c(SURVEY, YEAR,
                                               STRATUM, SEX, AGE)))
                    
                    ## weighted mean length
                    mean_length <- weighted.mean(x = df$LENGTH_MM,
                                                 w = df$AGEPOP)
                    
                    ## weighted std.dev length
                    sd_length <- sqrt(sum(df$AGEPOP/sum(df$AGEPOP) *
                                            (df$LENGTH_MM - mean_length)^2))
                    
                    ## append `mean_length` and `sd_length` to `output_df`
                    output_df[, c("LENGTH_MM_MEAN", "LENGTH_MM_SD")] <-
                      round(x = c(mean_length, sd_length),
                            digits = 2)
                    
                    output_df$SPECIES_CODE = ispp
                  }
                  
                  return(output_df)
                  
                }))
      )
    
    
    
  }
  
  rownames(mean_length_at_age) <- NULL
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Total numbers by "SURVEY", "YEAR", "STRATUM", "SPECIES_CODE", 
  ##   "SEX", AND "AGE", aggregating over length classes.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  age_comp <- stats::aggregate(AGEPOP ~ SURVEY + YEAR + STRATUM + 
                                 SPECIES_CODE + SEX + AGE,
                               data = age_comp,
                               FUN = function(x) round(x = sum(x)))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Merge the "LENGTH_MM_MEAN" and "LENGTH_MM_SD" columns from 
  ##   `mean_length_at_age` and `mean_length_at_age_neg9` into age_comp 
  ##   using "SURVEY", "YEAR", "STRATUM", "SPECIES_CODE", "SEX", "AGE" 
  ##   as a composite key.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  age_comp <- merge(x = age_comp,
                    y = subset(x = mean_length_at_age,
                               select = c(SURVEY, YEAR, STRATUM, 
                                          SPECIES_CODE, SEX, AGE, 
                                          LENGTH_MM_MEAN, LENGTH_MM_SD)),
                    by = c("SURVEY", "YEAR", "STRATUM", 
                           "SPECIES_CODE", "SEX", "AGE"))
  
  ## Merge "SURVEY_DEFINITION_ID" column from `cruise`
  age_comp <- 
    merge(x = age_comp,
          y = unique(x = subset(x = racebase_tables$cruise,
                                select = c(SURVEY, YEAR, 
                                           SURVEY_DEFINITION_ID)),
                     by = c("SURVEY", "YEAR")))
  
  ## Rename AGEPOP column as POPULATION_COUNT
  names(age_comp)[names(age_comp) == "AGEPOP"] <- "POPULATION_COUNT" 
  
  ## Sort age_comp
  age_comp <- age_comp[with(age_comp, order(SURVEY, STRATUM, YEAR,
                                            SEX, AGE)), ]
  
  return(list(age_comp = subset(x = age_comp,
                                select = c(SURVEY, SURVEY_DEFINITION_ID, 
                                           STRATUM, YEAR, SPECIES_CODE, 
                                           SEX, AGE, POPULATION_COUNT, 
                                           LENGTH_MM_MEAN, LENGTH_MM_SD)),
              length_at_age = count_length_age))
}
