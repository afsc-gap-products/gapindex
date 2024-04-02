#' Calculate stratum-level age composition and mean/std.dev length at age 
#' 
#' @param racebase_tables `r lifecycle::badge("deprecated")` Use the 
#'                        `gapdata` argument instead. 
#' @param gapdata data object created from `gapindex::get_data()`
#' @param alk age-length key (dataframe) created from `gapindex::calc_alk()`
#' @param size_comp `r lifecycle::badge("deprecated")` Use the 
#'                  `sizecomp_stratum` argument instead. 
#' @param sizecomp_stratum a dataframe of size compositions created from 
#'                         `gapindex::calc_sizecomp_stratum()`.
#'
#' @return A named list with two elements. "age_comp" is a dataframe of numbers 
#'         at age by survey, year, stratum, species, and sex. A table of column
#'         name descriptions is coming soon."length_at_age" is a support table
#'         for the gapindex::calc_agecomp_region() function and should not be
#'         used. 
#' 
#' @export
#' 

calc_agecomp_stratum <- function(gapdata = NULL,
                                 racebase_tables = lifecycle::deprecated(),
                                 alk = NULL,
                                 sizecomp_stratum = NULL,
                                 size_comp = lifecycle::deprecated()) {
  
  ## Input Check
  if (lifecycle::is_present(racebase_tables)) {
    lifecycle::deprecate_warn("2.2.0", 
                              "calc_agecomp_stratum(racebase_tables)", 
                              "calc_agecomp_stratum(gapdata)")
    gapdata <- racebase_tables
  }
  if (lifecycle::is_present(size_comp)) {
    lifecycle::deprecate_warn("2.2.0", 
                              "calc_agecomp_stratum(size_comp)", 
                              "calc_agecomp_stratum(sizecomp_stratum)")
    sizecomp_stratum <- size_comp
  }
  
  ## Error Check on function arguments
  for (iarg in c("gapdata", "alk", "sizecomp_stratum"))
    if (is.null(x = get(x = iarg)))
      stop(paste0("Must provide argument `", iarg, "`. ",
                  "See ?gapindex::calc_agecomp_stratum for more information"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Merge age distribution proportions and population numbers
  ## 
  ## Merge the age distribution proportions in `p_yklm` to the 
  ## numbers at length information in `size_comp` using 
  ## "SURVEY", "YEAR", "SPECIES_CODE", "SEX", and "LENGTH_MM" 
  ## as a composite key. all.x = TRUE will include sizes classes that 
  ## do not have observed age information. These AGES are coded as -9. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  age_comp <-
    merge(x = subset(x = size_comp,
                     subset = SPECIES_CODE %in% unique(x = alk$SPECIES_CODE)),
          y = alk,
          by = c("SURVEY", "YEAR", "SPECIES_CODE", "SEX", "LENGTH_MM"),
          all.x = TRUE)
  age_comp$AGE[is.na(x = age_comp$AGE)] <- -9
  age_comp$AGE_FRAC[is.na(x = age_comp$AGE_FRAC)] <- 1
  
  ## Calculate numbers at age as the product of the age_frac and the numbers 
  ## at length
  age_comp$AGEPOP <- age_comp$AGE_FRAC * age_comp$POPULATION_COUNT
  
  count_length_age <- age_comp
  count_length_age$AGE[count_length_age$LENGTH_MM < 0] <- -99
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  Calculate mean and sd length at age:
  ##  Calculate the weighted mean and sd of length at age using column 
  ## "AGEPOP" as the weight. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mean_length_at_age <- 
    age_comp[LENGTH_MM > 0,
             .(LENGTH_MM_MEAN = round(x = stats::weighted.mean(x = LENGTH_MM, 
                                                               w = AGEPOP), 
                                      digits = 2),
               LENGTH_MM_SD = round(x = sqrt(
                 x = sum(AGEPOP/sum(AGEPOP) * 
                           (LENGTH_MM - stats::weighted.mean(x = LENGTH_MM, 
                                                             w = AGEPOP))^2)
               ), 
               digits = 2) ),
             by = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR", "STRATUM", 
                    "SPECIES_CODE", "SEX", "AGE")]
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Total numbers by "SURVEY", "YEAR", "STRATUM", "SPECIES_CODE", 
  ##   "SEX", AND "AGE", aggregating over length classes.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  age_comp <- age_comp[,
                       .(POPULATION_COUNT = round(x = sum(AGEPOP))),
                       by = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR", 
                              "STRATUM", "SPECIES_CODE", "SEX", "AGE")]
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Merge the "LENGTH_MM_MEAN" and "LENGTH_MM_SD" columns from 
  ##   `mean_length_at_age` and `mean_length_at_age_neg9` into age_comp 
  ##   using "SURVEY", "YEAR", "STRATUM", "SPECIES_CODE", "SEX", "AGE" 
  ##   as a composite key.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  age_comp <- 
    mean_length_at_age[age_comp,
                       on = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR", 
                              "STRATUM", "SPECIES_CODE", "SEX", "AGE")]
  age_comp <- 
    age_comp[order(SURVEY_DEFINITION_ID, SURVEY, YEAR, 
                   STRATUM, SPECIES_CODE, SEX, AGE), -"SURVEY"]
  
  ## Any records with NA mean/sd length values are coded as AGE -99: 
  ## INDICATES A CASE WHERE NO LENGTHS WERE COLLECTED WITHIN A STRATUM 
  ## FOR A SPECIES/YEAR EVEN THOUGH CATCH NUMBERS EXISTED.
  age_comp$AGE[is.na(x = age_comp$LENGTH_MM_MEAN)] <- -99
  
  return(list(age_comp = age_comp[POPULATION_COUNT > 0],
              length_at_age = count_length_age))
}
