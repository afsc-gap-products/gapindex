#' Calculate stratum-level age composition and mean/std.dev length at age 
#' 
#' @param racebase_tables data object created from `gapindex::get_data()`
#' @param size_comp a dataframe of stratum abundances, result 
#'                  object from either `calc_sizecomp_aigoa_stratum()`
#'                  or `calc_sizecomp_bs_stratum()` depending on the region.
#' 
#' @export
#' 

# racebase_tables = cod_data
# size_comp = size_comps

calc_agecomp_stratum <- function(racebase_tables = NULL,
                                 size_comp = NULL) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Extract haul, cruise and specimen data from `racebase_tables`
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  haul <- racebase_tables$haul
  cruise <- racebase_tables$cruise
  
  ## Remove unaged otoliths (records with NA ages) from `specimen` df
  specimen <- subset(x = racebase_tables$specimen,
                     subset = !is.na(x = AGE) )
  
  ## Make sure lengths are rounded to the nearest 10 mm. Fish are measured to 
  ## the nearest cm but crab are measured to the nearest mm. 
  specimen$LENGTH <- round(x = specimen$LENGTH / 10)* 10
  
  ## Merge "SURVEY" column from the cruise information to `specimen` 
  ## using "CRUISEJOIN" as the key.
  specimen <- merge(x = specimen,
                    y = cruise[, c("CRUISEJOIN", "SURVEY", "YEAR")],
                    by = "CRUISEJOIN")
  
  ## Merge "YEAR" and "STRATUM" columns from `haul` to `specimen`, using
  ## "HAULJOIN" as a key. all.x = TRUE so that specimen from non-standard hauls
  ## are not included in the standard haul argument. These non-standard hauls
  ## will not have YEAR information, pull the "YEAR" from the cruise value.
  specimen <- merge(x = specimen, 
                    y = haul[, c("HAULJOIN", "STRATUM", "STATIONID")],
                    by = "HAULJOIN",
                    all.x = T)                  
  
  specimen$YEAR <- ifelse(test = is.na(specimen$YEAR), 
                          no = specimen$YEAR,
                          yes = as.numeric(substr(x = specimen$CRUISE, 
                                                  start = 1, stop = 4)))
  
  
  ## TEST -- delete ---
  ## Remove specimen with no STRATUM data IN 2018, THESE ARE NBS HAULS?
  specimen <- subset(x = specimen, 
                     subset = !is.na(STRATUM))
  
  ## Renamve "LENGTH" column add a "FREQ" column of 1s
  names(specimen)[names(specimen) == "LENGTH"] <- "LENGTH_MM"
  specimen$FREQ <- 1
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Narrow `size_comp` so each record refers to a sex/length combo
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # size_comp <- rbind(data.frame(size_comp[, c("SURVEY", "YEAR", "STRATUM", 
  #                                             'SPECIES_CODE', "LENGTH_MM")],
  #                               SEX = 1, 
  #                               sizepop = size_comp$MALES),
  #                    data.frame(size_comp[, c("SURVEY", "YEAR", "STRATUM", 
  #                                             'SPECIES_CODE', "LENGTH_MM")],
  #                               SEX = 2, 
  #                               sizepop = size_comp$FEMALES),
  #                    data.frame(size_comp[, c("SURVEY", "YEAR", "STRATUM", 
  #                                             'SPECIES_CODE', "LENGTH_MM")],
  #                               SEX = 3,
  #                               sizepop = size_comp$UNSEXED)
  # )
  ## Remove zero records
  # size_comp <- subset(x = size_comp, subset = sizepop > 0)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Aggregate specimen information: s_yklm will be the total number 
  ##   of age-specimens collected in year-y, species-k, length-l, sex-m. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  s_yklm <- rbind(
    ## Aggregate FEMALES and MALES
    stats::aggregate(
      FREQ ~ SURVEY + YEAR + SPECIES_CODE + SEX + LENGTH_MM + AGE,
      data = specimen,
      FUN = sum),
    
    ## For UNSEXED, we pool MALES and FEMALES AND UNSEXED TOGETHER
    cbind(
      stats::aggregate(FREQ ~ SURVEY + YEAR + SPECIES_CODE + LENGTH_MM + AGE,
                       data = specimen,
                       FUN = sum), 
      SEX = 3)[, c("SURVEY", "YEAR", "SPECIES_CODE", "SEX", 
                   "LENGTH_MM", "AGE", "FREQ")] 
  )
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Calculate distribution of age proportions
  ##   Split `s_yklm` by survey, year, species, sex, and length 
  ##   and then calculate the proportion of distribution of observed ages 
  ##   within each split (`p_yklm`, proportion of age for year-y, species-k, 
  ##   length-l, and sex-m). 
  ##   Then, rbind the splitted lists back together into one df.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p_yklm <- 
    do.call(what = rbind, 
            args = lapply(X = split(x = s_yklm, 
                                    f = list(s_yklm$SURVEY,
                                             s_yklm$YEAR, 
                                             s_yklm$SPECIES_CODE, 
                                             s_yklm$SEX,
                                             s_yklm$LENGTH_MM)),
                          FUN = function(df) {
                            df$age_frac <- df$FREQ / sum(df$FREQ)
                            return(df)
                          }))
  
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
                    y = p_yklm,
                    by = c("SURVEY", "YEAR", "SPECIES_CODE", 
                           "SEX", "LENGTH_MM"),
                    all.x = TRUE)
  age_comp$AGE[is.na(age_comp$AGE)] <- -9
  age_comp$age_frac[is.na(age_comp$age_frac)] <- 1
  
  ## Calculate numbers at age as the product of the age_frac and the numbers 
  ## at length
  age_comp$AGEPOP <- age_comp$age_frac * age_comp$POPULATION_COUNT
  
  count_length_age <- age_comp
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  Calculate mean and sd length at age:
  ##  Calculate the weighted mean and sd of length at age using column 
  ## "AGEPOP" as the weight. Calculations are done separately for lengths with
  ## associated ages and lengths without associated ages (AGE = -9)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  mean_length_at_age <- unique(x = subset(x = age_comp, 
                                          subset = AGE > 0,
                                          select = c(SURVEY, YEAR, 
                                                     SPECIES_CODE, 
                                                     STRATUM, SEX, AGE)))
  
  for (irow in 1:nrow(x = mean_length_at_age)) {
    mean_length_at_age[irow, c("LENGTH_MM_MEAN", "LENGTH_MM_SD")] <-
      with(subset(x = age_comp,
                  subset = SURVEY == mean_length_at_age$SURVEY[irow] & 
                    YEAR == mean_length_at_age$YEAR[irow] & 
                    SPECIES_CODE == mean_length_at_age$SPECIES_CODE[irow] &  
                    STRATUM == mean_length_at_age$STRATUM[irow] & 
                    SEX == mean_length_at_age$SEX[irow] & 
                    AGE == mean_length_at_age$AGE[irow] ), 
           {
             mean_length <- weighted.mean(x = LENGTH_MM, 
                                          w = AGEPOP)
             
             ## weighted std.dev length
             sd_length <- sqrt(sum(AGEPOP/sum(AGEPOP) * (LENGTH_MM - mean_length)^2))
             c(mean_length, sd_length)
           }
      )
  }
  
  # split_df <- split(x = subset(x = age_comp, 
  #                              subset = AGE > 0),
  #                   f = with(subset(x = age_comp, 
  #                                   subset = AGE > 0), 
  #                            list(SURVEY, YEAR, SPECIES_CODE, 
  #                                 STRATUM, SEX, AGE)))
  # 
  # mean_length_at_age <- 
  #   do.call(
  #     what = rbind,
  #     args = lapply(
  #       X = split_df,
  #       ## Within each sublist of split_df, calculate the 
  #       ## weighted mean and std.dev of length
  #       FUN = function(df) {
  #         
  #         ## temporary output df
  #         output_df <- data.frame()
  #         
  #         if (nrow(x = df) > 0) {
  #           ## record the combo of "SURVEY", "YEAR", 
  #           ## "SPECIES_CODE", "SEX", and "AGE" of the sublist.   
  #           output_df[1, c("SURVEY", "YEAR", "SPECIES_CODE", 
  #                          "STRATUM", "SEX", "AGE")] <- 
  #             unique(subset(x = df, 
  #                           select = c(SURVEY, YEAR, SPECIES_CODE, 
  #                                      STRATUM, SEX, AGE)))
  #           
  #           ## weighted mean length
  #           mean_length <- weighted.mean(x = df$LENGTH_MM, 
  #                                        w = df$AGEPOP)
  #           
  #           ## weighted std.dev length
  #           sd_length <- sqrt(sum(df$AGEPOP/sum(df$AGEPOP) * 
  #                                   (df$LENGTH_MM - mean_length)^2))
  #           
  #           ## append `mean_length` and `sd_length` to `output_df` 
  #           output_df[, c("LENGTH_MM_MEAN", "LENGTH_MM_SD")] <-
  #             round(x = c(mean_length, sd_length), 
  #                   digits = 2)
  #         } 
  #         
  #         return(output_df)
  #         
  #       }))
  # rownames(mean_length_at_age) <- NULL
  
  mean_length_at_age_neg9 <- unique(x = subset(x = age_comp, 
                                          subset = AGE == -9,
                                          select = c(SURVEY, YEAR, 
                                                     SPECIES_CODE, 
                                                     STRATUM, SEX, AGE)))
  
  for (irow in 1:nrow(x = mean_length_at_age_neg9)) {
    mean_length_at_age_neg9[irow, c("LENGTH_MM_MEAN", "LENGTH_MM_SD")] <-
      with(subset(x = age_comp,
                  subset = SURVEY == mean_length_at_age_neg9$SURVEY[irow] & 
                    YEAR == mean_length_at_age_neg9$YEAR[irow] & 
                    SPECIES_CODE == mean_length_at_age_neg9$SPECIES_CODE[irow] &  
                    STRATUM == mean_length_at_age_neg9$STRATUM[irow] & 
                    SEX == mean_length_at_age_neg9$SEX[irow] & 
                    AGE == mean_length_at_age_neg9$AGE[irow] ), 
           {
             mean_length <- weighted.mean(x = LENGTH_MM, 
                                          w = AGEPOP)
             
             ## weighted std.dev length
             sd_length <- sqrt(sum(AGEPOP/sum(AGEPOP) * (LENGTH_MM - mean_length)^2))
             c(mean_length, sd_length)
           }
      )
  }
  
  # ## Calculate mean and sd of length @ age for age -9 category
  # split_df <- split(x = subset(x = age_comp, subset = AGE == -9),
  #                   f = with(subset(x = age_comp, subset = AGE == -9), 
  #                            list(SURVEY, YEAR, SPECIES_CODE, 
  #                                 STRATUM, SEX)))
  # 
  # mean_length_at_age_neg9 <- 
  #   do.call(
  #     what = rbind,
  #     args = lapply(X = split_df,
  #                   FUN = function(df) {
  #                     
  #                     ## temporary output df
  #                     output_df <- data.frame()
  #                     
  #                     if (nrow(x = df) > 0) {
  #                       
  #                       ## record the combo of "SURVEY", "YEAR", 
  #                       ## "SPECIES_CODE", "SEX", and "AGE" of the split df. 
  #                       output_df[1, c("SURVEY", "YEAR", "SPECIES_CODE", 
  #                                      "STRATUM", "SEX")] <- 
  #                         unique(subset(x = df, 
  #                                       select = c(SURVEY, YEAR, SPECIES_CODE, 
  #                                                  STRATUM, SEX)))
  #                       output_df$AGE <- -9
  #                       
  #                       ## weighted mean length
  #                       mean_length <- weighted.mean(x = df$LENGTH_MM, 
  #                                                    w = df$AGEPOP)
  #                       
  #                       ## weighted std.dev length
  #                       sd_length <- sqrt(sum(df$AGEPOP/sum(df$AGEPOP) * 
  #                                               (df$LENGTH_MM - mean_length)^2))
  #                       
  #                       ## append `mean_length` and `sd_length` to `output_df` 
  #                       output_df[1, c("LENGTH_MM_MEAN", "LENGTH_MM_SD")] <-
  #                         round(x = c(mean_length, sd_length), digits = 2)
  #                       
  #                     }
  #                     return(output_df)
  #                   })
  #   )
  # rownames(mean_length_at_age_neg9) <- NULL
  
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
                    y = subset(x = rbind(mean_length_at_age, 
                                         mean_length_at_age_neg9),
                               select = c(SURVEY, YEAR, STRATUM, 
                                          SPECIES_CODE, SEX, AGE, 
                                          LENGTH_MM_MEAN, LENGTH_MM_SD)),
                    by = c("SURVEY", "YEAR", "STRATUM", 
                           "SPECIES_CODE", "SEX", "AGE"))
  
  ## Merge "SURVEY_DEFINITION_ID" column from `cruise`
  age_comp <- merge(x = age_comp,
                    y = unique(cruise[, c("SURVEY", "YEAR", 
                                          "SURVEY_DEFINITION_ID")]),
                    by = c("SURVEY", "YEAR"))
  
  ## Sort age_comp
  age_comp <- age_comp[with(age_comp, order(SURVEY, STRATUM, YEAR,
                                            SEX, AGE)), ]
  
  return(list(age_comp = subset(x = age_comp,
                                select = c(SURVEY, SURVEY_DEFINITION_ID, 
                                           STRATUM, YEAR, SPECIES_CODE, 
                                           SEX, AGE, AGEPOP, 
                                           LENGTH_MM_MEAN, LENGTH_MM_SD)),
              length_at_age = count_length_age))
}
