#' Calculate numbers at length for each stratum
#' 
#' @description Uses Equations 16 and 17 in Wakabayashi et al. 1985 to calculate
#'              numbers at length. To be used only for the GOA or AI regions.
#'            
#' @param racebase_tables data object created from `gapindex::get_data()`.
#' @param racebase_cpue object created from `gapindex::calc_cpue()`.
#' @param racebase_stratum_popn a dataframe of stratum abundances, result 
#'                               object from  `gapindex::calc_biomass_stratum()`
#' 
#' @export
#' 

calc_sizecomp_aigoa_stratum <- function(racebase_tables = NULL,
                                        racebase_cpue = NULL,
                                        racebase_stratum_popn = NULL) {
  
  ## Error Check
  if (is.null(racebase_tables$size)) 
    stop("racebase_tables$size must not be NULL. Either the taxon does not 
         have size information or rerun gapindex::get_data() with 
         argument pull_lengths = TRUE")
  
  ## Warning message if there are EBS data contained in racebase_tables
  if ("EBS" %in% racebase_tables$cruise$SURVEY | 
      "NBS" %in% racebase_tables$cruise$SURVEY)
    warning("EBS and/or NBS data are included in the input data. This function
       only applies the size composition to the AI/GOA. 
       Use gapindex::calc_size_stratum_BS() to calculate the size 
       composition to the EBS/NBS.")
  
  ## Subset only AI/GOA data from the cruise data. If there are no AI/GOA
  ## cruises in the dataset, send out an error to use the other size comp fn. 
  cruise <- subset(x = racebase_tables$cruise,
                   subset = SURVEY %in% c("AI", "GOA"))
  
  if (nrow(cruise) == 0){
    stop("AI or GOA cruises are not in argument racebase_tables$cruise.
         This function only applies the size composition to the AI/GOA. 
         Use gapindex::calc_size_stratum_BS() to calculate the size 
         composition to the EBS/NBS.")
  }
  
  size <- subset(x = racebase_tables$size, 
                 subset = CRUISEJOIN %in% cruise$CRUISEJOIN)
  size$SURVEY <- size$REGION
  
  haul <- subset(x = racebase_tables$haul,
                 subset = CRUISEJOIN %in% cruise$CRUISEJOIN)
  haul$YEAR <- as.numeric(format(haul$START_TIME, format = "%Y"))
  haul$SURVEY <- haul$REGION
  
  cpue <- subset(x = racebase_cpue, 
                 subset = HAULJOIN %in% haul$HAULJOIN)
  # cpue <- merge(x = cpue, 
  #               y = haul[, c("HAULJOIN", "YEAR", "STRATUM", "SURVEY")],
  #               by = "HAULJOIN")
  # catch <- subset(x = racebase_tables$catch,
  #                 subset = HAULJOIN %in% haul$HAULJOIN)
  # catch$SURVEY <- catch$REGION
  
  racebase_stratum_popn <- 
    subset(x = racebase_stratum_popn, 
           subset = SURVEY_DEFINITION_ID %in% c("AI" = 52, "GOA" = 47))
  
  ## Attach year and stratum information to size table
  size <- merge(x = size[, c("HAULJOIN", "SURVEY", "SPECIES_CODE", 
                             "LENGTH", "FREQUENCY", "SEX")],
                all.x = TRUE,
                y = haul[, c("HAULJOIN", "YEAR", "STRATUM")],
                by = "HAULJOIN")
  
  ## For each stratum, taxon, and year, sum nCPUE (numbers per area swept)
  cpue_sum <- 
    stats::aggregate(CPUE_NOKM2 ~ SPECIES_CODE + STRATUM + YEAR + SURVEY,
                     data = cpue,
                     FUN = sum)
  names(cpue_sum)[names(cpue_sum) == "CPUE_NOKM2"] <- "CPUE_SUM"
  
  ## Attach nCPUE sums to each record in the cpue table based on taxon, 
  ## stratum, and year. Normalize nCPUE by their respective nCPUE totals.
  ## Subset only hauls with a positive or non-NA nCPUE.
  cpue_ratio <- merge(x = cpue[, c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY", 
                                   "HAULJOIN", "CPUE_NOKM2")],
                      y = cpue_sum,
                      by = c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY"))
  
  cpue_ratio$CPUE_RATIO <- cpue_ratio$CPUE_NOKM2 / cpue_ratio$CPUE_SUM
  cpue_ratio <- subset(x = cpue_ratio, subset = CPUE_RATIO > 0)
  
  ## Query whether the HAULJOIN in the cpue_ratio table is in the size table,
  ## i.e., are there observed lengths associated with a positive catch?
  length_sum <- stats::aggregate( 
    FREQUENCY ~ YEAR + STRATUM + HAULJOIN + SPECIES_CODE,
    data = size,
    FUN = sum)
  names(length_sum)[names(length_sum) == "FREQUENCY"] <- "LENGTH_SUM"
  
  cpue_ratio <- merge(x = cpue_ratio[, c("SPECIES_CODE", "HAULJOIN", "STRATUM", 
                                         "YEAR", "SURVEY", "CPUE_RATIO")],
                      y = length_sum[, c("SPECIES_CODE", "HAULJOIN", 
                                         "LENGTH_SUM")],
                      all.x = TRUE,
                      by = c("SPECIES_CODE", "HAULJOIN"))
  cpue_ratio$POS_CATCH_W_LENS <- !is.na(cpue_ratio$LENGTH_SUM)
  
  ## Calculate the number of hauls within a stratum, taxon, and year where
  ## lengths were collected.
  len_hauls <- 
    stats::aggregate(POS_CATCH_W_LENS ~ SPECIES_CODE + STRATUM + YEAR + SURVEY,
                     data = cpue_ratio,
                     FUN = sum)
  
  ## For each year, stratum, haul, and species, sum number of recorded lengths
  length_sum <- stats::aggregate( 
    FREQUENCY ~ SURVEY + YEAR + STRATUM + HAULJOIN + SPECIES_CODE,
    data = size,
    FUN = sum)
  names(length_sum)[names(length_sum) == "FREQUENCY"] <- "LENGTH_SUM"
  
  ## Attach the length sums to each record in the size df by species and haul.
  ## Normalize the length frequencies by their respective totals.
  size <- merge(x = size,
                all.x = TRUE,
                y = length_sum[, c("SPECIES_CODE", "HAULJOIN", "LENGTH_SUM")],
                by = c("SPECIES_CODE", "HAULJOIN"))
  
  size$SIZE_RATIO <- size$FREQUENCY / size$LENGTH_SUM
  
  ## Attach the number of length hauls to the size df
  size  <- merge(x = size,
                 by.x = c("SPECIES_CODE",  "STRATUM", "YEAR", "SURVEY"),
                 all.x = TRUE,
                 y = len_hauls,
                 by = c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY"))
  
  ## Now we deal with hauls with a positive or non-NA reported catch but no
  ## recorded lengths. First, for each HAULJOIN in this group, grab the 
  ## lengths 
  
  lengths_by_stratum <- size[, c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY",
                                 "LENGTH", "SEX")]
  
  lengths_by_stratum <- lengths_by_stratum[!duplicated(lengths_by_stratum), ]
  
  size_wo_lens <- merge(x = subset(x = cpue_ratio, 
                                   subset = POS_CATCH_W_LENS == F,
                                   select = c(SPECIES_CODE, STRATUM, YEAR, 
                                              SURVEY, HAULJOIN)), 
                        y = lengths_by_stratum,
                        by = c("STRATUM", "SPECIES_CODE", "YEAR", "SURVEY"))
  
  ## We then impute the size ratio of these no-length-collected hauls with the 
  ## average size composition.  
  
  size_ratio_null <- aggregate(SIZE_RATIO ~ SPECIES_CODE + SEX + 
                                 LENGTH + STRATUM + YEAR + SURVEY,
                               data = size,
                               FUN = sum)
  size_ratio_null <- merge(x = size_ratio_null,
                           y = len_hauls,
                           by = c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY"))
  size_ratio_null$SIZE_RATIO <-
    size_ratio_null$SIZE_RATIO / size_ratio_null$POS_CATCH_W_LENS
  
  ## Attach the size ratio for each record in size_wo_lens based on 
  ## year, taxon, stratum, sex, and length
  size_wo_lens <- merge(x = size_wo_lens,
                        y = size_ratio_null,
                        by = c("SPECIES_CODE", "YEAR", "STRATUM", "SURVEY",
                               "SEX", "LENGTH"))
  
  ## Combine the size data (hauls with positive catch and recorded lengths)
  ## with the hauls with positive catch and no lengths that we just imputted.
  size <- rbind(subset(x = size, 
                       select = c(SPECIES_CODE, YEAR, STRATUM, SURVEY, 
                                  SEX, LENGTH,
                                  HAULJOIN, SIZE_RATIO, POS_CATCH_W_LENS)), 
                subset(x = size_wo_lens,
                       select = c(SPECIES_CODE, YEAR, STRATUM, SURVEY,
                                  SEX, LENGTH,
                                  HAULJOIN, SIZE_RATIO, POS_CATCH_W_LENS)))
  
  ## Attach cpue ratio to the size df by year, taxon, stratum, and haul
  size <- merge(x = size[, c("YEAR", "SPECIES_CODE", "STRATUM", "SURVEY", 
                             "SEX", "LENGTH", "HAULJOIN", "SIZE_RATIO")],
                y = cpue_ratio[, c("YEAR", "SPECIES_CODE", "STRATUM", 
                                   "SURVEY", "HAULJOIN","CPUE_RATIO")],
                by = c("YEAR", "SPECIES_CODE", "STRATUM", 
                       "SURVEY", "HAULJOIN"))
  
  ## Attach population estimate to the size df by year, taxon, and stratum
  size <- merge(x = size,
                y = racebase_stratum_popn[, c("YEAR", "SPECIES_CODE", 
                                              "STRATUM", "SURVEY", 
                                              "POPULATION_COUNT")],
                by = c("YEAR", "SPECIES_CODE", "STRATUM", "SURVEY"))
  
  ## Numbers at size and length is now the product of the size ratio, cpue
  ## ratio, and stratum population estimate.
  size$NUMBER <- size$SIZE_RATIO * size$CPUE_RATIO * size$POPULATION_COUNT
  
  size_comp <- stats::aggregate(NUMBER ~ SURVEY + YEAR + SPECIES_CODE + STRATUM + 
                                  SEX + LENGTH,
                                data = size,
                                FUN = function(x) round(x = sum(x)))
  
  # ## Widen the size_comp df so that each sex is a column.
  # size_comp <- stats::reshape(data = size_comp, 
  #                             v.names = "NUMBER", 
  #                             idvar = c("SURVEY", "YEAR", 'STRATUM', 
  #                                       "SPECIES_CODE", "LENGTH"),
  #                             timevar = "SEX", 
  #                             direction = "wide")
  # 
  # ## Add a column of zeros if there are no unsexed individuals (NUMBER.3)
  # if (!"NUMBER.3" %in% names(size_comp)) size_comp$NUMBER.3 <- 0
  # 
  # names(size_comp)[names(size_comp) %in% paste0("NUMBER.", 1:3)] <-
  #   c("NUMBER.1" = "MALES", 
  #     "NUMBER.2" =  "FEMALES", 
  #     "NUMBER.3" = "UNSEXED")[names(size_comp)[names(size_comp) %in% 
  #                                                paste0("NUMBER.", 1:3)]]
  # 
  # ## Order sexes to M, F, U. Set NAs to zero. 
  # size_comp <- subset(size_comp, 
  #                     select = c(SURVEY, YEAR, STRATUM, SPECIES_CODE, LENGTH,   
  #                                MALES, FEMALES, UNSEXED))
  # na_idx <- is.na(size_comp[, c("MALES", "FEMALES", "UNSEXED")])
  # size_comp[, c("MALES", "FEMALES", "UNSEXED")][na_idx] <- 0
  # 
  # ## Calculate total over sexes
  # size_comp$TOTAL <- rowSums(size_comp[, c("MALES", "FEMALES", "UNSEXED")])
  # 
  ## Add units to LENGTH column name
  names(size_comp)[names(size_comp) == "LENGTH"] <- "LENGTH_MM"
  ## Change column name "NUMBER" to "POPULATION_COUNT"
  names(size_comp)[names(size_comp) == "NUMBER"] <- "POPULATION_COUNT"
  
  # ## Add SURVEY_DEFINITION_ID to output
  size_comp <- merge(x = size_comp, y = racebase_tables$survey, by = "SURVEY")

  ## Rearrnge columns
  size_comp <- size_comp[, c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR",
                             "STRATUM", "SPECIES_CODE", "LENGTH_MM", "SEX",
                             "POPULATION_COUNT"#,
                             # "MALES", "FEMALES", "UNSEXED", "TOTAL"
                             )]

  return(size_comp[with(size_comp,
                        order(SPECIES_CODE, YEAR, SURVEY_DEFINITION_ID,
                              STRATUM, LENGTH_MM)), ])
}
