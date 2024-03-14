#' Calculate age-length key (alk)
#' 
#' @description Calculates empircal probability of age at length from collected otolith data. 
#'            
#' @param racebase_tables data object created from `gapindex::get_data()`.
#' @param unsex string, option to determine how unsexed individuals are 
#'              treated. Option "all" means that unsexed alks are determined 
#'              by combining all sexes (males, females, and unsexed) and is 
#'              the option used for standard design-based composition 
#'              production. Option "unsex" means that unsexed alk are only
#'              determined by unsexed individuals and is the option used for
#'              creating data inputs for model-based age composition indices.
#'              Defaults to "unsex"
#' @param global boolean. Should missing length bins be filled by using an
#'               alk consisting of all years? Defaults to TRUE. 
#' 
#' @return dataframe of probabilities ("AGE_FRAC") of ages ("AGE") by length 
#'         (LENGTH_MM) for a given survey ("SURVEY"), year ("YEAR"), species 
#'         (SPECIES_CODE), and sex (SEX). 
#' 
#' @export
#' 

calc_alk <- function(racebase_tables = NULL,
                     unsex = c("all", "unsex")[2], 
                     global = TRUE) {
  
  ## Error Check
  if (is.null(x = racebase_tables))
    stop("Must provide argument `racebase_tables` a named list from 
         gapindex::get_data().")
  if (is.null(x = racebase_tables$size)) 
    stop("racebase_tables$size must not be NULL. Either the set of taxa does 
         not have size information or rerun gapindex::get_data() with 
         argument pull_lengths = TRUE")
  if (is.null(x = racebase_tables$specimen)) 
    stop("racebase_tables$specimen must not be NULL. Either the set of taxa 
         does not have age information or rerun gapindex::get_data() with 
         argument pull_lengths = TRUE")
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Extract haul, cruise and specimen data from `racebase_tables`
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  haul <- racebase_tables$haul
  cruise <- racebase_tables$cruise
  size <- racebase_tables$size
  specimen <- racebase_tables$specimen
  
  ## Remove unaged otoliths (records with NA ages) from `specimen` df
  # specimen <- subset(x = racebase_tables$specimen,
  # subset = !is.na(x = AGE) )
  
  ## Make sure lengths are rounded to the nearest 10 mm. Fish are measured to 
  ## the nearest cm but crab are measured to the nearest mm. 
  specimen$LENGTH <- round(x = specimen$LENGTH / 10)* 10
  
  ## Merge "SURVEY" column from the cruise information to `specimen` and `size`
  ## using "CRUISEJOIN" as the key.
  specimen <- cruise[, c("CRUISEJOIN", "SURVEY", "YEAR")][specimen,
                                                          on = "CRUISEJOIN"]
  
  # specimen <- merge(x = specimen,
  #                   y = cruise[, c("CRUISEJOIN", "SURVEY", "YEAR")],
  #                   by = "CRUISEJOIN")
  
  
  size <- cruise[, c("CRUISEJOIN", "SURVEY", "YEAR")][size,
                                                      on = "CRUISEJOIN"]
  
  # size <- merge(x = size,
  #               y = cruise[, c("CRUISEJOIN", "SURVEY", "YEAR")],
  #               by = "CRUISEJOIN")
  
  ## Merge "YEAR" and "STRATUM" columns from `haul` to `specimen`, using
  ## "HAULJOIN" as a key. all.x = TRUE so that specimen from non-standard hauls
  ## are not included in the standard haul argument. These non-standard hauls
  ## will not have YEAR information, pull the "YEAR" from the cruise value.
  specimen <- 
    haul[, c("HAULJOIN", "STRATUM", "STATIONID")][
      specimen, 
      on = "HAULJOIN"]
  
  # specimen <- merge(x = specimen, 
  #                   y = haul[, c("HAULJOIN", "STRATUM", "STATIONID")],
  #                   by = "HAULJOIN",
  #                   all.x = T)                  
  # 
  # specimen$YEAR <- ifelse(test = is.na(specimen$YEAR), 
  #                         no = specimen$YEAR,
  #                         yes = as.numeric(substr(x = specimen$CRUISE, 
  #                                                 start = 1, stop = 4)))
  
  ## Remove specimen without stratum information
  specimen <- specimen[!is.na(x = STRATUM)] 
  
  ## Renamve "LENGTH" column add a "FREQ" column of 1s for tabulations
  names(specimen)[names(specimen) == "LENGTH"] <- "LENGTH_MM"
  specimen$FREQ <- 1
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Aggregate specimen information: s_yklm will be the total number 
  ##   of age-specimens collected in year-y, species-k, length-l, sex-m. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (unsex == "unsex") 
    s_yklm <- specimen[,
                       .("FREQ" = sum(FREQ)),
                       by = c("SURVEY", "YEAR", "SPECIES_CODE", 
                              "SEX", "LENGTH_MM", "AGE")]
  
  # s_yklm <- stats::aggregate(
  #   FREQ ~ SURVEY + YEAR + SPECIES_CODE + SEX + LENGTH_MM + AGE,
  #   data = specimen,
  #   FUN = sum)
  
  if (unsex == "all") 
    s_yklm <- rbind(
      specimen[SEX %in% 1:2,
               .("FREQ" = sum(FREQ)),
               by = c("SEX", "SURVEY", "YEAR", "SPECIES_CODE", 
                      "LENGTH_MM", "AGE")],
      cbind(SEX = 3,
            specimen[,
                     .("FREQ" = sum(FREQ)),
                     by = c("SURVEY", "YEAR", "SPECIES_CODE", 
                            "LENGTH_MM", "AGE")])
    )
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Calculate distribution of age proportions for a given length, `p_yklm`.
  ##   This is the non-global age-length key.
  ##   
  ##   Split `s_yklm` by survey, year, species, sex, and length 
  ##   and then calculate the proportion of distribution of observed ages 
  ##   within each split (`p_yklm`, proportion of age for year-y, species-k, 
  ##   length-l, and sex-m). 
  ##   Then, rbind the splitted lists back together into one df.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p_yklm <-
    s_yklm[, 
           (function(df) {
             df$AGE_FRAC <- df$FREQ / sum(df$FREQ)
             return(df[, -"FREQ"]) 
           })(.SD), 
           by = c("SURVEY", "YEAR", "SPECIES_CODE", "SEX", "LENGTH_MM")]
  
  if (global) {
    ## To figure out which length-bins we need to fill in, we tabulate
    ## every length bin encountered in the length subsample (`size`) for a
    ## given survey, year, species, and sex. 
    every_combo_of_lengths <- 
      data.table::CJ(SURVEY = sort(x = unique(x = size$SURVEY)),
                     YEAR = sort(x = unique(x = size$YEAR)),
                     SPECIES_CODE = sort(x = unique(x = size$SPECIES_CODE)),
                     SEX = sort(x = unique(x = size$SEX)),
                     LENGTH_MM = seq(from = min(size$LENGTH, na.rm = TRUE),
                                     to = max(size$LENGTH, na.rm = TRUE),
                                     by = 10),
                     AGE = seq(from = min(specimen$AGE, na.rm = TRUE),
                               to = max(specimen$AGE, na.rm = TRUE),
                               by = 1))
    
    p_yklm <- p_yklm[every_combo_of_lengths,
                     on = c("SURVEY", "YEAR", "SPECIES_CODE", 
                            "SEX", "LENGTH_MM", "AGE"), ]
    
    missing_lengths <-
      p_yklm[,
             .("NUM_AGES" = sum(AGE_FRAC, na.rm = TRUE)),
             by = c("SURVEY", "YEAR", "SPECIES_CODE", "SEX", "LENGTH_MM") ][
               NUM_AGES == 0, -"NUM_AGES"
             ]
    
    # p_yklm <- merge(x = every_combo_of_lengths,
    #                 y = p_yklm,
    #                 all.x = TRUE,
    #                 by = c("SURVEY", "YEAR", "SPECIES_CODE",
    #                        "SEX", "LENGTH_MM", "AGE"))
    # 
    # p_yklm[,
    #        (function(df){
    #          if (sum(df$AGE_FRAC, na.rm = TRUE) == 0) {
    #            return(data.table::data.table(
    #              SURVEY = unique(df$SURVEY),
    #              YEAR =  unique(df$YEAR),
    #              SPECIES_CODE = unique(df$SPECIES_CODE),
    #              SEX = unique(df$SEX),
    #              LENGTH_MM = unique(df$LENGTH_MM))) 
    #          } 
    #        })(.SD),
    #        by = c("SURVEY", "YEAR", "SPECIES_CODE", "SEX", "LENGTH_MM")]
    # 
    # missing_lengths <- 
    #   do.call(what = rbind, 
    #           args = lapply(X = split(x = p_yklm, 
    #                                   f = with(p_yklm, list(SURVEY,
    #                                                         YEAR, 
    #                                                         SPECIES_CODE, 
    #                                                         SEX,
    #                                                         LENGTH_MM))),
    #                         FUN = function(df) {
    #                           if (sum(df$AGE_FRAC, na.rm = TRUE) == 0)
    #                             return(data.frame(
    #                               SURVEY = unique(df$SURVEY),
    #                               YEAR =  unique(df$YEAR),
    #                               SPECIES_CODE = unique(df$SPECIES_CODE),
    #                               SEX = unique(df$SEX),
    #                               LENGTH_MM = unique(df$LENGTH_MM))) 
    #                         }
    #           )
    #   )
    
    ## Aggregate specimen information over years to calculate a global ALK,
    ## i.e., an ALK that is aggregated over years so we can fill in years
    ## where a length-bin for a sex was not encountered.
    s_klm <- 
      specimen[,
               .("FREQ" = sum(FREQ)),
               by = c("SURVEY", "SPECIES_CODE", "SEX", "LENGTH_MM", "AGE")]
    
    # s_klm <- stats::aggregate(
    #   FREQ ~ SURVEY + SPECIES_CODE + SEX + LENGTH_MM + AGE,
    #   data = specimen,
    #   FUN = sum)
    
    p_klm <-
      s_klm[, 
            (function(df) {
              df$AGE_FRAC <- df$FREQ / sum(df$FREQ)
              return(df[, -"FREQ"]) 
            })(.SD), 
            by = c("SURVEY", "SPECIES_CODE", "SEX", "LENGTH_MM")]
    
    # p_klm <- 
    #   do.call(what = rbind, 
    #           args = lapply(X = split(x = s_klm, 
    #                                   f = list(s_klm$SURVEY,
    #                                            s_klm$SPECIES_CODE, 
    #                                            s_klm$SEX,
    #                                            s_klm$LENGTH_MM)),
    #                         FUN = function(df) {
    #                           df$AGE_FRAC <- df$FREQ / sum(df$FREQ)
    #                           return(df)
    #                         }))
    
    ## For each record in `missing_lengths`, merge all the age probabilities
    ## from `p_klm` for that length bin. 
    filled_in_lengths <- p_klm[missing_lengths,
                               on = c("SURVEY", "SPECIES_CODE", 
                                      "SEX", "LENGTH_MM"),
                               allow.cartesian = T]
    
    # filled_in_lengths <- 
    #   merge(x = missing_lengths,
    #         y = p_klm,
    #         all.x = TRUE,
    #         by = c("SURVEY", "SPECIES_CODE", "SEX", "LENGTH_MM"))
    
    ## Append the globally-filled lengths with the the non-global `p_yklm` alk
    ## to get a now global alk. 
    p_yklm <- rbind(p_yklm[!is.na(x = AGE_FRAC)],
                    filled_in_lengths[!is.na(x = AGE_FRAC),
                                      .(SURVEY, YEAR, SPECIES_CODE, 
                                        SEX, LENGTH_MM, AGE, AGE_FRAC)])
    
    p_yklm <- p_yklm[every_combo_of_lengths,
                     on = c("SURVEY", "YEAR", "SPECIES_CODE", 
                            "SEX", "LENGTH_MM", "AGE")]
    
    # p_yklm <- merge(x = every_combo_of_lengths,
    #                 y = p_yklm,
    #                 all.x = TRUE,
    #                 by = c("SURVEY", "YEAR", "SPECIES_CODE", 
    #                        "SEX", "LENGTH_MM", "AGE"))
  }
  
  p_yklm$AGE_FRAC[is.na(x = p_yklm$AGE_FRAC)] <- 0
  return(unique(p_yklm))
}

