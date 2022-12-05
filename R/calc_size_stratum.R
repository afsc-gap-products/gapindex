#' Calculate numbers at length for each stratum
#' 
#' @description Uses Equations 16 and 17 in Wakabayashi et al. 1985 to calculate
#'              numbers at length 
#' 
#' @param racebase_tables data object created from AFSC.GAP.DBE::get_data()
#' @param racebase_cpue object created from AFSC.GAP.DBE::get_cpue().
#' @param racebase_stratum_stats a dataframe of stratum abundances, result 
#'                               object from  `get_biomass_stratum()`.
#' 
#' @export
#' 

calc_size_stratum <- function(racebase_tables = NULL,
                              racebase_cpue = NULL,
                              racebase_stratum_stats = NULL) {
  
  ## Extract data objects
  size <- racebase_tables$size
  cpue <- racebase_cpue
  haul <- racebase_tables$haul
  haul$YEAR <- as.numeric(format(haul$START_TIME, format = "%Y"))
  catch <- racebase_tables$catch
  catch$SPECIES_CODE <- catch$group
  
  ## attach year and stratum information to size df
  size <- merge(x = size[, c("HAULJOIN", "REGION", "SPECIES_CODE", "LENGTH",
                             "FREQUENCY", "SEX")], 
                y = haul[, c("HAULJOIN", "YEAR", "STRATUM")],
                by = "HAULJOIN")
  
  ## Equation 16 divides the numbers per area swept of the haul over length and sex
  ## S_ijk is the estimated numbers per area swept from the cpue df
  ## s_ijklm is the FREQUENCY column of the size df
  ## s_ijk is the denominator, summing the frequency summed over 
  ##        ages, sex, and length class
  s_ijk <- stats::aggregate(
    FREQUENCY ~ YEAR + STRATUM + HAULJOIN + SPECIES_CODE,
    data = size,
    FUN = sum)
  names(s_ijk)[5] <- "s_ijk"
  
  size <- merge(x = size, 
                y = s_ijk[, c("SPECIES_CODE", "HAULJOIN", "s_ijk")],
                by = c("SPECIES_CODE", "HAULJOIN"))
  size <-  merge(x = size, 
                 y = cpue[, c("HAULJOIN", "SPECIES_CODE", "NUMCPUE_IND_KM2")],
                 by = c("HAULJOIN", "SPECIES_CODE"))
  size$S_ijklm <- size$FREQUENCY / size$s_ijk * size$NUMCPUE_IND_KM2
  
  ## Equation 17: divide the estimated population size in a stratum among
  ##              length and sex
  ## S_ik is the denominator, summing the S_ijklm calculated in Equation 16 
  ##      across hauls, sex, and length
  ## S_iklm is the numerator, summing the S_ijklm calculated in Equation 16
  ##      across hauls
  S_ik <- stats::aggregate(S_ijklm ~ YEAR + STRATUM + SPECIES_CODE,
                           data = size,
                           FUN = sum)
  names(S_ik)[4] <- "S_ik"
  
  S_iklm <- 
    stats::aggregate(S_ijklm ~ YEAR + STRATUM + SPECIES_CODE + LENGTH + SEX,
                     data = size,
                     FUN = sum)
  names(S_iklm)[6] <- "S_iklm" 
  
  S_iklm <- merge(x = S_iklm,
                  y = S_ik,
                  by = c("YEAR", 'STRATUM', "SPECIES_CODE"))
  
  S_iklm <- merge(x = S_iklm,
                  y = racebase_stratum_stats[c("YEAR", 'STRATUM', 
                                               "SPECIES_CODE", "pop")],
                  by = c("YEAR", 'STRATUM', "SPECIES_CODE"))
  
  S_iklm$NUMBER <- round(S_iklm$pop * S_iklm$S_iklm / S_iklm$S_ik)
  S_iklm <- subset(x = S_iklm, 
                   select = -c(S_iklm, S_ik, pop))
  
  ## Widen output tables so each sex is a column
  P_iklm <- 
    stats::reshape(data = S_iklm, 
                   v.names = "NUMBER", 
                   idvar = c("YEAR", 'STRATUM', "SPECIES_CODE", "LENGTH"),
                   timevar = "SEX", 
                   direction = "wide")
  
  names(P_iklm)[match(x = c("NUMBER.1", "NUMBER.2", "NUMBER.3"), 
                      table = names(P_iklm))] <-
    c("MALES", "FEMALES", "UNSEXED")
  
  ## Order sexes to M, F, U. Set NAs to zero. 
  P_iklm <- subset(P_iklm, 
                   select = c(YEAR, STRATUM, SPECIES_CODE, LENGTH,   
                              MALES, FEMALES, UNSEXED))
  na_idx <- is.na(P_iklm[, c("MALES", "FEMALES", "UNSEXED")])
  P_iklm[, c("MALES", "FEMALES", "UNSEXED")][na_idx] <- 0
  
  ## Calculate total over sexes
  P_iklm$TOTAL <- rowSums(P_iklm[, c("MALES", "FEMALES", "UNSEXED")])
  
  return(P_iklm)
}
