#' Calculate numbers at length for each stratum
#' 
#' @description Uses Equations 16 and 17 in Wakabayashi et al. 1985 to calculate
#'              numbers at length. To be used only for the EBS_SHELF or 
#'              NBS_SHELF regions.
#' 
#' @param racebase_tables data object created from `AFSC.GAP.DBE::get_data()`.
#' @param racebase_cpue object created from `AFSC.GAP.DBE::calc_cpue()`.
#' @param racebase_stratum_popn a dataframe of stratum abundances, result 
#'                               object from  `AFSC.GAP.DBE::calc_biomass_stratum()`.
#' 
#' @export
#' 

calc_size_stratum_BS <- function(racebase_tables = NULL,
                                 racebase_cpue = NULL,
                                 racebase_stratum_popn = NULL) {
  
  ## Extract data objects
  size <- racebase_tables$size
  cpue <- racebase_cpue
  haul <- racebase_tables$haul
  haul$YEAR <- as.numeric(format(haul$START_TIME, format = "%Y"))
  catch <- racebase_tables$catch
  catch$SPECIES_CODE <- catch$GROUP
  
  ## Attach year and stratum information to size df
  size <- merge(x = size[, c("HAULJOIN", "REGION", "SPECIES_CODE", "LENGTH",
                             "FREQUENCY", "SEX")], 
                y = haul[, c("HAULJOIN", "YEAR", "STRATUM")],
                by = "HAULJOIN")
  
  ##############################################
  ## Wakabayashi et al. 1985 Equation 16: 
  ##############################################
  ## At each station with length-frequency records, the number of samples
  ##     within each sex/length class was estimated by expanding the 
  ##     length-frequency subsample to the total catch (per area swept). 
  ##     This is S_ijklm, the estimated number of individuals of sex-m and 
  ##     length-l of species-k in station-j in stratum-i.
  ##
  ## s_ijklm is the FREQUENCY column of the size df, the number of records
  ##     of sex-m and length-l of species-k recorded at station-j in stratum-i
  ## s_ijk is the frequency of recorded indidivuals in  summed over 
  ##        sex, and length class, the number of individuals of species-k 
  ##        recorded at station-j in stratum-i summed over length and sex 
  ##        classes
  ## S_ijk is the estimated numbers per area swept from the cpue df
  ##     of species-k at station-j in stratum-i. The notation S_ijk is not 
  ##     used below, instead NUMCPUE_IND_KM2 is used. 
  ##
  ## s_ijk and NUMCPUE_IND_KM2 are merged into the size df by joining 
  ##     via the HAULJOIN value and species code so that calculations can
  ##     be vectorized easily.


  s_ijk <- stats::aggregate(
    FREQUENCY ~ YEAR + STRATUM + HAULJOIN + SPECIES_CODE,
    data = size,
    FUN = sum)
  names(s_ijk)[5] <- "s_ijk"
  
  size <- merge(x = size, 
                y = s_ijk[, c("SPECIES_CODE", "HAULJOIN", "s_ijk")],
                by = c("SPECIES_CODE", "HAULJOIN"))
  size <-  merge(x = size, 
                 by.x = c("HAULJOIN", "SPECIES_CODE"),
                 y = cpue[, c("HAULJOIN", "GROUP", "NUMCPUE_COUNT_KM2")],
                 by.y = c("HAULJOIN", "GROUP"))
  size$S_ijklm <- size$FREQUENCY / size$s_ijk * size$NUMCPUE_COUNT_KM2
  
  ##############################################
  ## Wakabayashi et al. 1985 Equation 17: 
  ##############################################
  ## The number of individuals (population) of species-k of sex-m and 
  ##      length-l in stratum-i is noted as P_iklm.
  ## 
  ## S_ijklm is summed over hauls-j, creating S_iklm and this is divided
  ##    by S_ik, which is S_ijklm summed over hauls-j, sex-m, and length-l.
  ## S_iklm / S_ik is the proportion of the stratum population, P_ik (noted
  ##    as racebase_stratum_popn$pop below) that is attributed to sex-m
  ##    and length-l for species-k in stratum-i. When multiplied by P_ik, 
  ##    we get P_iklm.
  ##
  ## P_ik (racebase_stratum_popn$pop) and S_ik are merged into the S_iklm
  ##    df via the year, stratum, and species_code values. Wakabayashi et al.
  ##    1985 assumes one year of data so there is no year index in their 
  ##    paper but this function has the capability of calculating size comps
  ##    for multiple years of data.

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
                  by.x = c("YEAR", 'STRATUM', "SPECIES_CODE"),
                  y = racebase_stratum_popn[c("YEAR", 'STRATUM', 
                                              "GROUP", "POPULATION_COUNT")],
                  by.y = c("YEAR", 'STRATUM', "GROUP"),
                  all.y = TRUE)
  
  ## For some strata in some years, lengths were not collected so the stratum
  ## population is denoted as the total population with a dummy length class
  ## of -9. A sex-class of 4 distinguishes this case, interpreted as no 
  ## individuals collected as supposed the to a sex-class of 3 which is 
  ## individuals collected but sex information not available or not sexed.
  S_iklm$LENGTH[is.na(S_iklm$LENGTH)] <- -9 ## dummy variable
  S_iklm$SEX[is.na(S_iklm$SEX)] <- 4 ## dummy variable
  S_iklm[is.na(S_iklm$S_iklm), c("S_iklm", "S_ik")] <- 1 #to ease calculation
  
  ## Population calculation
  S_iklm$NUMBER <- 
    round(x = S_iklm$POPULATION_COUNT * S_iklm$S_iklm / S_iklm$S_ik)
  
  ## Remove zero-records
  S_iklm <- subset(x = S_iklm, 
                   subset = NUMBER > 0,
                   select = -c(S_iklm, S_ik, POPULATION_COUNT))
  
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
  
  ## Order sexes to M, F, U, TOTAL. Set NAs to zero. 
  P_iklm <- subset(P_iklm, 
                   select = c(YEAR, STRATUM, SPECIES_CODE, LENGTH,   
                              MALES, FEMALES, UNSEXED))
  na_idx <- is.na(P_iklm[, c("MALES", "FEMALES", "UNSEXED")])
  P_iklm[, c("MALES", "FEMALES", "UNSEXED")][na_idx] <- 0
  
  ## Calculate total over sexes
  P_iklm$TOTAL <- 
    rowSums(x = P_iklm[, c("MALES", "FEMALES", "UNSEXED")],
            na.rm = TRUE)
  
  ## Reorder rows by size
  P_iklm <- P_iklm[order(P_iklm$YEAR, P_iklm$STRATUM, P_iklm$SPECIES_CODE,
                         P_iklm$LENGTH), ]
  
  return(P_iklm)
}
