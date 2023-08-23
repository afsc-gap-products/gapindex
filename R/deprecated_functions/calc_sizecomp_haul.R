#' Calculate numerical CPUE at length by haul
#' 
#' @description Uses Equation 16 in Wakabayashi et al. 1985 to calculate
#'              numbers-catch-per-unit-effort (CPUE, Numbers/km2) at length.
#'            
#' @param racebase_tables data object created from `gapindex::get_data()`.
#' @param racebase_cpue object created from `gapindex::calc_cpue()`.
#' 
#' @return dataframe of numerical CPUE ("S_ijklm_NO_KM2") by survey ("SURVEY"), 
#' year ("YEAR"), stratum ("STRATUM"), haul ("HAULJOIN), species (SPECIES_CODE), 
#' sex (SEX), and length (LENGTH_MM). 
#' 
#' @export
#' 

calc_sizecomp_haul <- function(racebase_tables = NULL,
                               racebase_cpue = NULL) {
  
  ## Error Check
  if (is.null(x = racebase_tables$size)) 
    stop("racebase_tables$size must not be NULL. Either the taxon does not 
         have size information or rerun gapindex::get_data() with 
         argument pull_lengths = TRUE")
  
  ## Subset dataframe haul based on the CRUISEJOIN field in dataframe cruise. 
  ## Attach SURVEY and YEAR information to dataframe haul using CRUISJOIN
  ## as the key. 
  cruise <- racebase_tables$cruise
  haul <- subset(x = racebase_tables$haul,
                 subset = CRUISEJOIN %in% cruise$CRUISEJOIN)
  haul <- merge(x = haul, 
                y = cruise[, c("CRUISEJOIN", "SURVEY", "YEAR")],
                by = "CRUISEJOIN")
  
  cpue <- subset(x = racebase_cpue,
                 subset = CRUISEJOIN %in% cruise$CRUISEJOIN)
  size <- subset(x = racebase_tables$size, 
                 subset = HAULJOIN %in% haul$HAULJOIN)
  
  ## Attach YEAR, SURVEY,  and STRATUM information from dataframe `haul to 
  ## dataframe `size` using HAULJOIN as the key.
  size <-  merge(x = size[, c("HAULJOIN", "SPECIES_CODE",
                              "LENGTH", "FREQUENCY", "SEX")],
                 y = cpue[, c("SURVEY", "HAULJOIN", "YEAR", "STRATUM",
                              "SPECIES_CODE", "CPUE_NOKM2")],
                 by = c("HAULJOIN", "SPECIES_CODE"))
  
  
  
  ##############################################
  ## Wakabayashi et al. 1985 Equation 16: 
  ##############################################
  ## At each station with length-frequency records, the number of individuals
  ##     within each sex/length class was estimated by expanding the 
  ##     length-frequency subsample to the total catch (per area swept). 
  ##     This is S_ijklm, the estimated number of individuals of species-k 
  ##     with sex-m and length-l caught at station-j within stratum-i.
  ##
  ## s_ijklm is the FREQUENCY column of the size df, the number of records
  ##     of sex-m and length-l of species-k recorded at station-j in stratum-i
  ## s_ijk is the frequency of recorded individuals summed over sex and length
  ##        class, i.e., the number of individuals of species-k recorded 
  ##        at station-j in stratum-i summed over all length and sex classes
  ## S_ijk is the estimated numbers per area swept from the cpue df
  ##     of species-k at station-j in stratum-i. The notation S_ijk is not 
  ##     used below, instead CPUE_NOKM2 is used. 
  ##
  ## s_ijk and NUMCPUE_IND_KM2 are merged into the size df by joining 
  ##     via the HAULJOIN value and species code so that calculations can
  ##     be vectorized easily.
  
  s_ijk <- stats::aggregate(
    FREQUENCY ~ SURVEY + YEAR + HAULJOIN + SPECIES_CODE,
    data = size,
    FUN = sum)
  names(s_ijk)[names(s_ijk) == "FREQUENCY"] <- "s_ijk"
  
  size <- merge(x = size,
                y = s_ijk[, c("SPECIES_CODE", "HAULJOIN", "s_ijk")],
                by = c("SPECIES_CODE", "HAULJOIN"))
  
  size$S_ijklm_NOKM2 <- size$FREQUENCY / size$s_ijk * size$CPUE_NOKM2
  
  ## Add units to length and return
  names(size)[names(size) == "LENGTH"] <- "LENGTH_MM"
  return(size)
  
}
