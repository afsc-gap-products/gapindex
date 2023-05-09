#' Calculate index of total biomass per stratum
#'
#' @param racebase_tables data object created from `gapindex::get_data()`
#' @param cpue object created from `gapindex::calc_cpue()`.
#' @param vulnerability the vulnerability of the species to the survey 
#'                      (defaults to 1).
#' 
#' @return dataframe of biomass and population abundance estimates across 
#'         strata, along with variances and CIs. 
#' 
#' @export
#'

calc_biomass_stratum <- function(racebase_tables = NULL,
                                 cpue = NULL,
                                 vulnerability = 1) {
  
  cruise <- racebase_tables$cruise
  haul <- racebase_tables$haul
  
  ## Check inputs
  if (any(sapply(X = list(cpue, racebase_tables), FUN = is.null))) {
    stop("Please provide inputs for data args: `cpue` and `racebase_tables`. 
         See ?gapindex::calc_biomass_stratum for more details.")
  }
  
  ## Calculate mean and variance of stratum biomass. For strata with only
  ## one station, variance is ASSUMED zero
  wgt_stats <- 
    stats::aggregate(
      CPUE_KGKM2 ~ SPECIES_CODE + STRATUM + YEAR + 
        SURVEY + SURVEY_DEFINITION_ID + DESIGN_YEAR, 
      data = cpue, 
      FUN = function(x) 
        c("CPUE_KGKM2_MEAN" = mean(x, na.rm = TRUE), 
          "CPUE_KGKM2_VAR" = ifelse(test = length(stats::na.omit(x)) < 2, 
                                    yes = NA, 
                                    no = stats::var(x, na.rm = TRUE) / 
                                      length(stats::na.omit(x))),
          "COUNT_HAUL" = length(x = x),
          "COUNT_CATCH" = length(x = stats::na.omit(x[x > 0]))
        ))
  
  ## Calculate mean and variance of stratum abundance. For strata with only
  ## one station, variance is ASSUMED zero
  num_stats <- 
    stats::aggregate(
      CPUE_NOKM2 ~ SPECIES_CODE + STRATUM + YEAR + 
        SURVEY + SURVEY_DEFINITION_ID + DESIGN_YEAR, 
      data = cpue,
      na.action = NULL,
      FUN = function(x) 
        c("CPUE_NOKM2_MEAN" = mean(x, na.rm = TRUE), 
          "CPUE_NOKM2_VAR" = ifelse(test = length(stats::na.omit(x)) < 2, 
                                    yes = NA, 
                                    no = stats::var(x, na.rm = TRUE) / 
                                      length(stats::na.omit(x))),
          "COUNT_NUMBER" =  length(x = stats::na.omit(x[x > 0]))
        ))
  
  ## Column merge mean wCPUE and nCPUE into one dataframe
  stratum_stats <- cbind(
    wgt_stats[, c("SPECIES_CODE", "STRATUM", "YEAR", "DESIGN_YEAR", 
                  "SURVEY", "SURVEY_DEFINITION_ID")],
    wgt_stats$CPUE_KGKM2,
    num_stats$CPUE_NOKM2)
  
  ## Attach stratum data to stratum_stats
  stratum_stats <- merge(
    x = stratum_stats,
    y = racebase_tables$strata[, c("SURVEY_DEFINITION_ID", "STRATUM", 
                                   "DESIGN_YEAR", "AREA_KM2")],
    by = c("SURVEY_DEFINITION_ID", "DESIGN_YEAR", "STRATUM"))
  
  ## Calculate design-based estimate and variance of biomass and abundance
  stratum_stats[, c("BIOMASS_MT", "BIOMASS_VAR", 
                    "POPULATION_COUNT", "POPULATION_VAR")] <-
    with(stratum_stats, 
         data.frame(BIOMASS_MT = AREA_KM2 * CPUE_KGKM2_MEAN / 
                      vulnerability * 0.001,
                    BIOMASS_VAR = AREA_KM2^2 * CPUE_KGKM2_VAR * 1e-6,
                    POPULATION_COUNT = AREA_KM2 * CPUE_NOKM2_MEAN / 
                      vulnerability,
                    POPULATION_VAR = AREA_KM2^2 * CPUE_NOKM2_VAR))
  
  ## Reorder fields, sort
  stratum_stats <- subset(x = stratum_stats,
                          select = c(SURVEY_DEFINITION_ID, SURVEY,
                                     STRATUM, SPECIES_CODE, YEAR,
                                     COUNT_HAUL, COUNT_CATCH, COUNT_NUMBER,
                                     CPUE_KGKM2_MEAN, CPUE_KGKM2_VAR, 
                                     CPUE_NOKM2_MEAN, CPUE_NOKM2_VAR,
                                     BIOMASS_MT, BIOMASS_VAR, 
                                     POPULATION_COUNT, POPULATION_VAR))
  stratum_stats <- stratum_stats[with(stratum_stats,
                                      order(YEAR, SURVEY_DEFINITION_ID,
                                            STRATUM, SPECIES_CODE)), ]
  
  ## Remove EBS + NW strata pre-1987 as these aren't used
  if (any(stratum_stats$YEAR < 1987 & stratum_stats$SURVEY == "EBS")) {
    warning("The (EBS + NW) output only includes years 1987-present.
      Years 1982-1986 are NOT included for the (EBS + NW) output because
      essentially no stations within strata 82 & 90 (subarea 8 & 9)
      were sampled during those years. Biomass/Abundance estimates for 
      these early years were removed.")
    
    stratum_stats <- subset(x = stratum_stats, 
                            subset = !(SURVEY_DEFINITION_ID == 98 & 
                                         YEAR < 1987 & 
                                         STRATUM %in% c(82, 90)) )
  }
  
  return(stratum_stats)
  
}
