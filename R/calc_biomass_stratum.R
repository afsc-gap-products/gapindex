#' Calculate index of total biomass per stratum
#'
#' @param racebase_tables data object created from `AFSC.GAP.DBE::get_data()`
#' @param cpue object created from `AFSC.GAP.DBE::calc_cpue()`.
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
  
  ## Check inputs
  if (any(sapply(X = list(cpue, racebase_tables), FUN = is.null))) {
    stop("Please provide inputs for data arguments: `cpue` and `racebase_tables`. 
         See ?AFSC.GAP.DBE::calc_biomass_stratum for more details.")
  }
  
  ## Calculate mean and variance of stratum biomass. For strata with only
  ## one station, variance is ASSUMED zero
  wgt_stats <- 
    stats::aggregate(
      WGTCPUE_KG_KM2 ~ GROUP + STRATUM + YEAR, 
      data = cpue, 
      FUN = function(x) 
        c("MEAN_WGTCPUE_KG_KM2" = mean(x, na.rm = TRUE), 
          "VAR_WGTCPUE" = ifelse(test = length(x) < 2, 
                                 yes = 0, 
                                 no = stats::var(x) / length(x))))
  
  ## Calculate mean and variance of stratum abundance. For strata with only
  ## one station, variance is ASSUMED zero
  num_stats <- 
    stats::aggregate(
      NUMCPUE_COUNT_KM2 ~ GROUP + STRATUM + YEAR, 
      data = cpue,
      na.action = NULL,
      FUN = function(x) 
        c("MEAN_NUMCPUE_COUNT_KM2" = mean(x, na.rm = TRUE), 
          "VAR_NUMCPUE" = ifelse(test = length(stats::na.omit(x)) < 2, 
                                 yes = 0, 
                                 no = stats::var(x, na.rm = TRUE) / 
                                   length(stats::na.omit(x)))))
  
  ## Column merge mean wCPUE and nCPUE into one dataframe
  stratum_stats <-  cbind(wgt_stats[, c("GROUP", "STRATUM", "YEAR")],
                          data.frame(wgt_stats$WGTCPUE_KG_KM2),
                          data.frame(num_stats$NUMCPUE_COUNT_KM2))
  
  ## Attach stratum data to stratum_stats
  stratum_stats <- merge(x = stratum_stats, 
                         y = racebase_tables$strata, 
                         by = "STRATUM")
  
  ## Calculate design-based estimate and variance of biomass and abundance
  stratum_stats[, c("BIOMASS_MT", "BIOMASS_VAR", 
                    "POPULATION_COUNT", "POPULATION_VAR")] <-
    with(stratum_stats, 
         data.frame(BIOMASS_MT = AREA * MEAN_WGTCPUE_KG_KM2 / vulnerability * 0.001,
                    BIOMASS_VAR = AREA^2 * VAR_WGTCPUE * 1e-6,
                    POPULATION_COUNT = AREA * MEAN_NUMCPUE_COUNT_KM2,
                    POPULATION_VAR = AREA^2 * VAR_NUMCPUE))
  
  ## Reorder fields
  stratum_stats <- subset(x = stratum_stats, 
                          select = c(SURVEY, YEAR, STRATUM, GROUP, 
                                     MEAN_WGTCPUE_KG_KM2, VAR_WGTCPUE, 
                                     BIOMASS_MT, BIOMASS_VAR,
                                     MEAN_NUMCPUE_COUNT_KM2, VAR_NUMCPUE,
                                     POPULATION_COUNT, POPULATION_VAR))
  
  return(stratum_stats)
}
