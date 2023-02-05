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
        c("mean_wgt_cpue" = mean(x, na.rm = TRUE), 
          "var_wgt_cpue" = ifelse(test = length(x) < 2, 
                                  yes = 0, 
                                  no = stats::var(x) / length(x))))
  
  ## Calculate mean and variance of stratum abundance. For strata with only
  ## one station, variance is ASSUMED zero
  num_stats <- 
    stats::aggregate(
      NUMCPUE_IND_KM2 ~ GROUP + STRATUM + YEAR, 
      data = cpue,
      na.action = NULL,
      FUN = function(x) 
        c("mean_num_cpue" = mean(x, na.rm = TRUE), 
          "var_num_cpue" = ifelse(test = length(stats::na.omit(x)) < 2, 
                                  yes = 0, 
                                  no = stats::var(x, na.rm = TRUE) / 
                                    length(stats::na.omit(x)))))
  
  ## Column merge mean wCPUE and nCPUE into one dataframe
  stratum_stats <-  cbind(wgt_stats[, c("GROUP", "STRATUM", "YEAR")],
                          data.frame(wgt_stats$WGTCPUE_KG_KM2),
                          data.frame(num_stats$NUMCPUE_IND_KM2))
  
  ## Attach stratum data to stratum_stats
  stratum_stats <- merge(x = stratum_stats, 
                         y = racebase_tables$strata, 
                         by = "STRATUM")
  
  ## Calculate design-based estimate and variance of biomass and abundance
  stratum_stats[, c("biomass_mt", "biomass_var", "pop", "pop_var")] <-
    with(stratum_stats, 
         data.frame(biomass_mt = AREA * mean_wgt_cpue / vulnerability * 0.001,
                    biomass_var = AREA^2 * var_wgt_cpue * 1e-6,
                    pop = AREA * mean_num_cpue,
                    pop_var = AREA^2 * var_num_cpue))
  
  return(stratum_stats)
}
