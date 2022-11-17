#' Calculate index of total biomass per stratum
#'
#' @param cpue object created from AFSC.GAP.DBE::get_cpue().
#' @param haul object created from the haul slot from the object created from
#'             AFSC.GAP.DBE::get_data().
#' @param strata object created from the stratum slot from the object created 
#'               from AFSC.GAP.DBE::get_data().
#' @param vulnerability the vulnerability of the species to the survey 
#'                      (defaults to 1).
#' @param ci_val numeric, ci_val% confidence interval. Defaults to 95.
#' 
#' @return dataframe of biomass and population abundance estimates across 
#'         strata, along with variances and CIs. 
#' 
#' @export
#'

get_biomass_stratum <- function(cpue = NULL,
                                haul = NULL,
                                strata = NULL,
                                vulnerability = 1,
                                ci_val = 95) {
  
  ## Check inputs
  if (any(sapply(X = list(cpue, haul, strata), FUN = is.null))) {
    stop("Please provide inputs for data arguments: `cpue`, `haul`, and 
         `strata`. See ?AFSC.GAP.DBE::get_biomass_stratum for more details.")
  }
  
  ## Calculate mean and variance of stratum biomass. For strata with only
  ## one station, variance is ASSUMED zero
  wgt_stats <- 
    stats::aggregate(
      WGTCPUE_KG_KM2 ~ SPECIES_CODE + STRATUM + YEAR, 
      data = cpue, 
      FUN = function(x) 
        c("haul_count" = length(x),
          "mean_wgt_cpue" = mean(x, na.rm = TRUE), 
          "var_wgt_cpue" = ifelse(test = length(x) < 2, 
                                  yes = 0, 
                                  no = stats::var(x) / length(x))))
  
  ## Calculate mean and variance of stratum abundance. For strata with only
  ## one station, variance is ASSUMED zero
  num_stats <- 
    stats::aggregate(
      NUMCPUE_IND_KM2 ~ SPECIES_CODE + STRATUM + YEAR, 
      data = cpue, 
      FUN = function(x) 
        c("mean_num_cpue" = ifelse(test = length(x) == 1, 
                                   yes = 0, 
                                   no = mean(x, na.rm = TRUE)), 
          "var_num_cpue" = ifelse(test = length(x) < 2, 
                                  yes = 0, 
                                  no = stats::var(x) / length(x)),
          "catch_count" = sum(x > 0)))
  
  ## Column merge mean wCPUE and nCPUE into one dataframe
  stratum_stats <-  cbind(wgt_stats[, c("SPECIES_CODE", "STRATUM", "YEAR")],
                          data.frame(wgt_stats$WGTCPUE_KG_KM2),
                          data.frame(num_stats$NUMCPUE_IND_KM2))
  
  ## Megsie check
  if (all(stratum_stats$catch_count <= stratum_stats$haul_count)) {
    print("Number of hauls with positive catches is realistic.")
  }
  
  ## Attach stratum data to stratum_stats
  stratum_stats <- merge(x = stratum_stats, 
                         y = strata, 
                         by = "STRATUM")
  
  ## Calculate t-value of the stratum estimates based on the df (number of 
  ## hauls - 1)
  stratum_stats$qt_val <- 
    ifelse(test = stratum_stats$haul_count <= 1,
           yes = 0,
           no = suppressWarnings(stats::qt(p = (1 - ci_val / 100) / 2,
                                           df = stratum_stats$haul_count - 1,
                                           lower.tail = F)))
  
  ## Calculate design-based estimate and variance of biomass and abundance
  stratum_stats[, c("biomass_mt", "biomass_var", "pop", "pop_var")] <-
    with(stratum_stats, 
         data.frame(biomass_mt = AREA * mean_wgt_cpue / vulnerability * 0.001,
                    biomass_var = AREA^2 * var_wgt_cpue * 1e-6,
                    pop = AREA * mean_num_cpue,
                    pop_var = AREA^2 * var_num_cpue))
  
  ## Calculate 95% CIs
  stratum_stats[, c("biomass_lower_ci", "biomass_upper_ci", 
                    "pop_lower_ci", "pop_upper_ci")] <-
    with(stratum_stats, data.frame(
      biomass_lower_ci = biomass_mt - qt_val * sqrt(biomass_var), 
      biomass_upper_ci = biomass_mt + qt_val * sqrt(biomass_var),
      pop_lower_ci = pop - qt_val * sqrt(pop_var),
      pop_upper_ci = pop + qt_val * sqrt(pop_var)))
  
  ## Set any negative lower ci values to zero
  stratum_stats[, c("biomass_lower_ci", "pop_lower_ci")] <- 
    apply(X = stratum_stats[, c("biomass_lower_ci", "pop_lower_ci")],
          MARGIN = 1,
          FUN = function(x) max(x, 0, na.rm = TRUE))
  
  return(stratum_stats)
}
