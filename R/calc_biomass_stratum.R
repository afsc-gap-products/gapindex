#' Calculate index of total biomass per stratum
#'
#' @param racebase_tables data object created from `gapindex::get_data()`
#' @param cpue object created from `gapindex::calc_cpue()`.
#'                      
#' @eval c("@return", get_table_metadata("inst/extdata/metadata.csv", 
#' select = c("SURVEY_DEFINITION_ID", "SURVEY", "STRATUM", "SPECIES_CODE" ,
#' "YEAR", "N_HAUL", "N_WEIGHT", "N_COUNT", "N_LENGTH", "CPUE_KGKM2_MEAN",
#' "CPUE_KGKM2_VAR", "CPUE_NOKM2_MEAN", "CPUE_NOKM2_VAR", "BIOMASS_MT",
#' "BIOMASS_VAR", "POPULATION_COUNT", "POPULATION_VAR")))
#' 
#' @export
#'

calc_biomass_stratum <- function(racebase_tables = NULL,
                                 cpue = NULL) {
  
  ## Check inputs
  if (any(sapply(X = list(cpue, racebase_tables), FUN = is.null))) {
    stop("Please provide inputs for data args: `cpue` and `racebase_tables`. 
         See ?gapindex::calc_biomass_stratum for more details.")
  }
  
  ##   Gather datasets from `racebase_tables`
  cruise <- racebase_tables$cruise
  haul <- racebase_tables$haul
  
  ## Calculate mean and variance stratum weight CPUE, total number of hauls and 
  ## number of hauls with positive weights. For strata with only one station, 
  ## the variance is not defined (coded as NA).
  wgt_stats <- 
    cpue[, 
         list(
           "CPUE_KGKM2_MEAN" = mean(x = CPUE_KGKM2, na.rm = TRUE),
           "CPUE_KGKM2_VAR" = ifelse(
             test = length(stats::na.omit(CPUE_KGKM2)) < 2,
             yes = NA_real_,
             no = stats::var(CPUE_KGKM2, na.rm = TRUE) /
               length(stats::na.omit(CPUE_KGKM2))),
           "N_HAUL" = length(x = CPUE_KGKM2),
           "N_WEIGHT" = length(x = stats::na.omit(CPUE_KGKM2[CPUE_KGKM2 > 0])) 
         ), 
         by = c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY", 
                "SURVEY_DEFINITION_ID", "DESIGN_YEAR")]
  
  ## Calculate mean and variance stratum numerical CPUE, and number of hauls 
  ## with count data. For strata with only one station, the variance is not 
  ## defined (coded as NA).
  num_stats <- 
    cpue[, 
         list(
           "CPUE_NOKM2_MEAN" = ifelse(
             test = length(x = stats::na.omit(CPUE_NOKM2)) == 0,
             yes = NA_real_,
             no = mean(CPUE_NOKM2, na.rm = TRUE)), 
           "CPUE_NOKM2_VAR" = ifelse(
             test = length(stats::na.omit(CPUE_NOKM2)) < 2, 
             yes = NA_real_, 
             no = stats::var(CPUE_NOKM2, na.rm = TRUE) / 
               length(stats::na.omit(CPUE_NOKM2))),
           "N_COUNT" = length(x = stats::na.omit(CPUE_NOKM2[CPUE_NOKM2 > 0])) 
         ), 
         by = c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY", 
                "SURVEY_DEFINITION_ID", "DESIGN_YEAR")]
  
  if (!is.null(x = racebase_tables$size)) {
    
    ## Merge the cruise df into haul using "CRUISEJOIN" as the key, then
    ## merge that resultant df into racebase_tables$size using "HAULJOIN"
    ## as the key
    size <- racebase_tables$size[cruise[haul, 
                                        on = "CRUISEJOIN"], 
                                 on = "HAULJOIN"]
    
    ## Calculate the number of hauls with size data
    size_stats <- size[, .("N_LENGTH" = length(x = unique(x = HAULJOIN))), 
                       by = c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY",
                              "SURVEY_DEFINITION_ID", "DESIGN_YEAR")]
    
  } else { # If there are no size data, the number of hauls with size data is 0
    size_stats <- num_stats[, c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY",
                                "SURVEY_DEFINITION_ID", "DESIGN_YEAR")]
    size_stats$N_LENGTH <- 0
    
    warning(paste0("Size data are not present in argument `racebase_tables`. ",
                   "Thus, the 'N_LENGTH' column in the output of this ",
                   "function is assumed to be zero but double-check ",
                   "that the `pull_lengths` argument in the ",
                   "gapindex::get_data() call == TRUE."))
  }
  
  ## Column merge mean wCPUE and nCPUE into one dataframe
  stratum_stats <- cbind(
    wgt_stats[, c("SPECIES_CODE", "STRATUM", "YEAR", "DESIGN_YEAR", 
                  "SURVEY", "SURVEY_DEFINITION_ID")],
    wgt_stats[, c("CPUE_KGKM2_MEAN", "CPUE_KGKM2_VAR", "N_HAUL", "N_WEIGHT")],
    num_stats[, c("CPUE_NOKM2_MEAN", "CPUE_NOKM2_VAR", "N_COUNT")])
  
  ## Merge N_LENGTH column from `size_stats` into stratum_stats using 
  ## "SPECIES_CODE", "STRATUM", "YEAR", "DESIGN_YEAR", "SURVEY", and 
  ## "SURVEY_DEFINITION_ID" as a composite key
  stratum_stats <- 
    size_stats[stratum_stats, 
               on = c("SPECIES_CODE", "STRATUM", "YEAR", "DESIGN_YEAR", 
                      "SURVEY", "SURVEY_DEFINITION_ID")]
  stratum_stats[is.na(x = stratum_stats$N_LENGTH), N_LENGTH := 0]
  
  ## Attach stratum data to stratum_stats
  stratum_stats <- 
    racebase_tables$strata[, c("SURVEY_DEFINITION_ID", "STRATUM", 
                               "DESIGN_YEAR", "AREA_KM2")][
                                 stratum_stats, 
                                 on = c("SURVEY_DEFINITION_ID", 
                                        "DESIGN_YEAR", "STRATUM")]
  
  ## Calculate design-based estimate and variance of biomass and abundance
  stratum_stats[, c("BIOMASS_MT", "BIOMASS_VAR", 
                    "POPULATION_COUNT", "POPULATION_VAR")] <-
    with(stratum_stats, 
         data.table::data.table(
           BIOMASS_MT = AREA_KM2 * CPUE_KGKM2_MEAN * 0.001,
           BIOMASS_VAR = AREA_KM2^2 * CPUE_KGKM2_VAR * 1e-6,
           POPULATION_COUNT = AREA_KM2 * CPUE_NOKM2_MEAN,
           POPULATION_VAR = AREA_KM2^2 * CPUE_NOKM2_VAR))
  
  ## Reorder fields, sort
  stratum_stats <-
    stratum_stats[order(YEAR, SURVEY_DEFINITION_ID, STRATUM, SPECIES_CODE),
                  .(SURVEY_DEFINITION_ID, SURVEY, STRATUM, SPECIES_CODE, YEAR,
                    N_HAUL, N_WEIGHT, N_COUNT, N_LENGTH,
                    CPUE_KGKM2_MEAN, CPUE_KGKM2_VAR, 
                    CPUE_NOKM2_MEAN, CPUE_NOKM2_VAR,
                    BIOMASS_MT, BIOMASS_VAR, 
                    POPULATION_COUNT, POPULATION_VAR)]
  
  ## Remove EBS + NW strata pre-1987 as these aren't used
  if (any(stratum_stats$YEAR < 1987 & stratum_stats$SURVEY == "EBS")) {
    warning("The (EBS + NW) output only includes years 1987-present.
      Years 1982-1986 are NOT included for the (EBS + NW) output because
      essentially no stations within strata 82 & 90 (subarea 8 & 9)
      were sampled during those years. Biomass/Abundance estimates for 
      these early years were removed.")
    
    stratum_stats <- stratum_stats[!(SURVEY_DEFINITION_ID == 98 &
                                       YEAR < 1987 & 
                                       STRATUM %in% c(82, 90)) ]
  }
  
  return(stratum_stats)
}
