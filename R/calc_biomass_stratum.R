#' Calculate index of total biomass per stratum
#'
#' @param racebase_tables `r lifecycle::badge("deprecated")` Use the 
#'                     `gapdata` argument instead. 
#' @param gapdata data object created from `gapindex::get_data()`
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

calc_biomass_stratum <- function(gapdata = NULL,
                                 racebase_tables = lifecycle::deprecated(),
                                 cpue = NULL) {
  ## Input check
  if (lifecycle::is_present(racebase_tables)) {
    lifecycle::deprecate_warn("2.2.0", 
                              "get_data(sql_channel)", 
                              "get_data(channel)")
    gapdata <- racebase_tables
  }
  
  for (iarg in c("gapdata", "cpue"))
    if (is.null(x = get(x = iarg)))
      stop(paste0("Must provide argument `", iarg, "`. ",
                  "See ?gapindex::calc_biomass_stratum for more information"))
  
  ## Gather datasets from `gapdata`
  cruise <- gapdata$cruise
  haul <- gapdata$haul
  
  ## Calculate stratum-level mean weight CPUE (and associated variance), total 
  ## number of hauls and number of hauls with positive weights.
  wgt_stats <- 
    cpue[, 
         list(
           ## CPUE_KGKM2_MEAN is the mean stratum weight CPUE estimate
           "CPUE_KGKM2_MEAN" = mean(x = CPUE_KGKM2, na.rm = TRUE),
           ## CPUE_KGKM2_VAR is the variance associated with the mean stratum
           ## weight CPUE estimate. If a stratum only contains one station, the 
           ## variance is not defined and is given an NA 
           "CPUE_KGKM2_VAR" = ifelse(
             test = length(stats::na.omit(CPUE_KGKM2)) < 2,
             yes = NA_real_,
             no = stats::var(CPUE_KGKM2, na.rm = TRUE) /
               length(stats::na.omit(CPUE_KGKM2))),
           ## N_HAUL is the number of hauls
           "N_HAUL" = length(x = CPUE_KGKM2),
           ## N_COUNT is the number of hauls with weight data
           "N_WEIGHT" = length(x = stats::na.omit(CPUE_KGKM2[CPUE_KGKM2 > 0]))
         ), 
         by = c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY", 
                "SURVEY_DEFINITION_ID", "DESIGN_YEAR")]
  
  ## Calculate stratum-level mean numerical CPUE (and associated variance), and
  ## number of hauls with count data.
  num_stats <- 
    cpue[, 
         list(
           ## CPUE_NOKM2_MEAN is the mean numerical CPUE estimate. If there are
           ## no recorded counts (e.g., Porifera) then it is given an NA. 
           "CPUE_NOKM2_MEAN" = ifelse(
             test = length(x = stats::na.omit(CPUE_NOKM2)) == 0,
             yes = NA_real_,
             no = mean(CPUE_NOKM2, na.rm = TRUE)), 
           ## CPUE_NOKM2_VAR is the variance associated with the mean stratum
           ## numerical CPUE estimate. If a stratum only contains one station, 
           ## it is given an NA variance 
           "CPUE_NOKM2_VAR" = ifelse(
             test = length(stats::na.omit(CPUE_NOKM2)) < 2, 
             yes = NA_real_, 
             no = stats::var(CPUE_NOKM2, na.rm = TRUE) / 
               length(stats::na.omit(CPUE_NOKM2))),
           ## N_COUNT is the number of hauls with count data
           "N_COUNT" = length(x = stats::na.omit(CPUE_NOKM2[CPUE_NOKM2 > 0])) 
         ), 
         by = c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY", 
                "SURVEY_DEFINITION_ID", "DESIGN_YEAR")]
  
  if (!is.null(x = gapdata$size)) {
    
    ## Merge the `cruise` data onto the `haul` df using "CRUISEJOIN" as the 
    ## key, then merge that resultant df into gapdata$size using 
    ## "HAULJOIN" as the key
    size <- merge(x = haul,
                  y = cruise,
                  by = "CRUISEJOIN")
    size <- merge(x = size,
                  y = gapdata$size,
                  by = "HAULJOIN")
    
    ## Calculate the number of hauls with size data
    size_stats <- size[, .("N_LENGTH" = length(x = unique(x = HAULJOIN))), 
                       by = c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY",
                              "SURVEY_DEFINITION_ID", "DESIGN_YEAR")]
    
  } else { # If there are no size data, the number of hauls with size data is 0
    size_stats <- num_stats[, c("SPECIES_CODE", "STRATUM", "YEAR", "SURVEY",
                                "SURVEY_DEFINITION_ID", "DESIGN_YEAR")]
    size_stats$N_LENGTH <- 0
    
    warning(paste0("Size data are not present in argument `gapdata`. ",
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
  ## "SURVEY_DEFINITION_ID" as a composite key. 
  stratum_stats <- 
    merge(x = stratum_stats,
          y = size_stats,
          by = c("SPECIES_CODE", "STRATUM", "YEAR", "DESIGN_YEAR", 
                 "SURVEY", "SURVEY_DEFINITION_ID"),
          all.x = TRUE)
  
  ## Any remaining records with an NA for N_LENGTH is assigned a value of 0
  stratum_stats[is.na(x = stratum_stats$N_LENGTH), N_LENGTH := 0]
  
  ## Attach stratum data to stratum_stats using "SURVEY_DEFINITION_ID", 
  ## "STRATUM", "DESIGN_YEAR", "AREA_KM2"
  stratum_stats <-
    merge(x = stratum_stats,
          y = gapdata$strata[, c("SURVEY_DEFINITION_ID", "STRATUM", 
                                 "DESIGN_YEAR", "AREA_KM2")],
          by = c("SURVEY_DEFINITION_ID", "DESIGN_YEAR", "STRATUM"))
  
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
    
    stratum_stats <- subset(x = stratum_stats,
                            subset = !(SURVEY_DEFINITION_ID == 98 &
                                         YEAR < 1987 & 
                                         STRATUM %in% c(82, 90)) )
  }
  
  return(data.table::data.table(stratum_stats, 
                                key = c("SURVEY_DEFINITION_ID", "SURVEY", 
                                        "SPECIES_CODE", "YEAR", "STRATUM")))
}
