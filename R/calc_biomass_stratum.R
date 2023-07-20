#' Calculate index of total biomass per stratum
#'
#' @param racebase_tables data object created from `gapindex::get_data()`
#' @param cpue object created from `gapindex::calc_cpue()`.
#' @param vulnerability the vulnerability of the species to the survey 
#'                      (defaults to 1).
#' 
#' @return dataframe of biomass and population abundance estimates (with 
#' associated variances) across survey, year, species, and strata 
#' 
#' | Field Name           | Description                                                                                                                                                         |
#' |----------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' | SURVEY_DEFINITION_ID | Integer number identifier corresponding to survey region. See   gapindex::survey_ids for a list of relevant survey regions.                                         |
#' | SURVEY               | Survey region.                                                                                                                                                      |
#' | STRATUM              | Stratum ID. STRATUM = 0 indicates an experimental tow.                                                                                                              |
#' | SPECIES_CODE         | Taxon code. [See the code book for the full   list.](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
#' | YEAR                 | Survey year.                                                                                                                                                        |
#' | N_HAUL               | Number of hauls in stratum.                                                                                                                                         |
#' | N_WEIGHT             | Number of hauls with a positive catch weight.                                                                                                                       |
#' | N_COUNT              | Number of hauls with a positive numerical catch.                                                                                                                    |
#' | N_LENGTH             | Number of hauls with length-frequency data                                                                                                                          |
#' | CPUE_KGKM2_MEAN      | Mean weight catch per area swept (kg/km^2).                                                                                                                         |
#' | CPUE_KGKM2_VAR       | Variance of the mean weight catch per area swept.                                                                                                                   |
#' | CPUE_NOKM2_MEAN      | Mean numerical catch per area swept (no/km^2).                                                                                                                      |
#' | CPUE_NOKM2_VAR       | Variance of the mean weight catch per area swept                                                                                                                    |
#' | BIOMASS_MT           | Estimated total biomass (mt).                                                                                                                                       |
#' | BIOMASS_VAR          | Variance associated with the estimated total biomass.                                                                                                               |
#' | POPULATION_COUNT     | Estimated total numerical abundance (numbers).                                                                                                                      |
#' | POPULATION_VAR       | Variance associated with the estimated total numerical abundance.                                                                                                   |
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
          "N_HAUL" = length(x = x),
          "N_WEIGHT" = length(x = stats::na.omit(x[x > 0]))
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
        c("CPUE_NOKM2_MEAN" = ifelse(test = length(stats::na.omit(x)) == 0,
                                     yes = NA,
                                     no = mean(x, na.rm = TRUE)), 
          "CPUE_NOKM2_VAR" = ifelse(test = length(stats::na.omit(x)) < 2, 
                                    yes = NA, 
                                    no = stats::var(x, na.rm = TRUE) / 
                                      length(stats::na.omit(x))),
          "N_COUNT" =  length(x = stats::na.omit(x[x > 0]))
        ))
  
  if (!is.null(x = racebase_tables$size)) {
    size <- merge(x = racebase_tables$size,
                  y = haul[, c("HAULJOIN", "STRATUM")],
                  by = "HAULJOIN")
    size <- merge(x = size, 
                  y = racebase_tables$cruise,
                  by = "CRUISEJOIN")
    
    size_stats <- 
      aggregate(HAULJOIN ~ SPECIES_CODE + STRATUM + YEAR + 
                  SURVEY + SURVEY_DEFINITION_ID + DESIGN_YEAR,
                data = size,
                FUN = function(x) length(unique(x)))
    
  } else {
    size_stats <- subset(x = num_stats, 
                         select = c("SPECIES_CODE", "STRATUM", "YEAR", 
                                    "SURVEY", "SURVEY_DEFINITION_ID", 
                                    "DESIGN_YEAR"))
    size_stats$HAULJOIN <- 0
    
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
    wgt_stats$CPUE_KGKM2,
    num_stats$CPUE_NOKM2)
  
  ## Merge "HAULJOIN" column from `size_stats` into stratum_stats using 
  ## "SPECIES_CODE", "STRATUM", "YEAR", "DESIGN_YEAR", "SURVEY", and 
  ## "SURVEY_DEFINITION_ID" as a composite key
  stratum_stats <- 
    merge(x = stratum_stats,
          y = size_stats,
          by = c("SPECIES_CODE", "STRATUM", "YEAR", "DESIGN_YEAR", 
                 "SURVEY", "SURVEY_DEFINITION_ID"),
          all.x = TRUE)
  names(stratum_stats)[names(stratum_stats) == "HAULJOIN"] <- "N_LENGTH"
  stratum_stats$N_LENGTH[is.na(stratum_stats$N_LENGTH)] <- 0
  
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
                                     N_HAUL, N_WEIGHT, N_COUNT, N_LENGTH,
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
