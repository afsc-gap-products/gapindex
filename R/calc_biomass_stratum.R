#' Calculate index of total biomass per stratum
#'
#' @param racebase_tables data object created from `gapindex::get_data()`
#' @param cpue object created from `gapindex::calc_cpue()`.
#' @param vulnerability the vulnerability of the species to the survey 
#'                      (defaults to 1).
#'                      
#' @eval c("@return", get_table_metadata("data-raw/metadata.csv", 
#' select = c("SURVEY_DEFINITION_ID", "SURVEY", "STRATUM", "SPECIES_CODE" ,
#' "YEAR", "N_HAUL", "N_WEIGHT", "N_COUNT", "N_LENGTH", "CPUE_KGKM2_MEAN",
#' "CPUE_KGKM2_VAR", "CPUE_NOKM2_MEAN", "CPUE_NOKM2_VAR", "BIOMASS_MT",
#' "BIOMASS_VAR", "POPULATION_COUNT", "POPULATION_VAR")))
#' 
#' @export
#'

calc_biomass_stratum <- function(racebase_tables = NULL,
                                 cpue = NULL,
                                 vulnerability = 1) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Gather datasets from `racebase_tables`
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cruise <- racebase_tables$cruise
  haul <- racebase_tables$haul
  
  ## Check inputs
  if (any(sapply(X = list(cpue, racebase_tables), FUN = is.null))) {
    stop("Please provide inputs for data args: `cpue` and `racebase_tables`. 
         See ?gapindex::calc_biomass_stratum for more details.")
  }
  
  ## Calculate mean and variance stratum weight CPUE, total number of hauls and 
  ## number of hauls with positive weights. For strata with only one station, 
  ## the variance is not defined (coded as NA).
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
  
  ## Calculate mean and variance stratum numerical CPUE, and number of hauls 
  ## with count data. For strata with only one station, the variance is not 
  ## defined (coded as NA).
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
    ## Attach "STRATUM" column from `haul` to the size table using "HAULJOIN"
    ## as the key
    size <- merge(x = racebase_tables$size,
                  y = haul[, c("HAULJOIN", "STRATUM")],
                  by = "HAULJOIN")
    
    ## Attach "CRUISEJOIN" column from the cruise data to the `size` 
    ## table using "CRUISEJOIN" as the key
    size <- merge(x = size, 
                  y = racebase_tables$cruise,
                  by = "CRUISEJOIN")
    
    ## Calculate the number of hauls with size data
    size_stats <- 
      aggregate(HAULJOIN ~ SPECIES_CODE + STRATUM + YEAR + 
                  SURVEY + SURVEY_DEFINITION_ID + DESIGN_YEAR,
                data = size,
                FUN = function(x) length(x = unique(x = x)))
    names(x = size_stats)[names(x = size_stats) == "HAULJOIN"] <- "N_LENGTH"
  } else { # If there are no size data, the number of hauls with size data is 0
    size_stats <- subset(x = num_stats, 
                         select = c("SPECIES_CODE", "STRATUM", "YEAR", 
                                    "SURVEY", "SURVEY_DEFINITION_ID", 
                                    "DESIGN_YEAR"))
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
    wgt_stats$CPUE_KGKM2,
    num_stats$CPUE_NOKM2)
  
  ## Merge N_LENGTH column from `size_stats` into stratum_stats using 
  ## "SPECIES_CODE", "STRATUM", "YEAR", "DESIGN_YEAR", "SURVEY", and 
  ## "SURVEY_DEFINITION_ID" as a composite key
  stratum_stats <- 
    merge(x = stratum_stats,
          y = size_stats,
          by = c("SPECIES_CODE", "STRATUM", "YEAR", "DESIGN_YEAR", 
                 "SURVEY", "SURVEY_DEFINITION_ID"),
          all.x = TRUE)
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
