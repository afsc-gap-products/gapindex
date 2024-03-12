#' Calculate CPUEs at haul level
#' 
#' @description This function zero-fills the catch data and converts numbers
#'              and weights to CPUE.
#' 
#' @param racebase_tables data object created from `gapindex::get_data()`
#'         
#' @eval c("@return", get_table_metadata("inst/extdata/metadata.csv", 
#' select = c("SURVEY_DEFINITION_ID", "SURVEY", "CRUISE", "CRUISEJOIN", 
#' "YEAR", "HAULJOIN", "STRATUM", "DESIGN_YEAR", "LATITUDE_DD_START",
#' "LATITUDE_DD_END","LONGITUDE_DD_START", "LONGITUDE_DD_END") ))
#' 
#' @export
#' 

calc_cpue <- function(racebase_tables = NULL) {
  ## Input check
  if (is.null(x = racebase_tables))
    stop("Must provide argument `racebase_tables` a named list from 
         gapindex::get_data().")
  
  ## Extract objects from `racebase_tables`
  cruisedat <- racebase_tables$cruise 
  haul <- racebase_tables$haul
  catch <- racebase_tables$catch
  species <- racebase_tables$species
  
  ## Remove species with no catch records, output warning
  omitted_species <- 
    species$GROUP_CODE[!species$GROUP_CODE %in% 
                         racebase_tables$catch$SPECIES_CODE]
  species <- species[GROUP_CODE %in% unique(catch$SPECIES_CODE)]
  
  if (length(x = omitted_species) > 0) 
    warning(paste0("No catch records found for species codes: ", 
                   gapindex::stitch_entries(omitted_species)))
  
  ## For abundance index calculations, the haul df should only have records 
  ## where haul$ABUNDANCE_HAUL == "Y". This check is just a warning to the user.
  if (any(haul$ABUNDANCE_HAUL == "N")) {
    warning(paste("The `haul` dataframe in argument `racebase_tables``",
                  "contains records where haul$abundance_haul == 'N'.",
                  "Standard abundance index calculations only include records",
                  "where haul$abundance_haul == 'Y'. "))
  }
  
  ## Merge "SURVEY", "SURVEY_DEFINITION_ID", "DESIGN_YEAR", and "YEAR" columns 
  ## from `cruisedat` to `haul` using "CRUISEJOIN" as the key.
  dat <- cruisedat[, c("CRUISEJOIN", "SURVEY", "SURVEY_DEFINITION_ID", 
                       "DESIGN_YEAR", "YEAR")][haul, on = "CRUISEJOIN"]
  
  ##  `dat` only has non-zero records. To fill in zero-weight records, we 
  ## first create a table called `all_combos` of "HAULJOIN" and "SPECIES_CODE"
  all_combos <- 
    data.table::CJ(HAULJOIN = dat$HAULJOIN, 
                   SPECIES_CODE = sort(x = unique(x = catch$SPECIES_CODE)))
  
  ## Then left join the haul data in `dat` to `all_combos` using "HAULJOIN"
  ## as the key.
  dat <- merge(x = all_combos, 
               y = dat, 
               by = "HAULJOIN", 
               all.x = TRUE)
  
  ## Left Join `catch` with `dat` using the "HAULJOIN" AND "SPECIES_CODE" 
  ## as a composite key
  dat <- merge(x = dat,
               y = catch, 
               by = c("HAULJOIN", "SPECIES_CODE"), 
               all.x = TRUE)
  
  ## There are some hauls where a weight is recorded, but not a count. 
  ## These records perhaps erroneously have a zero. These "zero" counts are
  ## not included in the numbers per area swept calculation, so we need to 
  ## NA these values and put zero counts for hauls with zero weights
  dat$NUMBER_FISH[dat$WEIGHT > 0 & dat$NUMBER_FISH == 0] <- NA
  
  ## Any record with no weight or count data (NA) are replaced with a zero
  dat[is.na(x = dat$WEIGHT) & is.na(x = dat$NUMBER_FISH), 
      c("WEIGHT", "NUMBER_FISH")] <- 0
  
  ## reorder columns, rename some
  dat <- with(dat,
              data.table::data.table(
                SURVEY_DEFINITION_ID, SURVEY, CRUISE, CRUISEJOIN, YEAR, 
                HAULJOIN, STRATUM, DESIGN_YEAR,
                LATITUDE_DD_START = START_LATITUDE, 
                LATITUDE_DD_END = END_LATITUDE, 
                LONGITUDE_DD_START = START_LONGITUDE, 
                LONGITUDE_DD_END = END_LONGITUDE,
                DEPTH_M = BOTTOM_DEPTH,
                BOTTOM_TEMPERATURE_C = GEAR_TEMPERATURE,
                SPECIES_CODE,
                WEIGHT_KG = WEIGHT,
                COUNT = NUMBER_FISH,
                AREA_SWEPT_KM2 = DISTANCE_FISHED * (0.001 * NET_WIDTH)))
  
  ## CPUE calculations
  dat <- cbind(dat, 
               with(dat, data.table::data.table(
                 CPUE_KGKM2 = WEIGHT_KG / AREA_SWEPT_KM2,
                 CPUE_NOKM2 = COUNT / AREA_SWEPT_KM2))
  )
  
  return(dat)
}
