#' Calculate haul-level catch per unit effort
#' 
#' @description This function calculates and zero-fills weight and numerical
#'              catch per unit effort (area swept, km2) 
#' 
#' @param racebase_tables `r lifecycle::badge("deprecated")` Use the 
#'                     `gapdata` argument instead. 
#' @param gapdata data object created from `gapindex::get_data()`
#'         
#' @eval c("@return", get_table_metadata("inst/extdata/metadata.csv", 
#' select = c("SURVEY_DEFINITION_ID", "SURVEY", "CRUISE", "CRUISEJOIN", 
#' "YEAR", "HAULJOIN", "STRATUM", "DESIGN_YEAR", "LATITUDE_DD_START",
#' "LATITUDE_DD_END","LONGITUDE_DD_START", "LONGITUDE_DD_END", "DEPTH_M",
#' "BOTTOM_TEMPERATURE_C", "SPECIES_CODE", "WEIGHT_KG", "COUNT", 
#' "AREA_SWEPT_KM2", "CPUE_KGKM2", "CPUE_NOKM2") ))
#' 
#' @export
#' 

calc_cpue <- function(gapdata = NULL,
                      racebase_tables = lifecycle::deprecated()) {
  
  ## Input check
  if (lifecycle::is_present(racebase_tables)) {
    lifecycle::deprecate_warn("2.2.0", 
                              "calc_cpue(racebase_tables)", 
                              "calc_cpue(gapdata)")
    gapdata <- racebase_tables
  }
  
  if (is.null(x = gapdata))
    stop(paste("Must provide argument `gapdata` a named list from",
               "gapindex::get_data()."))
  
  ## Extract objects from `gapdata`
  cruisedat <- gapdata$cruise 
  haul <- gapdata$haul
  catch <- gapdata$catch
  species <- gapdata$species
  
  ## For abundance index calculations, the haul df should only have records 
  ## where haul$ABUNDANCE_HAUL == "Y". This check is just a warning to the user.
  if (any(haul$ABUNDANCE_HAUL == "N")) {
    warning(paste("The `haul` dataframe in argument `gapdata`",
                  "contains records where haul$abundance_haul == 'N'.",
                  "Standard abundance index calculations only include records",
                  "where haul$abundance_haul == 'Y'. "))
  }
  
  ## Remove species with no catch records, output warning at the end
  omitted_species <- species$GROUP_CODE[!species$GROUP_CODE %in% 
                                          gapdata$catch$SPECIES_CODE]
  species <- subset(x = species,
                    subset = GROUP_CODE %in% unique(catch$SPECIES_CODE))
  
  ## Merge "SURVEY", "SURVEY_DEFINITION_ID", "DESIGN_YEAR", and "YEAR" columns 
  ## from `cruisedat` to `haul` using "CRUISEJOIN" as the key.
  dat <- merge(x = haul,
               y = cruisedat[, c("CRUISEJOIN", "SURVEY", 
                                 "SURVEY_DEFINITION_ID", 
                                 "DESIGN_YEAR", "YEAR")],
               by = "CRUISEJOIN")
  
  ##  `catch` only has non-zero records. To fill in zero-weight records, we 
  ## first create a table called `all_combos` of "HAULJOIN" and "SPECIES_CODE"
  all_combos <- 
    data.table::CJ(HAULJOIN = dat$HAULJOIN, 
                   SPECIES_CODE = sort(x = unique(x = catch$SPECIES_CODE)))
  
  ## Then left join the haul data in `dat` to `all_combos` using "HAULJOIN"
  ## as the key. This is probably a redundant step...
  dat <- merge(x = all_combos, 
               y = dat, 
               by = "HAULJOIN", 
               all.x = TRUE)
  
  ## Left Join `catch` with `dat` using the "HAULJOIN" AND "SPECIES_CODE" 
  ## as a composite key to zero-fill catch records for hauls did not encounter a 
  ## particular SPECIES_CODE. 
  dat <- merge(x = dat,
               y = catch, 
               by = c("HAULJOIN", "SPECIES_CODE"), 
               all.x = TRUE)
  
  ## There are some hauls where a weight is recorded, but not a count. 
  ## These records perhaps erroneously have a zero. These "zero" counts are
  ## not included in the numbers per area swept calculation, so we need to 
  ## NA these values.
  dat$NUMBER_FISH[dat$WEIGHT > 0 & dat$NUMBER_FISH == 0] <- NA
  
  ## Any record with no weight and count data (NA) are replaced with a zero
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
  
  ## Tuck the list of omitted species (no catch data) into a slot
  ## the attributes() of `dat`. 
  if (length(x = omitted_species) > 0) {
    attributes(x = dat)$omitted_species_codes <- omitted_species
    warning(paste("No catch records found for", length(x = omitted_species),
                  "SPECIES_CODE(s). The list of omitted species_codes",
                  "are tucked in the `omitted_species_codes` slot of the",
                  "attributes() of this object."))
  }
  
  return(dat)
}
