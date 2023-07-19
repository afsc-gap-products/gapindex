#' Calculate CPUEs at haul level
#' 
#' @description This function zero-fills the catch data and converts numbers 
#'              and weights to CPUE.
#' 
#' @param racebase_tables data object created from `gapindex::get_data()``
#' 
#' @return dataframe of weight and numerical CPUE for the region, species, and
#'         years pulled from `gapindex::get_data()`
#' 
#' | Field Name           | Description                                                                                                                                                                                                                                                                                                                    |
#' |----------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' | SURVEY_DEFINITION_ID | Integer number identifier corresponding to survey region. See   gapindex::survey_ids for a list of relevant survey regions.                                                                                                                                                                                                    |
#' | SURVEY               | Survey region.                                                                                                                                                                                                                                                                                                                 |
#' | CRUISE               | Six-digit integer number identifying the cruise number of the form:   YYYY99 (where YYYY = year of the cruise; 99 = 2-digit number and is   sequential; 01 denotes the first cruise that vessel made in this year, 02 is   the second, etc.).                                                                                  |
#' | YEAR                 | Survey year.                                                                                                                                                                                                                                                                                                                   |
#' | HAULJOIN             | Integer identifier assigned to each unique cruise, vessel, and haul   combination.                                                                                                                                                                                                                                             |
#' | STRATUM              | Stratum ID. STRATUM = 0 indicates an experimental tow.                                                                                                                                                                                                                                                                         |
#' | DESIGN_YEAR          | The year the survey design was created. For historical designs, this may   simply be the first year of the time series, e.g., 1984 for historical GOA   strata. In the Bering Sea, stratum boundaries are periodically updated to   incorporate more accurate bathymetric information or reflect modfied survey   footprints.  |
#' | LATITUDE_DD_START    | Latitude (one hundred thousandth of a decimal degree) of the start of the   trawl path.                                                                                                                                                                                                                                        |
#' | LATITUDE_DD_END      | Latitude (one hundred thousandth of a decimal degree) of the end of the   trawl path.                                                                                                                                                                                                                                          |
#' | LONGITUDE_DD_START   | Longitude (one hundred thousandth of a decimal degree) of the start of   the trawl path.                                                                                                                                                                                                                                       |
#' | LONGITUDE_DD_END     | Longitude (one hundred thousandth of a decimal degree) of the end of the   trawl path.                                                                                                                                                                                                                                         |
#' | SPECIES_CODE         | The taxon code of the organism associated with the 'common_name' and   'scientific_name' columns. For a complete species list, review the [code   books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).                                                        |
#' | WEIGHT_KG            | Total weight (kg) of individuals in a haul by taxon.                                                                                                                                                                                                                                                                           |
#' | COUNT                | Total number of individuals caught in the haul                                                                                                                                                                                                                                                                                 |
#' | AREA_SWEPT_KM2       | The total area the net sampled while the net was on-bottom (square km),   defined as the distance fished times the net width.                                                                                                                                                                                                  |
#' | CPUE_KGKM2           | Total catch weight (kg) per area swept (square km) by the trawl net.                                                                                                                                                                                                                                                           |
#' | CPUE_NOKM2           | Total catch number per area swept (square km) by the trawl net.                                                                                                                                                                                                                                                                |
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
  
  ## remove species with no catch records, output warning
  omitted_species <- 
    species$SPECIES_CODE[!species$SPECIES_CODE %in% 
                           racebase_tables$catch$SPECIES_CODE]
  species <- subset(x = racebase_tables$species,
                    subset = SPECIES_CODE %in% unique(catch$SPECIES_CODE))
  
  if (length(x = omitted_species) > 0) 
    warning(paste0("No catch records found for species codes: ", 
                   gapindex::stitch_entries(omitted_species)))
  
  ## Attach "YEAR" column to `haul` from `cruisedat` using "CRUISEJOIN" as the 
  ## key.
  haul <- merge(x = haul, 
                y = cruisedat[, c("CRUISEJOIN", "YEAR")],
                by = "CRUISEJOIN")
  
  ## For abundance index calculations, the haul df should only have records 
  ## where haul$ABUNDANCE_HAUL == "Y". This check is just a warning to the user.
  if (any(haul$ABUNDANCE_HAUL == "N")) {
    warning(paste0("`haul` dataframe in argument `racebase_tables`` contains ",
                   "records where haul$abundance_haul == 'N'. Standard ",
                   "abundance index calculations only include records where ",
                   "haul$abundance_haul == 'Y'. "))
  }
  
  ## Merge "SURVEY", "SURVEY_DEFINITION_ID", "DESIGN_YEAR" columns from 
  ## `cruisedat` to `haul` using "CRUISEJOIN" as the key.
  dat <- merge(x = haul,
               y = cruisedat[, c("CRUISEJOIN", "SURVEY", 
                                 "SURVEY_DEFINITION_ID", "DESIGN_YEAR")],
               by = "CRUISEJOIN")
  
  ##  `dat` only has non-zero records. To fill in zero-weight records, we 
  ## first create a table called `all_combos` of "HAULJOIN" and "SPECIES_CODE"
  all_combos <- expand.grid(HAULJOIN = dat$HAULJOIN, 
                            SPECIES_CODE = sort(unique(species$GROUP)))
  
  ## Then merge the haul data in `dat` to `all_combos` using "HAULJOIN"
  ## as the key.
  dat <- merge(x = all_combos, 
               y = dat, 
               by = "HAULJOIN", 
               all.x = TRUE)
  
  ## Merge `catch` with `dat` using the "HAULJOIN" AND "SPECIES_CODE" 
  ## as a composite key
  dat <- merge(x = dat,
               y = catch[, c("HAULJOIN", "SPECIES_CODE", 
                             "WEIGHT", "NUMBER_FISH")], 
               by = c("HAULJOIN", "SPECIES_CODE"), 
               all.x = TRUE)
  
  ## There are some hauls where a weight is recorded, but not a count. 
  ## These records perhaps erroneously have a zero. These "zero" counts are
  ## not included in the numbers per area swept calculation, so we need to 
  ## NA these values and put zero counts for hauls with zero weights
  dat$NUMBER_FISH[dat$NUMBER_FISH == 0] <- -1
  dat$NUMBER_FISH[is.na(dat$NUMBER_FISH)] <- 0  
  dat$NUMBER_FISH[dat$NUMBER_FISH == -1] <- NA
  
  sum(is.na(dat$NUMBER_FISH))
  
  ## Any record with no weight or count data (NA) are replaced with a zero
  dat$WEIGHT[is.na(dat$WEIGHT)] <- 0
  
  ## reorder columns, rename some
  dat <- with(dat,
              data.frame(SURVEY_DEFINITION_ID, SURVEY, CRUISE, CRUISEJOIN, YEAR, 
                         HAULJOIN, STRATUM, DESIGN_YEAR,
                         LATITUDE_DD_START = START_LATITUDE, 
                         LATITUDE_DD_END = END_LATITUDE, 
                         LONGITUDE_DD_START = START_LONGITUDE, 
                         LONGITUDE_DD_END = END_LONGITUDE,
                         SPECIES_CODE,
                         WEIGHT_KG = WEIGHT,
                         COUNT = NUMBER_FISH,
                         AREA_SWEPT_KM2 = DISTANCE_FISHED * (0.001 * NET_WIDTH)))
  
  ## CPUE calculations
  dat <- cbind(dat,
               with(dat, data.frame(CPUE_KGKM2 = WEIGHT_KG / AREA_SWEPT_KM2,
                                    CPUE_NOKM2 = COUNT / AREA_SWEPT_KM2)))
  
  return(dat)
}
