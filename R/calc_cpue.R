#' Calculate CPUEs at haul level
#' 
#' @description This function zero-fills the catch data and converts numbers 
#'              and weights to CPUE.
#' 
#' @param racebase_tables data object created from `AFSC.GAP.DBE::get_data()``
#' 
#' @return dataframe of weight and numerical CPUE for the region, species, and
#'         years pulled from `AFSC.GAP.DBE::get_data()`
#' 
#' @export
#' 

calc_cpue <- function(racebase_tables = NULL) {
  
  cruisedat <- racebase_tables$cruise 
  haul <- racebase_tables$haul
  ## Attach year to haul
  haul <- merge(x = haul, 
                y = cruisedat[, c("CRUISEJOIN", "YEAR")],
                by = "CRUISEJOIN")
  catch <- racebase_tables$catch
  species <- racebase_tables$species
  
  ## For abundance index calculations, the haul df should only have records 
  ## where haul$ABUNDANCE_HAUL==Y. This check is just a warning to the user.
  if (any(haul$ABUNDANCE_HAUL == "N")) {
    warning(paste0("`haul` dataframe in argument `racebase_tables`` contains ",
                   "records where haul$abundance_haul == 'N'. Standard ",
                   "abundance index calculations only include records where ",
                   "haul$abundance_haul == 'Y'. "))
  }
  
  ## Merge cruise data with haul data using the CRUISEJOIN as the key.
  dat <- merge(x = haul,
               y = cruisedat[, c("CRUISEJOIN", "SURVEY", 
                                 "SURVEY_DEFINITION_ID", "DESIGN_YEAR")],
               by = "CRUISEJOIN" )
  
  ##  `dat` only has non-zero records. To fill in zero-weight records, we 
  ## first create a table of all combos of HAULJOIN and SPECIES_CODE.
  all_combos <- expand.grid(HAULJOIN = dat$HAULJOIN, 
                            SPECIES_CODE = sort(unique(species$GROUP)))
  
  ## Then merge the all_combos table with dat
  dat <- merge(x = all_combos, 
               y = dat, 
               by = c("HAULJOIN"), 
               all.x = TRUE)
  
  ## Merge catch data with dat using the HAULJOIN AND SPECIES_CODE as the key
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
              data.frame(CRUISE, CRUISEJOIN, HAULJOIN, DESIGN_YEAR, 
                         SURVEY, SURVEY_DEFINITION_ID, YEAR, STRATUM, 
                         START_LATITUDE, END_LATITUDE, 
                         START_LONGITUDE, END_LONGITUDE,
                         SPECIES_CODE,
                         WEIGHT_KG = WEIGHT,
                         COUNT = NUMBER_FISH,
                         AREASWEPT_KM2 = DISTANCE_FISHED * (0.001 * NET_WIDTH)))
  
  ## CPUE calculations
  dat <- cbind(dat,
               with(dat, data.frame(CPUE_KGKM2 = WEIGHT_KG / AREASWEPT_KM2,
                                    CPUE_NOKM2 = COUNT / AREASWEPT_KM2)))
  
  return(dat)
}
