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
  
  ## Merge cruise data with haul data using the CRUISEJOIN and REGION as 
  ## keys. Have not tested with multiple regions. 
  dat <- merge(x = haul, 
               y = cruisedat, 
               by = c("CRUISEJOIN") )
  
  ##  `dat` only has non-zero records. To fill in zero-weight records, we 
  ## first create a table of all combos of HAULJOIN and SPECIES_CODE.
  all_combos <- expand.grid(HAULJOIN = dat$HAULJOIN, 
                            GROUP = sort(unique(species$GROUP)))
  
  ## Then merge the all_combos table with dat
  dat <- merge(x = all_combos, 
               y = dat, 
               by = c("HAULJOIN"), 
               all.x = TRUE)
  
  ## If you have more than one species, each species should now have the same
  ## number of hauls for each year (CRUISE) of data.
  # table(dat$CRUISE.x, dat$SPECIES_CODE)
  
  ## Merge catch data with dat using the HAULJOIN as the key
  dat <- merge(x = dat, 
               y = catch, 
               by = c("GROUP", "HAULJOIN"), 
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
              data.frame(YEAR = as.numeric(format(START_TIME, format = "%Y")),
                         CRUISEJOIN = CRUISEJOIN, HAULJOIN, VESSEL = VESSEL.x, 
                         HAUL = HAUL, HAUL_TYPE, PERFORMANCE, DURATION, STRATUM, 
                         STATIONID, 
                         START_LATITUDE, END_LATITUDE, 
                         START_LONGITUDE, END_LONGITUDE,
                         GEAR_DEPTH_M = GEAR_DEPTH, 
                         BOTTOM_DEPTH_M = BOTTOM_DEPTH, 
                         SURFACE_TEMPERATURE_C = SURFACE_TEMPERATURE, 
                         GEAR_TEMPERATURE_C = GEAR_TEMPERATURE, 
                         DISTANCE_FISHED_KM = DISTANCE_FISHED, 
                         NET_WIDTH_M = NET_WIDTH,
                         GROUP,
                         WEIGHT_KG = WEIGHT,
                         COUNT = NUMBER_FISH,
                         AREASWEPT_KM2 = DISTANCE_FISHED * (0.001 * NET_WIDTH)))
  
  ## CPUE calculations
  dat <- 
    cbind(dat,
          with(dat, data.frame(WGTCPUE_KG_KM2 = WEIGHT_KG / AREASWEPT_KM2,
                               NUMCPUE_COUNT_KM2 = COUNT / AREASWEPT_KM2)))
  
  return(dat)
}
