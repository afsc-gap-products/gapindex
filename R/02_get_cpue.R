# Task 1: replicate design-based total biomass per stratum that's in the SQL table
#
# Data and script locations -----------------------------------------------
# CPUE table to replicate
# SQL Developer: AFSC--> Other Users --> AI --> Tables --> CPUE

# SQL script to replicate
# G Drive: G:/GOA/biomass_sizecomp_scripts/biomass/biomass.sql

# Stratum areas to use
# SQL Developer: AFSC--> Other Users --> GOA --> Tables --> GOA_STRATA

# This function replicates the tables in RACEBASE GOA --> CPUE
get_cpue <- function(racebase_tables = list(
                       cruisedat = cruisedat,
                       haul = haul,
                       catch = catch
                     ),
                     speciescode = 30060, # POP
                     survey_area = "AI") {

  # survey_area is called region in RACEBASE
  cruisedat <- racebase_tables$cruisedat
  haul <- racebase_tables$haul
  catch <- racebase_tables$catch

  sp_catch <- catch %>%
    filter(species_code == speciescode)

  dat <- haul %>%
    left_join(cruisedat,
      by = c("cruisejoin", "region")
    ) %>%
    filter(abundance_haul == "Y" &
      region == survey_area) %>%
    left_join(sp_catch, by = "hauljoin") %>%
    replace_na(list(
      weight = 0,
      number_fish = 0
    )) %>%
    dplyr::select(
      species_code,
      cruisejoin.x, vessel.x, haul.x, hauljoin,
      haul_type, performance, duration,
      stratum, stationid,
      distance_fished, weight, year,
      weight, number_fish,
      start_latitude, start_longitude,
      gear_temperature, surface_temperature,
      AreaSwept_km2
    ) %>%
    dplyr::rename(
      Lat = start_latitude,
      Lon = start_longitude,
      catch_kg = weight,
      Vessel = vessel.x,
      Bottom_temp = gear_temperature,
      Surface_temp = surface_temperature
    )
  # %>%
  # filter(year == survey_yr)

  x <- dat %>%
    mutate(
      wgtcpue = catch_kg / AreaSwept_km2,
      numcpue = number_fish / AreaSwept_km2,
      survey = survey_area
    ) %>%
    replace_na(list(species_code = speciescode)) %>%
    select(
      year, survey, Vessel, haul.x, hauljoin,
      stationid,
      stratum, distance_fished,
      species_code, catch_kg, number_fish,
      AreaSwept_km2, wgtcpue, numcpue
    ) %>%
    rename(haul = haul.x) %>%
    arrange(year)

  return(x)
}

# POP: 30060
# walleye pollock: 21740
# sablefish: 20510

# RACEBASE equivalent table: CPUE
# x <- get_cpue(survey_area = "GOA", speciescode = 30060)

# profvis(get_cpue(survey_area = "GOA", speciescode = 30060))






