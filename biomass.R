# Task 1: replicate design-based total biomass per stratum that's in the SQL table
#
# Data and script locations -----------------------------------------------
# CPUE table to replicate
# SQL Developer: AFSC--> Other Users --> AI --> Tables --> CPUE

# SQL script to replicate
# G Drive: G:/GOA/biomass_sizecomp_scripts/biomass/biomass.sql

# Stratum areas to use
# SQL Developer: AFSC--> Other Users --> GOA --> Tables --> GOA_STRATA


# Step 1: Get RACEBASE data -----------------------------------------------
# Where to put the cleaned/wrangled ---------------------------------------
#data_destination <- "C:/Users/margaret.siple/Work/design-based-indices/data"

library(tidyverse)

# Load Oracle data (takes a while) ----------------------------------------
# This is in my general Oracle access project. csv files of all the RACEBASE tables.
a <- list.files(
  path = here::here("..", "Oracle connection", "data", "oracle", "racebase"),
  pattern = "\\.csv"
)

for (i in 1:length(a)) {
  b <- read.csv(file = paste0(here::here(
    "..", "Oracle connection",
    "data", "oracle", "racebase", a[i]
  )))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = a[i]), value = b)
}


# Get stratum areas from GOA_STRATA table ---------------------------------
goa_strata <- read.csv(here::here(
  "..", "Oracle connection", "data",
  "oracle", "goa", "goa_strata.csv"
))
ai_strata <- goa_strata %>%
  janitor::clean_names() %>%
  filter(survey == "AI")

# Add area swept to each haul ---------------------------------------------
head(haul)
nrow(haul)
haul <- haul %>%
  mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width)) # bc distance in km and width in m
# This should match the EFFORT column in AI-->CPUE

# Get cruise info from RACE.DATA ------------------------------------------
# AFAIK you can only get the name of the survey from the cruises.csv file, which is from RACEDATA
cruises <- read.csv(here::here(
  "..", "Oracle connection",
  "data", "oracle",
  "race_data",
  "cruises.csv"
)) %>%
  janitor::clean_names()

cruisedat <- cruises %>%
  dplyr::select(year, survey_name, region, 
         cruisejoin, cruise_data_in_racebase, 
         vessel_country, survey_definition_id)


get_haul_cpue <- function(racebase_tables = list(cruisedat = cruisedat, 
                                                 haul = haul, 
                                                 catch = catch), 
                          speciescode = 30060, 
                          survey_area = "AI", 
                          survey_yr = 2018) {
  # test species: POP
  #survey_area is called region in RACEBASE
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
      cruisejoin.x, vessel.x, haul.x,
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
      Year = year,
      Catch_KG = weight,
      Vessel = vessel.x,
      Bottom_temp = gear_temperature,
      Surface_temp = surface_temperature
    ) %>%
    filter(Year == survey_yr)

  x <- dat %>%
    mutate(
      wCPUE = Catch_KG / AreaSwept_km2,
      nCPUE = number_fish / AreaSwept_km2
    )

  return(x)
}

# POP
get_haul_cpue()

# walleye pollock
tail(get_haul_cpue(speciescode = 21740))
#x <- get_haul_cpue(speciescode = 21740, survey_yr = 1994)

# Pacific halibut
head(get_haul_cpue(speciescode = 10120))
x <- get_haul_cpue(speciescode = 10120)

# Giant grenadier
# x <- get_haul_cpue(speciescode = 21230)
# head(x)
# tail(x)


#x <- get_haul_cpue(speciescode = 69310)

# Total survey area
At <- sum(ai_strata$area)

# Total CPUE for species, year, stratum
x2 <- x %>%
  group_by(Year, stratum) %>%
  dplyr::summarize(
    haul_count = length(haul.x),
    mean_wgt_cpue = mean(wCPUE),
    var_wgt_cpue = var(wCPUE)/length(wCPUE),
    mean_num_cpue = mean(nCPUE),
    var_num_cpue = var(nCPUE)/length(nCPUE),
    nstations = length(unique(stationid))
  ) %>%
  dplyr::ungroup()

vulnerability <- 1

x3 <- x2 %>%
  dplyr::left_join(ai_strata) %>%
  mutate(stratum_biomass = area * mean_wgt_cpue / vulnerability * 0.001, #kg --> mt
         biomass_var = area^2 * var_wgt_cpue * 1e-6, #kg--> mt, square it because it's variance
         min_biomass = stratum_biomass - qt(0.025, df = nstations-1, lower.tail = F) * sqrt(biomass_var),
         max_biomass = stratum_biomass + qt(0.025, df = nstations-1, lower.tail = F) * sqrt(biomass_var),
         stratum_pop = area * mean_num_cpue,  # not sure why this calculation would be different from the stratum_biomass
         pop_var = area^2 * var_num_cpue,
         min_pop = stratum_pop - qt(0.025, df = nstations-1, lower.tail = F) * sqrt(pop_var),
         max_pop = stratum_pop + qt(0.025, df = nstations-1, lower.tail = F) * sqrt(pop_var)
         ) %>%
  mutate(min_biomass = ifelse(min_biomass<0, 0, min_biomass),
         min_pop = ifelse(min_pop<0, 0, min_pop)) # set low CI to zero if it's negative

# Total CPUE for species and year (whole AI)
x4 <- x3 %>%
  dplyr::group_by(Year, stratum) %>%
  dplyr::summarize(
    wCPUE = sum(mean_wgt_cpue * area),
    nCPUE = sum(mean_num_cpue * area),
    varwCPUE = (area / At)^2 * var_wgt_cpue,
    varnCPUE = (area / At)^2 * var_num_cpue
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Year) %>%
  dplyr::summarize(
    wCPUE_total = sum(wCPUE) / At,
    nCPUE_total = sum(nCPUE) / At,
    varwCPUE_total = sum(varwCPUE),
    varnCPUE_total = sum(varnCPUE)
  )

x4

