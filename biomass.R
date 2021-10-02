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
# This local folder contains csv files of all the RACEBASE tables.
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


# Get stratum areas from GOA_STRATA table----------------------------------
goa_ai_strata <- read.csv(here::here(
  "..", "Oracle connection", "data",
  "oracle", "goa", "goa_strata.csv"
))

goa_strata <- goa_ai_strata %>%
  janitor::clean_names() %>%
  filter(survey == "GOA")

ai_strata <- goa_ai_strata %>%
  janitor::clean_names() %>%
  filter(survey == "AI")

# Add area swept to each haul ---------------------------------------------
head(haul)
nrow(haul)
haul <- haul %>%
  mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width)) # bc distance in km and width in m
# This should match the EFFORT column in AI/CPUE

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


# This function replicates the tables in RACEBASE GOA --> CPUE
get_haul_cpue <- function(racebase_tables = list(cruisedat = cruisedat, 
                                                 haul = haul, 
                                                 catch = catch), 
                          speciescode = 30060, #POP
                          survey_area = "AI", 
                          survey_yr = 2018) {
  
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
      species_code,
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
      Catch_KG = weight,
      Vessel = vessel.x,
      Bottom_temp = gear_temperature,
      Surface_temp = surface_temperature
    ) %>%
    filter(year == survey_yr)

  x <- dat %>%
    mutate(
      WGTCPUE = Catch_KG / AreaSwept_km2,
      NUMCPUE = number_fish / AreaSwept_km2
    )
#browser()
  return(x)
}

# POP
# RACEBASE equivalent table: CPUE
x <- get_haul_cpue(survey_yr = 2019, survey_area = "GOA", speciescode = 30060)


# Total survey area
region_usr <- "GOA"
strata <- switch(region_usr,
                 "GOA" = goa_strata,
                 "AI" = ai_strata)
At <- sum(strata$area)

# Total CPUE for species, year, stratum
# RACEBASE equivalent table: BIOMASS_STRATUM
x2 <- x %>%
  group_by(year, stratum) %>%
  dplyr::summarize(
    haul_count = length(unique(stationid)), # number of total abundance hauls
    mean_wgt_cpue = mean(WGTCPUE),
    var_wgt_cpue = ifelse(haul_count<=1,0,var(WGTCPUE)/haul_count),
    mean_num_cpue = mean(NUMCPUE),
    var_num_cpue = ifelse(haul_count<=1,0,var(NUMCPUE)/haul_count),
    catch_count = length(which(number_fish>0)) # number of hauls with nonzero catch
  ) %>%
  dplyr::ungroup() %>%
  select(year, stratum, 
         haul_count, catch_count, 
         mean_wgt_cpue, var_wgt_cpue, 
         mean_num_cpue, var_num_cpue)

if(all(x2$catch_count<=x2$haul_count)){
  print("Number of hauls with positive catches is realistic.")}

vulnerability <- 1

# RACEBASE equivalent table: BIOMASS_STRATUM
x3 <- x2 %>%
  dplyr::left_join(strata) %>%
  rowwise() %>% # need this for applying ifelse by row
  mutate(stratum_biomass = area * mean_wgt_cpue / vulnerability * 0.001, #kg --> mt
         biomass_var = area^2 * var_wgt_cpue * 1e-6, #kg--> mt, square it because it's variance
         qt_size = ifelse(haul_count<= 1, 0, qt(p = 0.025, df = haul_count-1, lower.tail = F)),
         min_biomass = stratum_biomass - qt_size * sqrt(biomass_var),
         max_biomass = stratum_biomass + qt_size * sqrt(biomass_var),
         stratum_pop = area * mean_num_cpue,  # not sure why this calculation would be different from the stratum_biomass
         pop_var = area^2 * var_num_cpue,
         min_pop = stratum_pop - qt_size * sqrt(pop_var),
         max_pop = stratum_pop + qt_size * sqrt(pop_var)
         ) %>%
  mutate(min_biomass = ifelse(min_biomass<0, 0, min_biomass),
         min_pop = ifelse(min_pop<0, 0, min_pop)) %>% # set low CI to zero if it's negative
select(survey, year, stratum, haul_count, catch_count, mean_wgt_cpue, var_wgt_cpue, mean_num_cpue, var_num_cpue, stratum_biomass, biomass_var, min_biomass, max_biomass, stratum_pop, pop_var, min_pop, max_pop, area)

# Total CPUE for species and year (whole survey region)
# RACEBASE equivalent table: BIOMASS_TOTAL
# BIOMASS_TOTAL has SURVEY_AREA, YEAR, HAUL_COUNT, CATCH_COUNT, MEAN_WGT_CPUE, VAR_WGT_CPUE, MEAN_NUM_CPUE, VAR_NUM_CPUE, TOTAL_BIOMASS, BIOMASS_VAR, MIN_BIOMASS, MAX_BIOMASS, TOTAL_POP, POP_VAR, MIN_POP, MAX_POP
x4 <- x3 %>%
  dplyr::group_by(year, stratum) %>%
  dplyr::summarize(
    WGTCPUE = sum(mean_wgt_cpue * area), #total per stratum
    NUMCPUE = sum(mean_num_cpue * area),
    varWGTCPUE = (area / At)^2 * var_wgt_cpue,
    varNUMCPUE = (area / At)^2 * var_num_cpue,
    haul_count = sum(haul_count),
    catch_count = sum(catch_count)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(
    WGTCPUE_total = sum(WGTCPUE) / At,
    NUMCPUE_total = sum(NUMCPUE) / At,
    varWGTCPUE_total = sum(varWGTCPUE),
    varNUMCPUE_total = sum(varNUMCPUE)
    
  )

x4


# Save/load stratum table to compare --------------------------------------


save(x3, file = "outputs/biomass_stratum_mcs.Rda")

# biomass_stratum from AI schema:
biomass_stratum <- read.csv("data/biomass_stratum.csv") %>%
  janitor::clean_names() %>%
  filter(year == 2018 & species_code == 10120) %>%
  arrange(-desc(stratum))
head(biomass_stratum)

all_equal(biomass_stratum, x3)
all.equal(target = biomass_stratum, current = x3)

diffdf::diffdf(base = biomass_stratum, compare = x3)
