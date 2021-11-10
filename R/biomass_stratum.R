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
get_cpue <- function(racebase_tables = list(cruisedat = cruisedat, 
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
      NUMCPUE = number_fish / AreaSwept_km2,
      survey = survey_area
    ) %>%
    replace_na(list(species_code = speciescode)) %>%
    select(year, survey, Vessel, haul.x, stationid, 
           stratum, distance_fished, 
           species_code, Catch_KG, number_fish, 
           AreaSwept_km2, WGTCPUE, NUMCPUE)
#browser()
  return(x)
}

# POP: 30060
# walleye pollock: 21740
# sablefish: 20510

# RACEBASE equivalent table: CPUE
x <- get_cpue(survey_yr = 2021, survey_area = "GOA", speciescode = 30060)


# Total survey area
region_usr <- "GOA"
strata <- switch(region_usr,
                 "GOA" = goa_strata,
                 "AI" = ai_strata)
At <- sum(strata$area)

# Total CPUE for species, year, stratum
# no RACEBASE equivalent (building block of BIOMASS_STRATUM)
x2 <- x %>%
  group_by(year, stratum) %>%
  dplyr::summarize(
    haul_count = length(unique(stationid)), # number of total abundance hauls
    mean_wgt_cpue = mean(WGTCPUE),
    var_wgt_cpue = ifelse(haul_count<=1,NA,var(WGTCPUE)/haul_count),
    mean_num_cpue = mean(NUMCPUE),
    var_num_cpue = ifelse(haul_count<=1,NA,var(NUMCPUE)/haul_count),
    catch_count = length(which(Catch_KG>0)) # number of hauls with nonzero catch
    ) %>%
  dplyr::ungroup() %>%
  select(year, stratum, 
         haul_count, catch_count, 
         mean_wgt_cpue, var_wgt_cpue, 
         mean_num_cpue, var_num_cpue)

if(all(x2$catch_count<=x2$haul_count)){
  print("Number of hauls with positive catches is realistic.")}

vulnerability <- 1

# RACEBASE equivalent table: BIOMASS_STRATUM - checked, all good.
biomass_stratum <- x2 %>%
  dplyr::left_join(strata) %>%
  rowwise() %>% # for applying ifelse() by row
  mutate(stratum_biomass = area * mean_wgt_cpue / vulnerability * 0.001, #kg --> mt
         stratum_ratio = area / At,
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
select(survey, year, stratum, stratum_ratio, haul_count, catch_count, mean_wgt_cpue, var_wgt_cpue, mean_num_cpue, var_num_cpue, stratum_biomass, biomass_var, min_biomass, max_biomass, stratum_pop, pop_var, min_pop, max_pop, area) %>%
  mutate(Ni = area/0.01,
  fi = (Ni*(Ni-haul_count))/haul_count
         )


biomass_stratum_oracle <- read.csv("data/biomass_stratum_2021.csv") %>% 
  filter(YEAR==2021) %>%
  filter(SPECIES_CODE==30060) %>% 
  select(-SPECIES_CODE) %>% 
  janitor::clean_names() %>%
  arrange(stratum)

biomass_stratum_R <- biomass_stratum %>% 
  select(-area, -Ni, -fi, -stratum_ratio) %>% 
  mutate(stratum_pop = as.integer(stratum_pop),
         min_pop = as.integer(min_pop)) %>%
  as.data.frame()

head(biomass_stratum_R)
head(biomass_stratum_oracle)

# Differences between BIOMASS_STRATUM 
diffdf::diffdf(biomass_stratum_R, biomass_stratum_oracle)
# Difference between biomass_stratum produced by this code and the table in the GOA schema for 2021 for mean_wgt_cpue: 
biomass_stratum_R$mean_wgt_cpue - biomass_stratum_oracle$mean_wgt_cpue
######################### All good up to this point #######################
###########################################################################
###########################################################################
###########################################################################
# Test out biomass_stratum, check with oracle version ---------------------
pop_check <- read.csv("data/biomass_stratum_2021.csv") #GOA
test_stratum <- pop_check %>% 
  filter(YEAR == 2021 & SPECIES_CODE==30060) %>%
  left_join(strata, by= c("STRATUM" = "stratum")) %>%
  mutate(stratum_ratio = area/At)

test_total <- test_stratum %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarize(
    haul_count = sum(HAUL_COUNT),
    catch_count = sum(CATCH_COUNT),
    # The values below are not matching with the GOA schema
    mean_wgt_cpue = sum(MEAN_WGT_CPUE * area, na.rm = TRUE)/At, # weighted avg cpue across strata
    mean_num_cpue = sum(MEAN_NUM_CPUE * area, na.rm = TRUE)/At, 
    var_wgt_cpue = sum(stratum_ratio^2 * VAR_WGT_CPUE, na.rm = TRUE),
    var_num_cpue = sum(stratum_ratio^2 * VAR_NUM_CPUE , na.rm = TRUE)) %>%
  ungroup()

test_total
# OK it's definitely how I'm calculating it that's the issue, because the BIOMASS_STRATUM table behaves the same way when I do these calculations like this



# Check strata ------------------------------------------------------------
strata_sqldev <- read.csv("data/goastrata_test.csv")%>%filter(SURVEY=="GOA")
all_equal(goa_ai_strata %>% filter(SURVEY=="GOA"), strata_sqldev)
# interesting... stratum areas etc are all perfectly equal.









# Total CPUE for species and year (whole survey region)
# RACEBASE equivalent table: BIOMASS_TOTAL
# BIOMASS_TOTAL has SURVEY_AREA, YEAR, SPECIES CODE, HAUL_COUNT, CATCH_COUNT, MEAN_WGT_CPUE, VAR_WGT_CPUE, MEAN_NUM_CPUE, VAR_NUM_CPUE, TOTAL_BIOMASS, BIOMASS_VAR, MIN_BIOMASS, MAX_BIOMASS, TOTAL_POP, POP_VAR, MIN_POP, MAX_POP

biomass_total <- biomass_stratum %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(
    haul_count = sum(haul_count),
    catch_count = sum(catch_count),
    mean_wgt_cpue = sum(mean_wgt_cpue*area, na.rm = TRUE)/At, # weighted avg cpue across stata * total area
    mean_num_cpue = sum(mean_num_cpue*area, na.rm = TRUE)/At, # could this be weighted a different way??
    var_wgt_cpue = sum(stratum_ratio^2 * var_wgt_cpue, na.rm = TRUE),
    var_num_cpue = sum(stratum_ratio^2 * var_num_cpue, na.rm = TRUE),
    total_biomass = sum(stratum_biomass), #checked - ok
    biomass_var = sum(biomass_var), # checked - ok
    # min_biomass = ,
    # max_biomass = ,
    total_pop = sum(stratum_pop), #checked
    pop_var = sum(pop_var) #checked
   # min_pop = ,
   # max_pop = 
    #
    # same for total_population
    )


biomass_total




# Save/load stratum table to compare --------------------------------------
save(biomass_stratum, file = "outputs/biomass_stratum_mcs.Rda")

# Code scraps for checking
# biomass_stratum from GOA or AI schema:
biomass_stratum <- read.csv("data/biomass_stratum.csv") %>%
  janitor::clean_names() %>%
  filter(year == 2018 & species_code == 10120) %>%
  arrange(-desc(stratum))
head(biomass_stratum)

all_equal(biomass_stratum, x3)
all.equal(target = biomass_stratum, current = x3)

diffdf::diffdf(base = biomass_stratum, compare = x3)
