#Check biomass_stratum from table against values in RACEBASE (from Wayne)
source("R/01_cleanup_data.R")
source("R/02_get_cpue.R")
source("R/03_get_biomass_stratum.R")

# Wayne's tables from Oracle - source AFSC//Other Users//GOA//BIOMASS_STRATUM
wayne <- read.csv("data/biomass_stratum_2021.csv")

pop_dbe <- get_biomass_stratum(speciescode = 30060,survey_area = "GOA")
pop_palsson <- wayne %>% filter(SPECIES_CODE==30060) %>% arrange(YEAR,STRATUM)

nrow(pop_dbe)
nrow(pop_palsson)

head(pop_dbe)
head(pop_palsson)




# haul counts are off! Palsson counts always larger than my counts