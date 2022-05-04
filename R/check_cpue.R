# check_cpue
# Check outputs from this function against tables in RACEBASE (from Wayne)
source("R/01_cleanup_data.R")
source("R/02_get_cpue.R")
source("R/03_get_biomass_stratum.R")

# Check for GOA POP 
cpue_dbe <- get_cpue(speciescode = 30060,survey_area = "GOA") %>% filter(year==2005)

# GOA.CPUE table downloaded from RACEBASE
cpue_wayne <- read.csv(here::here("data","cpue_racebase.csv"))
cpue_wayne <- cpue_wayne %>% janitor::clean_names()
# Emily's cpue summarized by year and station (for GAP public data)
cpue_em <- read.csv("C:/Users/margaret.siple/Work/gap_public_data/output/2022-05-04/cpue_station.csv")

# Not sure how to do this, try one species/year combo first:
cpue_w <- cpue_wayne %>% 
  filter(species_code==30060 & survey=="GOA" & year==2005)

cpue_e <- cpue_em %>% 
  filter(species_code==30060 & srvy=="GOA" & year==2005) 

nrow(cpue_dbe)
nrow(cpue_w)
nrow(cpue_e) # Em's table is not zero-filled so need to add in zeroes for stations where no POP (or whatever species) were caught.


# First, compare CPUE table from RACEBASE to mine:
length(unique(cpue_w$haul))
length(unique(cpue_dbe$haul))
colnames(cpue_w)
colnames(cpue_dbe)

compare.df <- cpue_w %>% 
  full_join(cpue_dbe, by=c("year","survey","hauljoin"),suffix = c(".racebase", ".dbe") ) %>%
  select(order(colnames(.)))
