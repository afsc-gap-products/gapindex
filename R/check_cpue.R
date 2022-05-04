# check_cpue
# Check outputs from this function against tables in RACEBASE (from Wayne)
source("R/01_cleanup_data.R")
source("R/02_get_cpue.R")
source("R/03_get_biomass_stratum.R")

# Check for:
# GOA POP
# GOA walleye pollock
year_compare <- 2005
region_compare <- "GOA"
sp_compare <- 30060

cpue_dbe <- get_cpue(speciescode = sp_compare, survey_area = "GOA") %>%
  filter(year == year_compare)

# GOA.CPUE table downloaded from RACEBASE
cpue_wayne <- read.csv(here::here("data", "cpue_racebase.csv"))
cpue_wayne <- cpue_wayne %>% janitor::clean_names()
# Emily's cpue summarized by year and station (for GAP public data)
cpue_em <- read.csv("C:/Users/margaret.siple/Work/gap_public_data/output/2022-05-04/cpue_station.csv")

# Not sure how to do this, try one species/year combo first:
cpue_w <- cpue_wayne %>%
  filter(species_code == sp_compare & survey == "GOA" & year == year_compare)

cpue_e <- cpue_em %>%
  filter(species_code == sp_compare & srvy == "GOA" & year == year_compare)

nrow(cpue_dbe)
nrow(cpue_w)
nrow(cpue_e) # Em's table is not zero-filled so need to add in zeroes for stations where no POP (or whatever species) were caught.

# First, compare CPUE table from RACEBASE to mine:
length(unique(cpue_w$haul))
length(unique(cpue_dbe$haul))
length(unique(cpue_e$haul))

colnames(cpue_w)
colnames(cpue_dbe)

compare.df <- cpue_w %>%
  full_join(cpue_dbe, by = c("year", "survey", "hauljoin"), suffix = c(".racebase", ".dbe")) %>%
  select(order(colnames(.)))

# check that the column order is right
all(compare.df$distance_fished.dbe == compare.df$distance_fished.racebase)

# Look at where CPUEs are different
all(compare.df$wgtcpue.dbe == compare.df$distance_fished.racebase)
compare.df$wgtcpue.dbe - compare.df$wgtcpue.racebase

# For POP, the max difference is
max(compare.df$wgtcpue.dbe - compare.df$wgtcpue.racebase)

# So I think it's ok.


# Compare with CPUE from Emily table --------------------------------------
cpue_e_0filled <- haul %>%
  dplyr::filter(year(start_time) == year_compare & region == "GOA" & abundance_haul == "Y") %>%
  dplyr::left_join(cpue_e, by = c("hauljoin", "cruise", "stratum", "haul", "vessel" = "vessel_id"))
head(cpue_e_0filled)
nrow(cpue_e_0filled)
# now it has the same nrows as cpue_w and cpue_dbe


