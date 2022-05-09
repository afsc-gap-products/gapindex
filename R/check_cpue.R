# check_cpue
# Check outputs from this function against tables in RACEBASE (from Wayne)
source("R/01_cleanup_data.R")
source("R/02_get_cpue.R")
source("R/03_get_biomass_stratum.R")


# Get tables to compare ---------------------------------------------------
# GOA.CPUE table downloaded from RACEBASE
cpue_wayne <- read.csv(here::here("data", "cpue_racebase.csv"))
cpue_wayne <- cpue_wayne %>% janitor::clean_names()

# Emily's cpue summarized by year and station (for GAP public data)
# source: https://github.com/afsc-gap-products/gap_public_data/blob/main/code/run.R
cpue_em <- read.csv("C:/Users/margaret.siple/Work/gap_public_data/output/2022-05-04/cpue_station.csv")


# check cpue for a single species -----------------------------------------
# Not sure how to do this, try one species/year combo first:
region_compare <- "GOA"
sp_compare <- 30060

cpue_dbe <- get_cpue(speciescode = sp_compare, survey_area = "GOA") %>%
  filter(year == year_compare)

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
  dplyr::left_join(cpue_e, by = c("hauljoin", "cruise", "stratum", "haul", "vessel" = "vessel_id")) %>%
  select(survey_id, year, region,cruise, haul, haul_type, srvy,cpue_kgkm2, hauljoin ) %>%
  mutate(cpue_kgkm2 = replace_na(cpue_kgkm2, 0))
head(cpue_e_0filled)
nrow(cpue_e_0filled)
head(cpue_w)
# now it has the same nrows as cpue_w and cpue_dbe

compare.df.em <- cpue_w %>%
  rename(cpue_kgkm2=wgtcpue) %>%
  full_join(cpue_e_0filled, by = c("hauljoin","haul"), # c("year", "hauljoin","survey"="srvy")
            suffix = c(".racebase", ".emily")) %>%
  select(order(colnames(.)))

nrow(compare.df.em)
head(compare.df.em)

# check that the column order is right
all(compare.df.em$distance_fished.emily == compare.df.em$distance_fished.racebase)

# Look at where CPUEs are different
all(compare.df.em$cpue_kgkm2.emily == compare.df.em$cpue_kgkm2.racebase)
(cpuediffs <- compare.df.em$cpue_kgkm2.emily - compare.df.em$cpue_kgkm2.racebase)

# For POP, the max difference is
max(cpuediffs)


# All years and SAFE species ----------------------------------------------
# Set up design table for GOA
safe_species <- read.csv(here::here("data","siple_safe_species.csv"))
goa_safe <- safe_species %>% filter(GOA==1 & !is.na(species_code)) %>% select(species_code)
designtable <- tibble(goa_safe,max_cpue_diff = 999)
