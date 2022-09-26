# check_cpue
# Check outputs from this function against tables in RACEBASE (from Wayne)
source("R/01_cleanup_data.R")
source("R/02_get_cpue.R")
source("R/03_get_biomass_stratum.R")


# Get tables to compare ---------------------------------------------------

# GOA.CPUE table downloaded from RACEBASE
cpue_wayne <- read.csv(here::here("data", "cpue_racebase.csv"))
cpue_wayne <- cpue_wayne %>% janitor::clean_names()
#
# EMILY SOURCE FOR ZERO-FILLED CPUE TALE IS PAUSED AT THE MOMENT BECAUSE SHE HAS TO SEND ME THE TABLE
# # Emily's cpue summarized by year and station (for GAP public data)
# # source: FOSS
# library("jsonlite")
# # link to the API
# api_link <- "https://origin-tst-ods-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey/"
# res <- httr::GET(url = api_link)
# # base::rawToChar(res$content) # Test connection
# data <- jsonlite::fromJSON(base::rawToChar(res$content))
# head(data$items)
#
# old/deprecated
cpue_em <- read.csv("C:/Users/margaret.siple/Work/gap_public_data/output/2022-09-15/cpue_station.csv")
cpue_em <- cpue_em %>% janitor::clean_names()

# check cpue for a single species -----------------------------------------
# Not sure how to do this, try one species/year combo first:
region_compare <- "GOA"
sp_compare <- 30060
year_compare <- 2021

cpue_dbe <- get_cpue(speciescode = sp_compare, survey_area = region_compare) %>%
  filter(year == year_compare)

cpue_w <- cpue_wayne %>%
  filter(species_code == sp_compare & survey == region_compare & year == year_compare)

cpue_e <- cpue_em %>%
  filter(species_code == sp_compare & srvy == region_compare & year == year_compare)

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
all(compare.df$wgtcpue.dbe == compare.df$wgtcpue.racebase)
compare.df$wgtcpue.dbe - compare.df$wgtcpue.racebase

# For POP, the max difference is
max(compare.df$wgtcpue.dbe - compare.df$wgtcpue.racebase)

# So I think it's ok.


# Compare with CPUE from Emily table --------------------------------------
cpue_e_0filled <- haul %>%
  dplyr::filter(year(start_time) == year_compare & region == "GOA" & abundance_haul == "Y") %>%
  dplyr::left_join(cpue_e, by = c("hauljoin", "cruise", "stratum", "haul", "vessel" = "vessel_id")) %>%
  select(survey_id, year, region, cruise, haul, haul_type, srvy, cpue_kgkm2, hauljoin) %>%
  mutate(cpue_kgkm2 = replace_na(cpue_kgkm2, 0))
head(cpue_e_0filled)
nrow(cpue_e_0filled)
head(cpue_w)
# now it has the same nrows as cpue_w and cpue_dbe

compare.df.em <- cpue_w %>%
  rename(cpue_kgkm2 = wgtcpue) %>%
  full_join(cpue_e_0filled,
    by = c("hauljoin", "haul"), # c("year", "hauljoin","survey"="srvy")
    suffix = c(".racebase", ".emily")
  ) %>%
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
safe_species <- read.csv(here::here("data", "siple_safe_species.csv"))

goa_safe <- safe_species %>%
  filter(GOA == 1 & !is.na(species_code)) %>%
  select(species_code)

designtable <- expand_grid(
  species_code = goa_safe$species_code,
  year = unique(cpue_wayne$year)
) %>%
  mutate(max_cpue_diff = NA) 

region_compare <- "GOA"

for(i in 1:nrow(designtable)){ #
  sp_compare <- designtable$species_code[i]
  year_compare <- designtable$year[i]
  
  cpue_dbe <- get_cpue(speciescode = sp_compare, survey_area = region_compare) %>%
    filter(year == year_compare)
  
  cpue_w <- cpue_wayne %>%
    filter(species_code == sp_compare & survey == region_compare & year == year_compare)
  
  compare.df <- cpue_w %>%
    full_join(cpue_dbe, by = c("year", "survey", "hauljoin"), suffix = c(".racebase", ".dbe")) %>%
    select(order(colnames(.)))
  
  
  designtable$max_cpue_diff[i] <- max(compare.df$wgtcpue.dbe - compare.df$wgtcpue.racebase)
  print(i/nrow(designtable))
}

designtable2 <- designtable %>% 
  arrange(-max_cpue_diff)


# Do the same thing for the AI --------------------------------------------
# AI.CPUE table downloaded from RACEBASE
cpue_wayne_ai <- read.csv(here::here("data", "cpue_racebase_ai.csv"))
cpue_wayne_ai <- cpue_wayne_ai %>% janitor::clean_names()
