# gapindex R Package 

Code to generate design-based indices of biomass, abundance, size composition, and age composition from survey data. The code takes haul and catch tables from RACEBASE to estimate biomass for (currently) the GOA, AI, EBS Shelf, and NBS Shelf regions. @MargaretSiple-NOAA created a [vignette](https://github.com/zoyafuso-NOAA/design-based-indices/tree/master/old_scripts/vignettes) on the biomass calculations using equations (from [Wakabayashi et al. 1985](https://drive.google.com/file/d/1m5c1N4WYysM1pscrpcgWOGZSZIK8vIHr/view?usp=sharing). We should make this into a wiki and have different pages for the different calculations (biomass, age compositions, etc.) along with other relevant information. 

## Authors

Primary contacts: @zoyafuso-NOAA, @MargaretSiple-NOAA

Groundfish Assessment Program
Resource Assessment and Conservation Engineering Division
Alaska Fisheries Science Center
National Marine Fisheries Service
National Oceanic and Atmospheric Administration

# Initial Testing

Make sure you have installed R packages devtools, RODBC, and getPass and are connected to the VPN when using this package.

```
library(devtools)
devtools::install_github("afsc-gap-products/gapindex")

library(gapindex)

## Connect to Oracle. See ?gapindex::get_connected first for more details
sql_channel <- gapindex::get_connected()

## Pull data.  See ?gapindex::get_data first for more details
cod_data <- gapindex::get_data(year_set = 2021,
                               survey_set = "EBS", #Eastern Bering Sea
                               spp_codes = 21720, #Pacific cod
                               haul_type = 3,
                               abundance_haul = "Y",
                               pull_lengths = TRUE,
                               sql_channel = sql_channel)

## Calculate and zero-fill catch-per-unit-effort (CPUE) by haul. See ?gapindex::calc_cpue first for more details
cpue <- gapindex::calc_cpue(racebase_tables = cod_data)

## Calculate biomass, population abundance, and mean CPUE by stratum. See ?gapindex::calc_biomass_stratum first for more details
biomass_stratum <- 
  gapindex::calc_biomass_stratum(racebase_tables = cod_data,
                                 cpue = cpue,
                                 vulnerability = 1)

## Aggregated biomass and population abundance by subarea and region. See ?gapindex::calc_biomass_subarea first for more details
biomass_subareas <- 
  gapindex::calc_biomass_subarea(racebase_tables = cod_data,
                                 biomass_strata = biomass_stratum)

## Calculate size composition by stratum. See ?gapindex::calc_sizecomp_bs_stratum first for more details. Note if you are calculating size comps for the Aleutian Islands of Gulf of Alaska regions, you should be using gapindex::calc_sizecomp_aigoa_stratum instead. gapindex::calc_sizecomp_bs_stratum is used for calculating size composition for the Eastern and Northern Bering Sea regions. 

sizecomp_stratum <- gapindex::calc_sizecomp_bs_stratum(
  racebase_tables = cod_data,
  racebase_cpue = cpue,
  racebase_stratum_popn = biomass_stratum)
  
## Aggregate size composition by region. See ?gapindex::calc_sizecomp_subarea first for more details. 
size_comp_subarea <- gapindex::calc_sizecomp_subarea(
  racebase_tables = cod_data, 
  size_comps = sizecomp_stratum
)

## Calculate age composition by stratum. See ?gapindex::calc_agecomp_stratum first for more details. 
agecomp_stratum <- gapindex::calc_agecomp_stratum(
  racebase_tables = cod_data,
  size_comp = sizecomp_stratum)

## Aggregate age composition by region. See ?gapindex::calc_agecomp_region first for more details. 
age_comp_region <- 
  gapindex::calc_agecomp_region(racebase_tables = cod_data,
                                age_comps_stratum = agecomp_stratum)
```

## Legal disclaimer
This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
