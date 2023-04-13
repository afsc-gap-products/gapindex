# gapindex R Package 

Code to generate design-based indices of biomass, abundance, size composition, and age composition from survey data. The code takes haul and catch tables from RACEBASE to estimate biomass for (currently) the GOA, AI, EBS Shelf, and NBS regions. @MargaretSiple-NOAA created a [vignette](https://github.com/zoyafuso-NOAA/design-based-indices/tree/master/old_scripts/vignettes) on the biomass calculations using equations (from [Wakabayashi et al. 1985](https://drive.google.com/file/d/1m5c1N4WYysM1pscrpcgWOGZSZIK8vIHr/view?usp=sharing). We should make this into a wiki and have different pages for the different calculations (biomass, age compositions, etc.) along with other relevant information. 

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
devtools::install_github("afsc-gap-products/gapindex@development")

library(gapindex)

## Connect to Oracle. See ?gapindex::get_connected first for more details
sql_channel <- gapindex::get_connected()

## Pull data.  See ?gapindex::get_data first for more details
cod_data <- gapindex::get_data(year_set = 2021,
                               survey_set = "EBS",
                               spp_codes = data.frame(GROUP = 21720, 
                                                      SPECIES_CODE = 21720),   
                               haul_type = 3,
                               abundance_haul = "Y",
                               pull_lengths = TRUE,
                               sql_channel = sql_channel)

## Fill in zeros and calculate CPUE. See ?gapindex::calc_cpue first for more details
cpue <- gapindex::calc_cpue(racebase_tables = cod_data)

## Calculate biomass, population abundance, and CIs for each strata. See ?gapindex::calc_biomass_stratum first for more details
biomass_stratum <- 
  gapindex::calc_biomass_stratum(racebase_tables = cod_data,
                                 cpue = cpue,
                                 vulnerability = 1)

## Calculate aggregated biomass and population abundance across subareas and region (STRATUM 999). See ?gapindex::calc_agg_biomass first for more details
biomass_subareas <- 
  gapindex::calc_agg_biomass(racebase_tables = cod_data,
                             biomass_strata = biomass_stratum)


## Calculate size composition by stratum. See ?gapindex::calc_size_stratum_BS first for more details. Note if you are calculating size comps for the AI/GOA regions, you should be using gapindex::calc_size_stratum_AIGOA instead. 
size_comp <- gapindex::calc_size_stratum_BS(
  racebase_tables = cod_data,
  racebase_cpue = cpue,
  racebase_stratum_popn = biomass_stratum)

## Calculate age composition for region. See ?gapindex::calc_age_comp first for more details. Note
age_comp <- gapindex::calc_age_comp(racebase_tables = cod_data, 
                                    size_comp = size_comp)

```

## Legal disclaimer
This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
