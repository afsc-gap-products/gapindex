# gapindex R Package 

This R package generates design-based indices of biomass, abundance, size 
composition, and age composition from NOAA-NMFS-AFSC-RACE-GAP bottom trawl 
survey data. Survey regions include: Gulf of Alaska (from 1984), 
Aleutian Islands (from 1980), Eastern Bering Sea Shelf (from 1982), 
Eastern Bering Sea Slope (from 2002), and Northern Bering Sea Shelf (from 2010).

The [gap_products](https://github.com/afsc-gap-products/gap_products) 
GitHub repository will store the code used to produce and 
test the standard data products for all regions each year calculated using the
gapindex R package. Rollout of this new workflow is currently slated for Fall 2024.

# Example Workflow

Make sure you have installed R packages devtools, RODBC, and getPass and are 
connected to the AFSC network when using this package.

```
## Install package
library(devtools)
devtools::install_github("afsc-gap-products/gapindex")
library(gapindex)

## Connect to Oracle
sql_channel <- gapindex::get_connected()

## Pull data.
gapindex_data <- gapindex::get_data(
  year_set = c(2007, 2009),
  survey_set = "GOA",
  spp_codes = 10261,   
  haul_type = 3,
  abundance_haul = "Y",
  pull_lengths = T,
  sql_channel = sql_channel)

## Fill in zeros and calculate CPUE
cpue <- gapindex::calc_cpue(racebase_tables = gapindex_data)

## Calculate stratum-level biomass, population abundance, mean CPUE and 
## associated variances
biomass_stratum <- gapindex::calc_biomass_stratum(
  racebase_tables = gapindex_data,
  cpue = cpue,
  vulnerability = 1)

## Calculate aggregated biomass and population abundance across subareas,
## management areas, and regions
biomass_subareas <- gapindex::calc_biomass_subarea(
  racebase_tables = gapindex_data,
  biomass_strata = biomass_stratum)

## Calculate size composition by stratum. See ?gapindex::calc_sizecomp_stratum
## for details on arguments
size_comp_stratum <- gapindex::calc_sizecomp_stratum(
  racebase_tables = gapindex_data,
  racebase_cpue = cpue,
  racebase_stratum_popn = biomass_stratum,
  spatial_level = "stratum",
  fill_NA_method = "AIGOA")

## Calculate aggregated size compositon across subareas, management areas, and
## regions
size_comp_subareas <- gapindex::calc_sizecomp_subareas(
  racebase_tables = gapindex_data,
  size_comps = size_comp_stratum)

## Calculate age-length key. See ?gapindex::calc_ALK for details on arguments
alk <- gapindex::calc_ALK(racebase_tables = gapindex_data, 
                          unsex = "all", 
                          global = F)

## Calculate age composition by stratum
age_comp_stratum <- gapindex::calc_agecomp_stratum(
  racebase_tables = gapindex_data, 
  alk = alk,
  size_comp = size_comp_stratum)

## Calculate aggregated age compositon across regions
age_comp_region <- gapindex::calc_agecomp_region(
  racebase_tables = gapindex_data, 
  age_comps_stratum = age_comp_stratum)

```

## In progress
* @MargaretSiple-NOAA created a [vignette](https://github.com/zoyafuso-NOAA/design-based-indices/tree/master/old_scripts/vignettes) on the biomass calculations using equations (from [Wakabayashi et al. 1985](https://drive.google.com/file/d/1m5c1N4WYysM1pscrpcgWOGZSZIK8vIHr/view?usp=sharing). We should make this into a wiki and have different pages for the different calculations (biomass, age compositions, etc.), assumptions, peculiarities, etc. 
* Increased documentation of function output similar to the metadata associated with GAP_PRODUCTS tables.
* Standardize data pulling process for ModSquad model-based index production
* Establish production cycle for standard gap products
* Formalize documentation of any changes that occur in the standard data products after each production cycle. 
* Create a Tech Memo that describes the changes in the Oracle data infrastructure
  in tandem with the creation of the gapindex package.

## Collaborators
The gapindex R package is a product of two AFSC-RACE-GAP working groups 
regarding GAP data processes and index computation. Many thanks to those who 
participated in those working groups:

**Data Processes Working Group**|**Index Computation Working Group**|**Supervisors **
:-----:|:-----:|:-----:
Alexandra Dowlin (AlexandraDowlin-NOAA)|Zack Oyafuso (zoyafuso-NOAA)*|Stan Kotwicki (StanKotwicki-NOAA)
Emily Markowitz (EmilyMarkowitz-NOAA)|Margaret Siple (MargaretSiple-NOAA)|Duane Stevenson (Duane-Stevenson-NOAA)
Liz Dawson (liz-dawson-NOAA)|Rebecca Haehn (RebeccaHaehn-NOAA)|Ned Laman (Ned-Laman-NOAA)
Sarah Friedman (SarahFriedman-NOAA)|Lukas DeFilippo (Lukas-DeFilippo-NOAA)| 
Christopher Anderson (ChrisAnderson-NOAA)|Paul von Szalay (vszalay)| 
Nancy Roberson (NancyRoberson)|Thaddaeus Buser (ThaddaeusBuser-NOAA)| 
 |*maintainer| 

## Legacy
*nanos gigantum humeris insidentes*: we stand on the shoulders of giants. 
Here is an non-exhaustive list of people who provided the foundation for many 
of the functions in this package:

AI-GOA: Michael Martin, 

Bering Sea: REM, Jason Conner, Jerry Hoff, 

Many of the index calculations are from Wakabayashi et al. (1985):

Wakabayashi, K., R. G. Bakkala, and M. S. Alton. 1985. Methods of the 
     U.S.-Japan demersal trawl surveys, p. 7-29. In R. G. Bakkala and K. 
     Wakabayashi (editors), Results of cooperative U.S.-Japan groundfish 
     investigations in the Bering Sea during May-August 1979. Int. North Pac. 
     Fish. Comm. Bull. 44.

## Acronymns
NOAA: National Oceanic and Atmospheric Administration

NMFS: National Marine Fisheries Service

AFSC: Alaska Fisheries Science Center

RACE: Resource Assessment and Conservation Engineering Division

GAP: Groundfish Assessment Program (GAP)

## Legal disclaimer
This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
