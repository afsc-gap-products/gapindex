# design-based-indices (in progress)

Code to generate design-based indices of abundance from survey data. The code takes haul and catch tables from RACEBASE to calculate CPUE by region and species. The goal of this code is to reproduce the tables found in the GOA and AI schemas: `BIOMASS_STRATUM`, `BIOMASS_TOTAL`, and `CPUE` (later we will add `AGECOMP_TOTAL`, `SIZE_COMP_TOTAL`, `SIZECOMP_TOTAL`. I (Megsie) am currently testing this code with those two regions in mind; the repo may be modified in the future to also include Bering Sea functions, OR Bering Sea indices may become their own repository.

Equations (from [Wakabayashi et al. 1985](https://drive.google.com/file/d/1m5c1N4WYysM1pscrpcgWOGZSZIK8vIHr/view?usp=sharing), modified / adapted) can be found under [vignettes](https://github.com/afsc-gap-products/design-based-indices/tree/master/vignettes).

The goal of this code is to reproduce the tables

## Scripts

| Script                         | Description                                     | Equation number(s) in Wakabayashi et al. |
|--------------------------------|-------------------------------------------------|------------------------------------------|
| `00_download_data_from_oracle.R` | Download raw tables from Oracle into data/      | --                                       |
| `01_cleanup_data.R`              | Cleanup up column names (snake case)            | --                                       |
| `02_get_cpue.R`                  | Calculate haul CPUE (numerical and weight CPUE) | 1       |
| `03_get_biomass_stratum.R`       | Get stratum level estimates of mean and variance of biomass      | 2, 3, 6, 7, 8          |
| `04_get_biomass_total.R`         | Get mean and variance of CPUE and total biomass for the whole survey area            | 4, 5, 9, 10, 11, 12                         |

## Workflow

To use this code, clone the repo and run the `run.R` script. 

The workflow for this set of functions has two main components:

**1. Local copies of data tables.** These scripts work off a local copy of the RACEBASE tables. The `00_download_data_from_oracle.R` script will download these tables and save them locally for you the first time you run the code (or whenever you want to update -- note that this requires you to add a `setup_channel.R` script that contains your Oracle info). If you do not have access to the RACEBASE tables in Oracle, we can help you get the tables you need.

Before you run summary functions (like `get_biomass_stratum()`), the `01_cleanup_data.R` script cleans up the data and formats it for the other functions.

**2. Summary functions.** The other functions contained in this repo will summarize survey data from RACEBASE as needed. Function names are based on the tables as they appear in the GOA or AI schemas. For example, `get_cpue()` produces the CPUE table from the GOA or AI schema.

## Authors

Primary contact: @MargaretSiple-NOAA

Contributors: @SarahFriedman-NOAA, ...

## Questions for the future of GAP index products

1. Do we want, in the future, to provide stock assessment authors with CPUE tables only? Or do we want to continue providing biomass indices.
2. All three regions have different methods for producing size and maybe age comps -- is it worth including scripts for three different methods? Different versions probably already exist in various locations.
3. We should test the biomass table functions to see if there is any Bering-specific need that hasn't been filled yet.

## Legal disclaimer
This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
