# design-based-indices (in progress)

Code to generate design-based indices of abundance from survey data. The code takes haul and catch tables from RACEBASE to calculate CPUE by region and species. The goal of this code is to reproduce the tables found in the GOA and AI schemas: `BIOMASS_STRATUM`, `BIOMASS_TOTAL`, and `CPUE`. I (Megsie) am currently testing this code with those two regions in mind; the repo may be modified in the future to also include Bering Sea functions, OR Bering Sea indices may become their own repository.

Equations (from Wakabayashi et al. 1985, modified / adapted) can be found under [vignettes](https://github.com/afsc-gap-products/design-based-indices/tree/master/vignettes).

## Workflow

**1. Local copies of data tables.** These scripts work off a local copy of the RACEBASE tables. The `00_download_data_from_oracle.R` script will download these tables and save them locally for you the first time you run the code (or whenever you want to update -- note that this requires you to add a `setup_channel.R` chunk that contains your Oracle info). 

Before you run the summary scripts, the `01_cleanup_data.R` script cleans up the data and formats it for the other functions.

**2. Summary functions.** The other functions contained in this repo will summarize data from RACEBASE as needed. Function names are based on the tables as they appear in the GOA or AI schemas. For example, `get_cpue()` produces the CPUE table from the GOA or AI schema.


## Legal disclaimer
This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
