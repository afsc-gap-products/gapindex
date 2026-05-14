# Example: Bootstrapping haul data

Example for bootstrapping haul data in a stratified random way and
getting a ‘realized distribution’ for total biomass.

For some stock assessments, the lognormal distribution may not be the
most appropriate distribution to assume for the distribution of stratum
biomass ([Monnahan et
al. 2026](https://academic.oup.com/icesjms/article/82/5/fsaf071/8151269)).
For these stocks, a generalized gamma or other distribution may be more
appropriate. The bootstrapping functions in gapindex allow you to
bootstrap the haul data while preserving the stratum-level sample size.
Here is a simple example for how to do this.

``` r

library(gapindex)

## Connect to Oracle. Make sure you are on the NOAA internal network or VPN
channel <- gapindex::get_connected()

## Pull GOA pollock data for 2023
gapindex_data <- gapindex::get_data(survey_set = "GOA",
                                    year_set = 2025,
                                    spp_codes = 21740,
                                    pull_lengths = TRUE,
                                    channel = channel)

## Calculate and zero-fill CPUE using gapindex
gapindex_cpue <- gapindex::calc_cpue(gapdata = gapindex_data)

# Bootstrap the data 50 times (should be more, this is just for an example. Boost this up to 500 or more if you want to see a reasonable-looking distribution.
bootstrapped_data <- suppressWarnings(run_bootstraps(nboot = 50, gapdata = gapindex_data, gapcpue = gapindex_cpue)) # this function gives warnings about depth categories; suppress them to save your brain

biomass_stratum <- gapindex::calc_biomass_stratum(gapdata = gapindex_data, cpue = gapindex_cpue)
biomass_subarea <- gapindex::calc_biomass_subarea(gapdata = gapindex_data, biomass_stratum = biomass_stratum)
biomass_total <- biomass_subarea[which(biomass_subarea$AREA_ID==99903),]

# Plot the bootstrapped total biomass
library(ggplot2)
ggplot(bootstrapped_data$biomass_total, aes(x=BIOMASS_MT)) +
  geom_histogram() +
  geom_vline(xintercept = biomass_total$BIOMASS_MT, color = 'red')
```

The red vertical line in this figure is the biomass index passed to
stock assessment in 2025. The histogram shows the distribution of total
biomasses calculated each of 500 bootstraps of the existing haul data.
