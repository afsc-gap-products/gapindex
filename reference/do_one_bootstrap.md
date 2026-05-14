# Bootstrap a CPUE table from gapindex

Bootstrap a CPUE table from gapindex

## Usage

``` r
do_one_bootstrap(boot, gapdata = NULL, gapcpue = NULL)
```

## Arguments

- boot:

  (numeric) bootstrap number.

- gapdata:

  gapdata object created from gapindex::get_data(). This can have
  multiple years of data, but should be one species and one region only.

- gapcpue:

  object created from gapindex::calc_cpue(). It should be created from
  the gapdata object.

## Value

a resampled CPUE table with one row per sample.

## Details

This function bootstraps CPUE within stratum (i.e., the bootstrapped
dataset will have the same number of hauls per stratum) with
replacement. This is the same method used in Monnahan et al. (2026).
This function returns one single resample of a CPUE table. Usually, you
would be running this function several times (1 for each bootstrap you
want to do of the data).

## Examples

``` r
testdata <- gapindex::get_data(
  year_set = c(1996, 1999),
  survey_set = c("GOA", "AI", "EBS", "NBS", "BSS")[1],
  spp_codes = c(21740, 30060, 10110)[1],
  haul_type = 3,
  abundance_haul = c("Y", "N")[1],
  pull_lengths = TRUE
)
#> Warning: Selecting ‘env’ backend. Secrets are stored in environment variables
#> Error in b_env_list(self, private, service, keyring): 'service' is required for 'env' backend.
testcpue <- gapindex::calc_cpue(gapdata = testdata)
#> Error: object 'testdata' not found

x1 <- do_one_bootstrap(boot = 1, gapdata = testdata, gapcpue = testcpue)
#> Error: object 'testcpue' not found
x2 <- do_one_bootstrap(boot = 2, gapdata = testdata, gapcpue = testcpue)
#> Error: object 'testcpue' not found

#library(ggplot2)
#ggplot(testcpue, aes(x = factor(YEAR), y = CPUE_KGKM2)) +
#   geom_jitter(width = 0.05, height = 0.00001) +
#   geom_jitter(data = x1$cpue, width = 0.05, color = "blue", alpha = 0.2) +
#   geom_jitter(data = x2$cpue, width = 0.05, color = "red", alpha = 0.2)
```
