# Calculate age-length key (alk)

Calculates empircal probability of age at length from collected otolith
data.

## Usage

``` r
calc_alk(
  gapdata = NULL,
  racebase_tables = lifecycle::deprecated(),
  unsex = c("all", "unsex")[2],
  global = TRUE
)
```

## Arguments

- gapdata:

  data object created from [`gapindex::get_data()`](get_data.md)

- racebase_tables:

  **\[deprecated\]** Use the `gapdata` argument instead.

- unsex:

  string, option to determine how unsexed individuals are treated.
  Option "all" means that unsexed alks are determined by combining all
  sexes (males, females, and unsexed) and is the option used for
  standard design-based composition production. Option "unsex" means
  that unsexed alk are only determined by unsexed individuals and is the
  option used for creating data inputs for model-based age composition
  indices. Defaults to "unsex"

- global:

  boolean. Should missing length bins be filled by using an alk
  consisting of all years? Defaults to TRUE.

## Value

dataframe of probabilities ("AGE_FRAC") of ages ("AGE") by length
(LENGTH_MM) for a given survey ("SURVEY"), year ("YEAR"), species
(SPECIES_CODE), and sex (SEX).
