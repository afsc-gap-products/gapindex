# Calculate stratum-level age composition and mean/std.dev length at age

Calculate stratum-level age composition and mean/std.dev length at age

## Usage

``` r
calc_agecomp_stratum(
  gapdata = NULL,
  racebase_tables = lifecycle::deprecated(),
  alk = NULL,
  sizecomp_stratum = NULL,
  size_comp = lifecycle::deprecated()
)
```

## Arguments

- gapdata:

  data object created from [`gapindex::get_data()`](get_data.md)

- racebase_tables:

  **\[deprecated\]** Use the `gapdata` argument instead.

- alk:

  age-length key (dataframe) created from
  [`gapindex::calc_alk()`](calc_ALK.md)

- sizecomp_stratum:

  a dataframe of size compositions created from
  [`gapindex::calc_sizecomp_stratum()`](calc_sizecomp_stratum.md).

- size_comp:

  **\[deprecated\]** Use the `sizecomp_stratum` argument instead.

## Value

A named list with two elements. "age_comp" is a dataframe of numbers at
age by survey, year, stratum, species, and sex. A table of column name
descriptions is coming soon."length_at_age" is a support table for the
gapindex::calc_agecomp_region() function and should not be used.
