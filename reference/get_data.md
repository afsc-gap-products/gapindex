# Pull AFSC GAP BTS survey data

Pulls survey, cruise, haul, catch, size, specimen (age), stratum,
subarea, stratum grouping information for the region, years, and species
of interest from the RACEBASE, RACE_DATA, and GAP_PRODUCTS schemata in
the AFSC Oracle database.

## Usage

``` r
get_data(
  year_set = c(1996, 1999),
  survey_set = c("GOA", "AI", "EBS", "NBS", "BSS")[1],
  spp_codes = c(21720, 30060, 10110),
  haul_type = 3,
  abundance_haul = c("Y", "N")[1],
  pull_lengths = FALSE,
  taxonomic_source = c("RACEBASE.SPECIES_CLASSIFICATION",
    "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION")[1],
  remove_na_strata = FALSE,
  na_rm_strata = lifecycle::deprecated(),
  channel = NULL,
  sql_channel = lifecycle::deprecated()
)
```

## Arguments

- year_set:

  numeric or integer vector of years

- survey_set:

  character string. One of c("GOA", "AI", "EBS", "NBS", "BSS").

- spp_codes:

  two-column data.frame of species codes (column name SPECIES_CODE) and
  GROUP_CODE name (column name GROUP_CODE). For single-species, the
  GROUP_CODE and species codes can be the same. Examples: 1) For a
  mixture of individual taxa and taxon groups: data.frame("SPECIES_CODE"
  = c(21720, 21220, 21230, 21232), "GROUP_CODE" = c(21720, "Grenadiers",
  "Grenadiers", "Grenadiers")) 2) For single taxa: c(21720,
  21740, 10110) 3) FOR PRODUCTION PURPOSES: a NULL value by default uses
  GAP_PRODUCTS.TAXON_GROUPS from the AFSC Oracle database.

- haul_type:

  integer. Defaults to haul type "3" for "standard bottom sample
  (preprogrammed station)" used for production purposes.

- abundance_haul:

  character string. "Y" are standardized hauls used in production and
  "N" are non-standard hauls due to bad performance, different gear,
  etc.

- pull_lengths:

  boolean T/F. Should length and specimen data be pulled? Defaults to
  FALSE for speed.

- taxonomic_source:

  character string. Table used to source taxonomic information. One of
  two options: "RACEBASE.SPECIES" (default) or
  "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION".
  "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION" is still a provisional table
  is an option for testing only.

- remove_na_strata:

  boolean. Remove hauls with NA stratum information. Defaults to FALSE.

- na_rm_strata:

  **\[deprecated\]** Use the `remove_na_strata` argument instead.

- channel:

  connection to Oracle created via gapindex::get_connected() or
  RODBC::odbcConnect().

- sql_channel:

  **\[deprecated\]** Use the `channel` argument instead.

## Value

a named list containing survey_design, survey, cruise, haul, catch, size
(if pull_lengths == TRUE), specimen (if pull_lengths == TRUE), species,
stratum, subarea, and stratum_groups information for the survey, years,
and species of interest.
