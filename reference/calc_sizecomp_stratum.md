# Calculate numerical CPUE at length by haul

Uses Equation 16 in Wakabayashi et al. 1985 to calculate
numbers-catch-per-unit-effort (CPUE, Numbers/km2) at length.

## Usage

``` r
calc_sizecomp_stratum(
  racebase_tables = deprecated(),
  gapdata = NULL,
  racebase_cpue = deprecated(),
  cpue = NULL,
  racebase_stratum_popn = deprecated(),
  abundance_stratum = NULL,
  spatial_level = c("stratum", "haul")[1],
  fill_NA_method = c("AIGOA", "BS")[1]
)
```

## Arguments

- racebase_tables:

  **\[deprecated\]** Use the `gapdata` argument instead.

- gapdata:

  data object created from [`gapindex::get_data()`](get_data.md)

- racebase_cpue:

  **\[deprecated\]** Use the `cpue` argument instead.

- cpue:

  catch per unit effort object created from
  [`gapindex::calc_cpue()`](calc_cpue.md).

- racebase_stratum_popn:

  **\[deprecated\]** Use the `abundance_stratum` argument instead.

- abundance_stratum:

  biomass/abundance by stratum object created from
  [`gapindex::calc_biomass_stratum()`](calc_biomass_stratum.md)

- spatial_level:

  string, one of c("stratum", "haul"). Should size compositions be
  calculated at the level of the "stratum" (used for standard size
  compositions) or "haul" (used for ModSquad model-based data-inputs).

- fill_NA_method:

  string, one of c("AIGOA", "BS"). This argument changes the way hauls
  with positive weights but no associated size data are dealt with. In
  the EBS, NBS, and BSS survey regions ("BS"), these hauls contribute to
  the dummy length -9 category for their respective strata. In the AI
  and GOA survey regions ("AIGOA"), an average size distribution is
  applied to these hauls so the length -9 category does not exist in the
  AI or GOA versions of the size composition.

## Value

|  |  |
|----|----|
| **Field** | **Description** |
| LENGTH_MM | Length bin in millimeters. A length of -9 indicates cases where no lengths were collected within a stratum for a species/year, even though catch numbers were recorded. |
| POPULATION_COUNT | The estimated population caught in the survey for a species, group, or total for a given survey. |
| SEX | Sex of a specimen where "1" = "Male", "2" = "Female", "3" = Unsexed. |
| SPECIES_CODE | The species code of the organism associated with the common_name and scientific_name columns. For a complete species list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
| STRATUM | RACE database statistical area for analyzing data. Strata were designed using bathymetry and other geographic and habitat-related elements. The strata are unique to each survey region. Stratum of value 0 indicates experimental tows. |
| SURVEY | Name and description of survey. The column survey is associated with the srvy and survey_definition_id columns. |
| SURVEY_DEFINITION_ID | The survey definition ID key code is an integer that uniquely identifies a survey region/survey design. The column survey_definition_id is associated with the srvy and survey columns. Full list of survey definition IDs are in RACE_DATA.SURVEY_DEFINITIONS and in the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
| YEAR | Year the observation (survey) was collected. |
