# Calculate size composition across aggregated subareas

Calculate size composition across aggregated subareas

## Usage

``` r
calc_sizecomp_subarea(
  gapdata = NULL,
  racebase_tables = lifecycle::deprecated(),
  sizecomp_stratum = NULL,
  size_comps = lifecycle::deprecated()
)
```

## Arguments

- gapdata:

  data object created from [`gapindex::get_data()`](get_data.md)

- racebase_tables:

  **\[deprecated\]** Use the `gapdata` argument instead.

- sizecomp_stratum:

  a dataframe of stratum biomass, result object from either
  `gapindex::calc_sizecomp_aigoa_stratum()` or
  `gapindex::calc_sizecomp_bs_stratum()`

- size_comps:

  **\[deprecated\]** Use the `sizecomp_stratum` argument instead.

## Value

|  |  |
|----|----|
| **Field** | **Description** |
| LENGTH_MM | Length bin in millimeters. A length of -9 indicates cases where no lengths were collected within a stratum for a species/year, even though catch numbers were recorded. |
| POPULATION_COUNT | The estimated population caught in the survey for a species, group, or total for a given survey. |
| SEX | Sex of a specimen where "1" = "Male", "2" = "Female", "3" = Unsexed. |
| SPECIES_CODE | The species code of the organism associated with the common_name and scientific_name columns. For a complete species list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
| SURVEY_DEFINITION_ID | The survey definition ID key code is an integer that uniquely identifies a survey region/survey design. The column survey_definition_id is associated with the srvy and survey columns. Full list of survey definition IDs are in RACE_DATA.SURVEY_DEFINITIONS and in the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
| YEAR | Year the observation (survey) was collected. |
| AREA_ID | Area ID key code for each statistical area used to produce production estimates (e.g., biomass, population, age comps, length comps). Each area ID is unique within each survey. |
