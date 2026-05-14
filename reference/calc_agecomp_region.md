# Calculate region-level age composition and mean/std.dev length at age

Calculate region-level age composition and mean/std.dev length at age

## Usage

``` r
calc_agecomp_region(
  gapdata = NULL,
  racebase_tables = lifecycle::deprecated(),
  agecomp_stratum = NULL,
  age_comps_stratum = lifecycle::deprecated()
)
```

## Arguments

- gapdata:

  data object created from [`gapindex::get_data()`](get_data.md)

- racebase_tables:

  **\[deprecated\]** Use the `gapdata` argument instead.

- agecomp_stratum:

  a named list of stratum age comps and numbers by
  survey/year/stratum/sex/length, a result object from
  [`gapindex::calc_agecomp_stratum()`](calc_agecomp_stratum.md).

- age_comps_stratum:

  **\[deprecated\]** Use the `agecomp_stratum` argument instead.

## Value

dataframe of age composition and mean/standard deviation of length at
age aggregated across regions.

|  |  |
|----|----|
| **Field** | **Description** |
| LENGTH_MM_MEAN | Mean length (millimeters) |
| LENGTH_MM_SD | Variance of mean length. |
| POPULATION_COUNT | The estimated population caught in the survey for a species, group, or total for a given survey. |
| SEX | Sex of a specimen where "1" = "Male", "2" = "Female", "3" = Unsexed. |
| SPECIES_CODE | The species code of the organism associated with the common_name and scientific_name columns. For a complete species list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
| SURVEY_DEFINITION_ID | The survey definition ID key code is an integer that uniquely identifies a survey region/survey design. The column survey_definition_id is associated with the srvy and survey columns. Full list of survey definition IDs are in RACE_DATA.SURVEY_DEFINITIONS and in the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
| YEAR | Year the observation (survey) was collected. |
| AGE | Age bin of taxon. Age bin of a taxon in years estimated by the age comp estimate. Age -9 indicates unaged lengths for a particular sex because no otoliths were collected for that sex/length combination. Age -99 indicates a case where no lengths were collected within a stratum for a species/year even though catch numbers were recorded. |
| AREA_ID_FOOTPRINT | Survey footprint, usually equivalent to the SURVEY_DEFINITION_ID with the exception of the Standard and Standard +NW survey footprints in the Eastern Bering Sea shelf bottom trawl survey |
| AREA_ID | Area ID key code for each statistical area used to produce production estimates (e.g., biomass, population, age comps, length comps). Each area ID is unique within each survey. |
