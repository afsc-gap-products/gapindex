# Calculate index of total biomass per stratum

Calculate index of total biomass per stratum

## Usage

``` r
calc_biomass_stratum(
  gapdata = NULL,
  racebase_tables = lifecycle::deprecated(),
  cpue = NULL
)
```

## Arguments

- gapdata:

  data object created from [`gapindex::get_data()`](get_data.md)

- racebase_tables:

  **\[deprecated\]** Use the `gapdata` argument instead.

- cpue:

  object created from [`gapindex::calc_cpue()`](calc_cpue.md).

## Value

|  |  |
|----|----|
| **Field** | **Description** |
| N_COUNT | Total number of hauls with positive count data. |
| N_HAUL | Total number of hauls. |
| N_LENGTH | Total number of hauls with length data. |
| N_WEIGHT | Total number of hauls with positive catch biomass. |
| POPULATION_COUNT | The estimated population caught in the survey for a species, group, or total for a given survey. |
| POPULATION_VAR | The estimated population variance caught in the survey for a species, group, or total for a given survey. |
| SPECIES_CODE | The species code of the organism associated with the common_name and scientific_name columns. For a complete species list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
| STRATUM | RACE database statistical area for analyzing data. Strata were designed using bathymetry and other geographic and habitat-related elements. The strata are unique to each survey region. Stratum of value 0 indicates experimental tows. |
| SURVEY | Name and description of survey. The column survey is associated with the srvy and survey_definition_id columns. |
| SURVEY_DEFINITION_ID | The survey definition ID key code is an integer that uniquely identifies a survey region/survey design. The column survey_definition_id is associated with the srvy and survey columns. Full list of survey definition IDs are in RACE_DATA.SURVEY_DEFINITIONS and in the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
| YEAR | Year the observation (survey) was collected. |
| BIOMASS_MT | The estimated total biomass. |
| BIOMASS_VAR | The estimated variance associated with the total biomass. |
| CPUE_KGKM2_MEAN | The mean catch weight (kilograms) per unit effort (area swept by the net, units squared kilometers). |
| CPUE_KGKM2_VAR | The variance of mean catch weight (kilograms) per unit effort (area swept by the net, units squared kilometers). |
| CPUE_NOKM2_MEAN | The mean of numerical catch per unit effort (area swept by the net, units square kilometers). |
| CPUE_NOKM2_VAR | The variance of mean numerical catch per unit effort (area swept by the net, units square kilometers). |
