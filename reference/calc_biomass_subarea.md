# Calculate index of total biomass across aggregated subareas

Calculate index of total biomass across aggregated subareas

## Usage

``` r
calc_biomass_subarea(
  racebase_tables = lifecycle::deprecated(),
  gapdata = NULL,
  biomass_strata = lifecycle::deprecated(),
  biomass_stratum = NULL
)
```

## Arguments

- racebase_tables:

  **\[deprecated\]** Use the `gapdata` argument instead.

- gapdata:

  data object created from [`gapindex::get_data()`](get_data.md)

- biomass_strata:

  **\[deprecated\]** Use the `biomass_stratum` argument instead.

- biomass_stratum:

  a dataframe of stratum biomass, result object from
  [`gapindex::calc_biomass_stratum()`](calc_biomass_stratum.md)

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
| SURVEY | Name and description of survey. The column survey is associated with the srvy and survey_definition_id columns. |
| SURVEY_DEFINITION_ID | The survey definition ID key code is an integer that uniquely identifies a survey region/survey design. The column survey_definition_id is associated with the srvy and survey columns. Full list of survey definition IDs are in RACE_DATA.SURVEY_DEFINITIONS and in the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
| YEAR | Year the observation (survey) was collected. |
| AREA_ID | Area ID key code for each statistical area used to produce production estimates (e.g., biomass, population, age comps, length comps). Each area ID is unique within each survey. |
| BIOMASS_MT | The estimated total biomass. |
| BIOMASS_VAR | The estimated variance associated with the total biomass. |
| CPUE_KGKM2_MEAN | The mean catch weight (kilograms) per unit effort (area swept by the net, units squared kilometers). |
| CPUE_KGKM2_VAR | The variance of mean catch weight (kilograms) per unit effort (area swept by the net, units squared kilometers). |
| CPUE_NOKM2_MEAN | The mean of numerical catch per unit effort (area swept by the net, units square kilometers). |
| CPUE_NOKM2_VAR | The variance of mean numerical catch per unit effort (area swept by the net, units square kilometers). |
