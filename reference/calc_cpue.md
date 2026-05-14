# Calculate haul-level catch per unit effort

This function calculates and zero-fills weight and numerical catch per
unit effort (area swept, km2)

## Usage

``` r
calc_cpue(gapdata = NULL, racebase_tables = lifecycle::deprecated())
```

## Arguments

- gapdata:

  data object created from [`gapindex::get_data()`](get_data.md)

- racebase_tables:

  **\[deprecated\]** Use the `gapdata` argument instead.

## Value

|  |  |
|----|----|
| **Field** | **Description** |
| DEPTH_M | Bottom depth (meters). |
| DESIGN_YEAR | Year ID associated with a given value AREA_ID. This field describes the changes in the survey design over time. |
| HAULJOIN | This is a unique numeric identifier assigned to each (vessel, cruise, and haul) combination. |
| LATITUDE_DD_END | Latitude (one hundred thousandth of a decimal degree) of the end of the haul. |
| LATITUDE_DD_START | Latitude (one hundred thousandth of a decimal degree) of the start of the haul. |
| LONGITUDE_DD_END | Longitude (one hundred thousandth of a decimal degree) of the end of the haul. |
| LONGITUDE_DD_START | Longitude (one hundred thousandth of a decimal degree) of the start of the haul. |
| SPECIES_CODE | The species code of the organism associated with the common_name and scientific_name columns. For a complete species list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
| STRATUM | RACE database statistical area for analyzing data. Strata were designed using bathymetry and other geographic and habitat-related elements. The strata are unique to each survey region. Stratum of value 0 indicates experimental tows. |
| SURVEY | Name and description of survey. The column survey is associated with the srvy and survey_definition_id columns. |
| SURVEY_DEFINITION_ID | The survey definition ID key code is an integer that uniquely identifies a survey region/survey design. The column survey_definition_id is associated with the srvy and survey columns. Full list of survey definition IDs are in RACE_DATA.SURVEY_DEFINITIONS and in the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). |
| WEIGHT_KG | Weight (thousandths of a kilogram) of individuals in a haul by taxon. |
| YEAR | Year the observation (survey) was collected. |
| AREA_SWEPT_KM2 | The area the net covered while the net was fishing (kilometers squared), defined as the distance fished times the net width. |
| BOTTOM_TEMPERATURE_C | Bottom temperature (tenths of a degree Celsius); NA indicates removed or missing values. |
| COUNT | Total whole number of individuals caught in haul or samples collected. |
| CPUE_KGKM2 | Catch weight (kilograms) per unit effort (area swept by the net, units square kilometers). |
| CPUE_NOKM2 | Numerical catch per unit effort (area swept by the net, units square kilometers). |
| CRUISE | This is a six-digit integer identifying the cruise number of the form: YYYY99 (where YYYY = year of the cruise; 99 = 2-digit number and is sequential; 01 denotes the first cruise that vessel made in this year, 02 is the second, etc.). |
| CRUISEJOIN | Unique integer ID assigned to each survey, vessel, and year combination. |
