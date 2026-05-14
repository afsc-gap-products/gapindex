# Pull station/grid data

Pull station/grid data

## Usage

``` r
get_station_info(
  region = NULL,
  channel = NULL,
  sql_channel = lifecycle::deprecated()
)
```

## Arguments

- region:

  Survey region (GOA, AI, EBS, or NBS)

- channel:

  connection to Oracle created via gapindex::get_connected() or
  RODBC::odbcConnect().

- sql_channel:

  **\[deprecated\]** Use the `channel` argument instead.

## Value

a dataframe containing station information from the specified survey. If
you request data for the EBS survey, the columns in the dataframe will
be STATIONID, LATITUDE, LONGITUDE, and STRATUM. For AI and GOA, the
columns will be AIGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT,
CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG.
