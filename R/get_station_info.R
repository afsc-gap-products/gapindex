#' Pull station/grid data
#'
#' @param region Survey region (GOA, AI, or EBS)
#' @param sql_channel connection created via gapindex::get_connected()
#'
#' @return a dataframe containing station information from the specified survey.
#' If you request data for the EBS survey, the columns in the dataframe will be STATIONID,
#' LATITUDE, LONGITUDE, and STRATUM. For AI and GOA, the columns will be AIGRID_ID,
#' TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG.
#'
#' @export
#'
get_station_info <- function(region = NULL,
                             sql_channel = NULL) {
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   1) Set up channel if sql_channel = NULL
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(x = sql_channel)) sql_channel <- gapindex::get_connected()

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   2) Pull station grid information
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (region == "EBS") {
    gridinfo <- RODBC::sqlQuery(
      query = "select STATIONID, LATITUDE, LONGITUDE, STRATUM from racebase.stations",
      channel = channel
    )
  }

  if (region == "AI") {
    # Query AIGRID table and join with survey grid shapefile
    gridinfo <- RODBC::sqlQuery(
      query = "select AIGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, 
      CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG from ai.aigrid_gis",
      channel = channel
    )
  }
  if (region == "GOA") {
    gridinfo <- RODBC::sqlQuery(
      query = "select GOAGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, 
      CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG from goa.goagrid_gis",
      channel = channel
    )
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   3) Return grid info
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  return(gridinfo)
}