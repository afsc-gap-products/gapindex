#' Pull station/grid data
#'
#' @param region Survey region (GOA, AI, EBS, or NBS)
#' @param sql_channel  `r lifecycle::badge("deprecated")` Use the `channel` 
#'                     argument instead.
#' @param channel      connection to Oracle created via 
#'                     gapindex::get_connected() or RODBC::odbcConnect().
#'
#' @return a dataframe containing station information from the specified survey.
#' If you request data for the EBS survey, the columns in the dataframe will 
#' be STATIONID, LATITUDE, LONGITUDE, and STRATUM. For AI and GOA, the columns 
#' will be AIGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, CENTER_LONG, 
#' SOUTH_LAT, EAST_LONG, WEST_LONG.
#'
#' @export
#'

get_station_info <- function(region = NULL,
                             channel = NULL,
                             sql_channel = lifecycle::deprecated()) {

  ## Error Check on argument `region`
  if (length(x = region) > 1 | !(region %in% c('EBS', 'NBS', 'GOA', 'AI'))) 
    stop("Input only one `region` from c('EBS', 'NBS', 'GOA', 'AI')")
  
  ##   Set up channel if channel = NULL
  if (lifecycle::is_present(sql_channel)) {
    lifecycle::deprecate_warn("2.2.0", 
                              "get_data(sql_channel)", 
                              "get_data(channel)")
    channel <- sql_channel
  }
  if (is.null(x = channel)) channel <- gapindex::get_connected()
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Pull station grid information
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stn_query <- 
    switch(
      region,
      "EBS" = "select STATIONID, LATITUDE, LONGITUDE, STRATUM 
      from racebase.stations 
      where stratum is not null 
      and stratum not in (70, 71, 81)",
      "NBS" = "select STATIONID, LATITUDE, LONGITUDE, STRATUM 
      from racebase.stations 
      where stratum in (70, 71, 81)",
      "AI" = "select AIGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, 
      CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG 
      from ai.aigrid_gis",
      "GOA" = "select GOAGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, 
      CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG 
      from goa.goagrid_gis"
    )
  
  gridinfo <- RODBC::sqlQuery(channel = channel, query = stn_query)
  return(gridinfo)
}
