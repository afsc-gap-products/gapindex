# By Paul von Szalay, sent to me 5/19/21
# SQL code is by Ned.
# This function extracts relevant data from racebase.haul,goa.cpue, and racebase.species tables
get_data_geostat <- function(channel = NA, region = "GOA", sp.code = 21740){
  
  if(is.na(channel)){
    require(RODBC)
    channel <- odbcConnect(dsn = "AFSC", uid = "siplem", pwd = "$fJntwv029", believeNRows = FALSE)
    close.channel = TRUE
  }else{
    close.channel <- FALSE
  }
  
  sqry <- paste0("select a.vessel, start_latitude lat, start_longitude lon, a.cruise||'_'||b.haul||'_'||b.vessel towid, 
                 floor(a.cruise/100) year, weight catch_kg, effort areaswept_km2, c.common_name
                 from racebase.haul a, goa.cpue b, racebase.species c
                 where a.hauljoin = b.hauljoin and a.region = '", 
                 region, "' and a.cruise >= 199001 and b.species_code = ", 
                 sp.code, " and b.species_code = c.species_code and a.start_longitude < -140")
  
  dat <- sqlQuery(channel = channel, query = sqry, rows_at_time = 1, errors = TRUE)
  names(dat) <- tolower(names(dat))
  
  if(close.channel)close(channel)
  
  dat
  
}

get_data_geostat()
