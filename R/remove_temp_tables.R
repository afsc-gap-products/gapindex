#' Drop Temporary Tables Created within the get_data() call
#' 
#' @param channel connection to Oracle created via gapindex::get_connected() 
#'
#' @export
#' 

remove_temp_tables <- function(channel) {
  
  ## Loop through temporary table names -- start
  for (itable in c("AVAIL_SPP", "CATCH", "CRUISE", "HAUL", "INPUT_SPP", 
                   "SIZE", "SPECIMEN", "STRATUM_GROUPS", "STRATUM", 
                   "SURVEY_DESIGN", "SURVEY", "SUBAREA", 
                   "USER_TAXONOMIC_INFO", "UNAVAIL_SPP", "USER_INPUT_SPP")) { 
    
    ## If itable exists
    if (nrow(x = RODBC::sqlQuery(
      channel = channel,
      query = paste0("SELECT table_name
                      FROM user_tables
                      WHERE TABLE_NAME = 'GAPINDEX_TEMPORARY_",  
                     itable, "_QUERY'"))) != 0) {
      ## Drop the table
      RODBC::sqlQuery(channel = channel, 
                      query = paste0("DROP TABLE ", "GAPINDEX_TEMPORARY_", 
                                     itable, "_QUERY PURGE;"))
    }
  } ## Loop through temporary table names -- end
}
