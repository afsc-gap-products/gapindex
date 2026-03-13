#' Drop Temporary Tables Created within the get_data() call
#' 
#' @param channel connection to Oracle created via gapindex::get_connected() 
#'
#' @keywords internal 
#' 

remove_temp_tables <- function(channel = NULL) {
  
  if (!(class(x = channel) %in% c("RODBC", "Oracle")) | 
      is.null(x = channel)) {
    stop("Argument `channel` must be either an RODBC or DBI/ODBC database connection, e.g., via gapindex::get_connected()")
  }
  conn_type <- class(x = channel)
  
  ## Loop through temporary table names -- start
  for (itable in c("AVAIL_SPP", "CATCH", "CRUISE", "HAUL", "INPUT_SPP", 
                   "SIZE", "SPECIMEN", "STRATUM_GROUPS", "STRATUM", 
                   "SURVEY_DESIGN", "SURVEY", "SUBAREA", 
                   "USER_TAXONOMIC_INFO", "UNAVAIL_SPP", "USER_INPUT_SPP")) { 
    
    table_exists <- gapindex::sql_query(
      channel = channel,
      query = paste0("SELECT table_name FROM user_tables WHERE TABLE_NAME = ",
                    "'GAPINDEX_TEMPORARY_", itable, "_QUERY'")
    ) |> nrow() != 0
    
    ## Drop and purge the temporary table if it exists
    if (table_exists) 
      gapindex::sql_query(channel = channel, 
                          query = paste0("DROP TABLE ", "GAPINDEX_TEMPORARY_", 
                                         itable, "_QUERY PURGE;"))
  } ## Loop through temporary table names -- end
}
