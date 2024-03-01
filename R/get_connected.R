#' Define RODBC connection to Oracle
#' 
#' @description Creates the RODBC connection to Oracle needed to pull SQL 
#' queries from RACE database. Make sure you are connected to the VPN before 
#' running the function. Also support users who use the Rpackage `keyring` to
#' store usernames and passwords. 
#' 
#' @param db string. A registered data source name, in this case "AFSC" by default. This argument is passed to the `dsn` argument in `RODBC::odbcConnect()`
#' @param check_access boolean. If TRUE (by default), checks whether you have the specific tables in GAP_PRODUCTS, RACEBASE and RACE_DATA used in the gapindex package. Outputs an error if the user does not have access to these tables with a message of the point of contact information for access. 
#' @return channel of class "RODBC". See `?RODBC::odbcConnect()` for more detail
#' @export
#' 

get_connected <- function(db = "AFSC", check_access = TRUE) {
  
   # check if database name is stored in keyring, if not request user/pwd
  if(!(db %in% keyring::key_list()[,1])) {
    username <- getPass::getPass(msg = paste("Enter your", db, 
                                             "Oracle Database Username: "))
    password <- getPass::getPass(msg = paste("Enter your", db, 
                                             "Oracle Database Password: "))
  } else {
    username <- keyring::key_list(db)$username
    password <-  keyring::key_get(db, keyring::key_list(db)$username)
  }
  
  suppressWarnings(channel <- RODBC::odbcConnect(dsn = paste(db), 
                                                 uid = paste(username), 
                                                 pwd = paste(password), 
                                                 believeNRows = FALSE))
  if (channel == -1) {
    stop("Unable to connect. Username or password may be incorrect. Check that you are connected to the network (e.g., VPN). Please re-enter.\n\n")
    return(invisible())
  }
  
  if (class(channel) == "RODBC") {
    cat("Successfully connected to Oracle.\n")
    
    if (check_access) {
      cat("Checking that you have access to the tables queried in the gapindex package.\n")
      tables_to_check <- 
        data.frame(table_name = c(
          "GAP_PRODUCTS.SURVEY_DESIGN",
          "GAP_PRODUCTS.AREA",
          "GAP_PRODUCTS.STRATUM_GROUPS",
          "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION",
          
          "RACEBASE.CATCH",
          "RACEBASE.HAUL",
          "RACEBASE.LENGTH",
          "RACEBASE.SPECIMEN",
          
          "RACE_DATA.CRUISES",
          "RACE_DATA.SURVEYS",
          "RACE_DATA.SURVEY_DEFINITIONS",
          "RACE_DATA.VESSELS"),
          access = F)
      
      for (itable in 1:nrow(x = tables_to_check)) {
        table_check <- tryCatch(
          expr = RODBC::sqlFetch(channel = channel,
                                 sqtable = tables_to_check$table_name[itable], 
                                 max = 5),
          error = function(cond) data.frame()
        )
        if (nrow(x = table_check) == 5) 
          tables_to_check$access[itable] <- TRUE
      }
      
      if (all(tables_to_check$access == T)) {
        cat("Confirming connection to all Oracle tables associated with the gapindex package.\n")
        return(channel)
      } else (
        stop("Cannot connect to these tables in Oracle:\n", 
             paste0(tables_to_check$table_name[tables_to_check$access == F], 
                    collapse = "\n"),
             "\n\nPlease contact nmfs.afsc.gap.metadata@noaa.gov for access to these tables and then try connecting again.")
      )
      
    } else(return(channel))
  }
}
