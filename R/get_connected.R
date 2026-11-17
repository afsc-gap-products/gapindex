#' Define Oracle connection to Oracle
#' 
#' @description Creates the database connection to Oracle needed to pull SQL 
#' queries from RACE database. Make sure you are connected to the VPN before 
#' running the function. Also support users who use the Rpackage `keyring` to
#' store usernames and passwords. 
#' 
#' @param db string. A registered data source name, in this case "AFSC" by default. This argument is passed to the `dsn` argument in `RODBC::odbcConnect()`
#' @param conn_type string. Which package should be used to set up the database
#' connection. Default is "RODBC" but "DBI" can also be used. 
#' @param check_access boolean. If TRUE (by default), checks whether you have the specific tables in GAP_PRODUCTS, RACEBASE and RACE_DATA used in the gapindex package. Outputs an error if the user does not have access to these tables with a message of the point of contact information for access. 
#' @param check_expiry boolean. If TRUE (by default), checks whether the user's Oracle password is in its grace period or within `warn_days` of expiring.
#' @param warn_days numeric. Number of days before password expiration to trigger a warning message. Default is 7.
#' @return channel either of class "RODBC" or "Oracle". See `?RODBC::odbcConnect()` or `?DBI::dbConnect()`for more detail
#' @export
#' 

get_connected <- function(db = "AFSC", 
                          conn_type = c("RODBC", "DBI")[1],
                          check_access = TRUE,
                          check_expiry = TRUE,
                          warn_days = 7) {
  
  ## Check that the conn_type is either "RODBC" or "DBI"
  if (is.null(x = conn_type) || !conn_type %in% c("RODBC", "DBI")) {
    stop("Argument `conn_type` must be one of these options: 'RODBC' or 'DBI'. The default is 'RODBC'.")
  }
  
  ## Ask user for username and password credentials. If the user has keyring
  ## set up then pull from their keyring::key_list(db)
  if ((db %in% keyring::key_list()[, 1])) {
    username <- keyring::key_list(db)$username
    password <- keyring::key_get(db, keyring::key_list(db)$username)
  } else {
    username <- getPass::getPass(msg = paste("Enter your", db, 
                                             "Oracle Database Username: "))
    password <- getPass::getPass(msg = paste("Enter your", db, 
                                             "Oracle Database Password: "))
  }
  
  ## Assemble arguments for connection function based on the conn_type
  conn_fn <- getExportedValue(name = c("RODBC" = "odbcConnect", 
                                       "DBI" = "dbConnect")[conn_type],
                              ns = conn_type)
  conn_args <- list("RODBC" = list(dsn = paste(db), 
                                   uid = paste(username), 
                                   pwd = paste(password), 
                                   believeNRows = FALSE),
                    "DBI" = list(drv = odbc::odbc(), 
                                 dsn = paste(db), 
                                 uid = paste(username), 
                                 pwd = paste(password)))[[conn_type]]
  
  ## Connect to Oracle and capture any warning/error messages (such as ORA-28000 / ORA-28001)
  conn_err <- character(0)
  channel <- withCallingHandlers(
    tryCatch(
      expr = do.call(what = conn_fn, args = conn_args),
      error = function(cond) {
        conn_err <<- c(conn_err, conditionMessage(cond))
        return(-1)
      }
    ),
    warning = function(w) {
      conn_err <<- c(conn_err, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  if (any(class(x = channel) %in% c("RODBC", "Oracle"))) { ## If connection is successful
    
    cat("Successfully connected to Oracle.\n")
    
    ## Check password expiration / grace period status
    if (check_expiry) {
      expiry_df <- tryCatch(
        gapindex::sql_query(
          channel = channel,
          query = "SELECT EXPIRY_DATE, ACCOUNT_STATUS, ROUND(EXPIRY_DATE - SYSDATE) AS DAYS_LEFT FROM USER_USERS"
        ),
        error = function(e) NULL
      )
      
      if (is.data.frame(expiry_df) && nrow(expiry_df) > 0) {
        status <- expiry_df$ACCOUNT_STATUS[1]
        days_left <- expiry_df$DAYS_LEFT[1]
        exp_date <- expiry_df$EXPIRY_DATE[1]
        
        if (!is.na(status) && grepl("EXPIRED\\(GRACE\\)", status)) {
          warning("Your ", db, " Oracle password has expired and is currently in its grace period. Please reset it immediately to avoid being locked out.", call. = FALSE)
        } else if (!is.na(days_left) && days_left <= warn_days) {
          message(sprintf("Notice: Your %s Oracle password will expire in %d day(s) (on %s).", 
                          db, as.integer(days_left), exp_date))
        }
      }
    }
    
    if (check_access & db == "AFSC") {
      cat("Checking that you have access to the tables queried in the gapindex package.\n")
      tables_to_check <- 
        data.frame(table_name = c(
          "GAP_PRODUCTS.SURVEY_DESIGN",
          "GAP_PRODUCTS.AREA",
          "GAP_PRODUCTS.STRATUM_GROUPS",
          "GAP_TAXONOMY.TAXONOMIC_CLASSIFICATION",
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
      
      ## Loop over the tables and check whether the user can query it
      for (itable in 1:nrow(x = tables_to_check)) { ## Start loop
        
        ## Do a quick test query 
        query <- paste("SELECT * FROM", tables_to_check$table_name[itable], 
                       "WHERE 1 = 0")
        
        table_check <- gapindex::sql_query(channel = channel,
                                           query = query)
        
        if (is.data.frame(x = table_check)) 
          tables_to_check$access[itable] <- TRUE
        
      } ## End loop
      
      if (all(tables_to_check$access == T)) {
        cat("Confirming connection to all Oracle tables associated with the gapindex package.\n")
      } else {
        stop("Cannot connect to these tables in Oracle:\n", 
             paste0(tables_to_check$table_name[tables_to_check$access == F], 
                    collapse = "\n"),
             "\n\nPlease contact nmfs.afsc.gap.metadata@noaa.gov for access to these tables and then try connecting again.")
      }
    }
    
    return(channel)
    
  } else { ## If the connection is not successful, check specific Oracle error codes
    if (any(grepl("ORA-28000", conn_err))) {
      stop("Unable to connect: Your ", db, " Oracle account is locked (ORA-28000). Please contact nmfs.afsc.gap.metadata@noaa.gov or send an IT service request ticket to afsc.servicedesk@noaa.gov to unlock your account before reconnecting.\n\n", call. = FALSE)
    } else if (any(grepl("ORA-28001", conn_err))) {
      stop("Unable to connect: Your ", db, " Oracle password has expired (ORA-28001). Please reset your password (and update keyring if used) before reconnecting.\n\n", call. = FALSE)
    } else {
      stop("Unable to connect. Username or password may be incorrect. Check that you are connected to the network (e.g., VPN). Please re-enter.\n\n", call. = FALSE)
    }
  }
}