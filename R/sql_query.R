#' Streamlined querying function using the DBI package
#' 
#' @description This function executes a SQL query regardless if the database connection was created via RODBC or DBI/ODBC. combines the DBI::dbSendQuery() and DBI::dbFetch() functions
#' 
#' @param channel A database connection either made via DBI::dbConnect(), odbc::dbConnect or  RODBC::odbcConnect()
#' @param query A character string containing a SQL query.
#' 
#' @return if the query is successfully executed, a dataframe of the query will result. Otherwise, an error message is returned as a string (or set of strings).
#' 
#' @export

sql_query <- function(channel, query) {
  
  if (is.null(x = channel) || !inherits(x = channel, what = c("RODBC", "Oracle"))) {
    stop("Argument 'channel' must be a valid RODBC or DBI connection object e.g., via gapindex::get_connected()")
  }
  
  if (inherits(x = channel, what = "Oracle")  ) {
    tryCatch(expr = DBI::dbGetQuery(conn = channel, statement = query),
             error = function(cond) {
               # This function is executed if an error occurs
               # 'cond' is the error condition object
               return(conditionMessage(cond))   # Return the error message
             })
  } else (
    RODBC::sqlQuery(channel = channel, query = query)
  )
  
  
}

