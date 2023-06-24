#' Upload a table to Oracle with associated metadata
#' 
#' @description T
#'
#' @param x Either a character string of the path to the .csv file or the data.frame of the table to upload. 
#' @param table_name Name of the table. (Add error checks to make sure table name follows any peculiarities of Oracle tables). 
#' @param table_metadata Description of what the table is. 
#' @param metadata_column data.frame describing the metadata for each of the 
#'                        fields in the table. Must contain these columns: 
#'                        1) colname: name of field
#'                        2) colname_long: longer version of name for printing
#'                           purposes.
#'                        3) units: units of field
#'                        4) dataype: Oracle data type
#'                        5) colname_desc: Full description of field
#' @param channel Establish your oracle connection using a function like `gapindex::get_connected()`. 
#' @param schema character string. The name of the schema to save table. 
#' @param update_metadata boolean. Default = TRUE indicates that the table metadata should be updated. 
#' @param append_table boolean. If TRUE, appends to an existing table, otherwise a new table is created.
#' @param share_with_all_users boolean. Default = TRUE. Give all users in Oracle view permissions. 
#'
#' @return
#' @export
#' 

upload_oracle <- function(x = NULL, 
                          table_name = NULL, 
                          metadata_column = NULL, 
                          table_metadata = NULL,
                          channel = NULL, 
                          schema = NULL, 
                          append_table = NULL,
                          update_metadata = TRUE,
                          share_with_all_users = TRUE) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Initial Error Checks
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Check that table_name is in all caps
  table_name <- toupper(table_name)
  
  # Check that metadata_column was included in the funciton call. 
  # If not, load it directly  from GAP_PRODUCTS
  if (is.null(metadata_column)) {
    metadata_column <- RODBC::sqlQuery(channel, paste0("SELECT * FROM GAP_PRODUCTS.METADATA_COLUMN"))
    names(metadata_column) <- gsub(pattern = "metadata_", replacement = "", x = names(metadata_column))
  }
  
  ## Error Checks for NULLs
  for (ivar in c("x", "table_name", "metadata_column", "table_metadata")) 
    if (is.null(x = get(x = ivar))) 
      stop(paste0("Must provide argument `", ivar, "`."))
  
  ## Check that x is a data.frame. If a path is provided, read the path.
  if (is.character(x = x)) {
    x <- utils::read.csv(file = x)
    # Foce capitalization of column names. Lowercase column names can lead to 
    # problematic downstream issues if not dealt with at this step and cause 
    # mismatches with the metadata_column reference table. 
    names(x) <- toupper(names(x))
  }
  if (!is.data.frame(x = x)) stop("Please supply a data.frame for argument `x`")
  
  ## Check that metadata_column is a dataframe with columns colname, 
  ## colname_long, units, datatype, and colname_desc
  if (!is.data.frame(x = metadata_column)){
    stop(
      "Argument `metadata_column` must be a data.frame.
       See gapindex::oracle_upload() for how to format `metadata_column`.")
  } else {
    if (!all(c("colname", "colname_long", "units", "datatype", "colname_desc")
             %in% names(metadata_column))) 
      stop(
        "Argument `metadata_column` must be a data.frame with fields:
        1) colname: name of field
        2) colname_long: longer version of name for printing purposes.
        3) units: units of field
        4) dataype: Oracle data type
        5) colname_desc: Full description of field")
  }
  
  ## Check that there is a connection
  if (is.null(channel)) channel <- gapindex::get_connected()
  
  cat(paste0("Oracle Data Table: ", schema, ".", table_name, 
             "\nNumber of Rows: ", nrow(x = x), 
             "\nNumber of Fields with Metadata: ", 
             sum(!is.na(x = metadata_column$colname)), "\n"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Initiate table if new, 
  ##   If table already exits, drop table before overwriting
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  start_time <- Sys.time()
  if (!append_table) {
    cat(paste0("Creating or overwriting new table: ", schema, ".", table_name, "\n"))
    
    ## If table is currently in the schema, drop (delete) the table
    existing_tables <-
      unlist(RODBC::sqlQuery(channel = channel,
                             query = "SELECT table_name FROM user_tables;"))
    if (table_name %in% existing_tables)
      RODBC::sqlDrop(channel = channel, sqtable = table_name, errors = FALSE)
  }
  
  cat(paste0("Updating Table ", table_name, " ... "))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Upload table to Oracle
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Format metadata as a named vector to be inputted as argument 
  ## `varTypes` in RODBC::sqlSave()
  metadata_column <- subset(x = metadata_column,
                            subset = !is.na(colname))
  
  vartype_vec <- stats::setNames(object = metadata_column$datatype,
                                 nm = metadata_column$colname)
  
  ## Assign the dataframe `x` to the table_name
  assign(x = table_name, value = x)
  
  ## Add the table to the schema
  # eval(parse(text = paste0("RODBC::sqlSave(channel = channel, dat = ",
  #                          table_name, ", varTypes = vartype_vec, ",
  #                          "rownames = FALSE, append = ", append_table, ")")))
  
  sql_save_args <- list(channel = channel, 
                        dat = x, 
                        varTypes = vartype_vec, 
                        tablename = paste0(schema, ".", table_name), 
                        rownames = FALSE, 
                        append = append_table)
  
  do.call(what = RODBC::sqlSave, args = sql_save_args)
  
  end_time <- Sys.time()
  cat(paste("Time Elapsed:", round(end_time - start_time, 2), 
            units(end_time - start_time), "\n"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Update Metadata
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (update_metadata) {
    cat("Updating Metadata ...\n")
    ## Add column metadata 
    if (nrow(x = metadata_column) > 0) {
      for (i in 1:nrow(x = metadata_column)) {
        
        desc <- gsub(pattern = "<sup>2</sup>",
                     replacement = "2",
                     x = metadata_column$colname_long[i], 
                     fixed = TRUE)
        short_colname <- gsub(pattern = "<sup>2</sup>", 
                              replacement = "2",
                              x = metadata_column$colname[i], 
                              fixed = TRUE)
        
        RODBC::sqlQuery(
          channel = channel,
          query = paste0('comment on column ', 
                         schema, '.', table_name,'.',
                         short_colname,' is \'',
                         desc, ". ", # remove markdown/html code
                         gsub(pattern = "'", replacement ='\"',
                              x = metadata_column$colname_desc[i]),'\';'))
        
      }
    }
    ## Add table metadata 
    RODBC::sqlQuery(
      channel = channel,
      query = paste0('comment on table ', schema,'.', table_name,
                     ' is \'',
                     table_metadata,'\';'))
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Grant select access to all users
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (share_with_all_users) {

    cat("Granting select access to all users ... ")
    all_schemas <- RODBC::sqlQuery(channel = channel,
                                   query = paste0('SELECT * FROM all_users;'))
    
    for (iname in sort(all_schemas$USERNAME)) {
      RODBC::sqlQuery(channel = channel,
                      query = paste0('grant select on ', schema,'.', table_name,
                                     ' to ', iname, ';'))
    }
    
  }
  cat("Finished\n\n")
}
