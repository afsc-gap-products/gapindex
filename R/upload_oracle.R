#' Upload a table to Oracle with associated metadata
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
#' @param share_with_all_users boolean. Default = TRUE. Give all users in Oracle select permissions. 
#'
#' @export
#' 

upload_oracle <- function(x = NULL, 
                          table_name = NULL, 
                          metadata_column = NULL, 
                          table_metadata = NULL,
                          channel = NULL, 
                          schema = NULL,
                          share_with_all_users = TRUE) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Initial Error Checks
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Error Checks for NULLs
  for (ivar in c("x", "table_name", "schema", 
                 "metadata_column", "table_metadata")) 
    if (is.null(x = get(x = ivar))) 
      stop(paste0("Must provide argument `", ivar, "`."))
  
  ## Check that metadata_column is a dataframe with columns "colname", 
  ## "colname_long", "units", "datatype", and "colname_desc"
  if (!is.data.frame(x = metadata_column)){
    stop(
      "Argument `metadata_column` must be a data.frame.
       See gapindex::oracle_upload() for how to format `metadata_column`.")
  } else {
    if (!all(c("colname", "colname_long", "units", "datatype", "colname_desc")
             %in% names(x = metadata_column))) 
      stop(
        "Argument `metadata_column` must be a string data.frame with fields:
        1) colname: name of field in the data table.
        2) colname_long: longer version of name for printing purposes.
        3) units: units of field.
        4) datatype: Oracle data type.
        5) colname_desc: Full description of field.
        
        See GAP_PRODUCTS.METADATA_COLUMN for example formats")
  }
  
  ## Check that x is a data.frame. If a path is provided, read the path.
  ## Make sure field names are capitalized.
  if (is.character(x = x)) {
    x <- utils::read.csv(file = x)
    names(x) <- toupper(names(x))
  }
  if (!is.data.frame(x = x)) 
    stop("Please supply a data.frame for argument `x`")
  
  ## Check that table_name is in all caps
  table_name <- toupper(x = table_name)
  schema <- toupper(x = schema)
  names(x = x) <- toupper(x = names(x))
  
  ## Check that there is a connection
  if (is.null(x = channel)) channel <- gapindex::get_connected()
  
  cat(paste0("Oracle Data Table: ", schema, ".", table_name, 
             "\nNumber of Rows: ", nrow(x = x), 
             "\nNumber of Fields with Metadata: ", 
             sum(!is.na(x = metadata_column$colname)), "\n\n"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Initiate table if new, 
  ##   If table already exits, drop table before overwriting
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## If table is currently in the argument `schema`, drop (delete) the table
  existing_tables <- RODBC::sqlTables(channel = channel, schema = schema)
  
  if (table_name %in% existing_tables$TABLE_NAME) {
    cat(paste0("Dropping table ", schema, ".", table_name, " ... \n"))
    
    RODBC::sqlDrop(channel = channel, sqtable = table_name, errors = FALSE)
    
  } 
  cat(paste0("Creating table: ", schema, ".", table_name, " ... "))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Upload table to Oracle
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  start_time <- Sys.time()
  ## Format metadata as a named vector to be inputted as argument 
  ## `varTypes` in RODBC::sqlSave()
  metadata_column <- subset(x = metadata_column,
                            subset = !is.na(x = colname))
  
  vartype_vec <- stats::setNames(object = metadata_column$datatype,
                                 nm = metadata_column$colname)
  
  ## Assign the dataframe `x` to the table_name
  assign(x = table_name, value = x)
  
  ## Upload table to Oracle
  sql_save_args <- list(channel = channel, 
                        dat = x, 
                        varTypes = vartype_vec, 
                        tablename = paste0(schema, ".", table_name), 
                        rownames = FALSE)
  
  do.call(what = RODBC::sqlSave, args = sql_save_args)
  
  end_time <- Sys.time()
  cat(paste("Time Elapsed:", round(end_time - start_time, 2), 
            units(end_time - start_time), "\n\n"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Update Metadata
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Updating Metadata ...\n")
  ## Add column metadata 
  
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
  
  ## Add table metadata 
  RODBC::sqlQuery(
    channel = channel,
    query = paste0('comment on table ', schema,'.', table_name, ' is \'',
                   table_metadata,'\';'))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Grant select access to all users
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (share_with_all_users) {
    cat("Granting select access to all users ... \n")
      RODBC::sqlQuery(channel = channel,
                      query = paste0('GRANT SELECT ON ', schema,'.', table_name,
                                     ' TO PUBLIC;'))
  }
  cat("Finished.\n\n")
}
