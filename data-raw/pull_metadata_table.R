##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Pull Metadata Table from GAP_PROUDCTS.METADATA_COLUMNS
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import libraries
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(gapindex)
library(devtools)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Connect to Oracle and pull GAP_PRODUCTS.METADATA_COLUMN
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sql_channel <- gapindex::get_connected()

metadata_column <- 
  RODBC::sqlQuery(channel = sql_channel, 
                  query = "SELECT METADATA_COLNAME, METADATA_COLNAME_DESC
                  FROM GAP_PRODUCTS.METADATA_COLUMN")
names(x = metadata_column) <- c("Field", "Description")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Write to data-raw folder
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
utils::write.csv(x = metadata_column,
                 file = "data-raw/metadata.csv",
                 row.names = FALSE)
