# Streamlined querying function using the DBI package

This function executes a SQL query regardless if the database connection
was created via RODBC or DBI/ODBC. combines the DBI::dbSendQuery() and
DBI::dbFetch() functions

## Usage

``` r
sql_query(channel, query)
```

## Arguments

- channel:

  A database connection either made via DBI::dbConnect(),
  odbc::dbConnect or RODBC::odbcConnect()

- query:

  A character string containing a SQL query.

## Value

if the query is successfully executed, a dataframe of the query will
result. Otherwise, an error message is returned as a string (or set of
strings).
