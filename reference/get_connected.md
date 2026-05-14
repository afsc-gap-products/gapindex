# Define RODBC connection to Oracle

Creates the RODBC connection to Oracle needed to pull SQL queries from
RACE database. Make sure you are connected to the VPN before running the
function. Also support users who use the Rpackage `keyring` to store
usernames and passwords.

## Usage

``` r
get_connected(db = "AFSC", check_access = TRUE)
```

## Arguments

- db:

  string. A registered data source name, in this case "AFSC" by default.
  This argument is passed to the `dsn` argument in
  [`RODBC::odbcConnect()`](https://rdrr.io/pkg/RODBC/man/odbcConnect.html)

- check_access:

  boolean. If TRUE (by default), checks whether you have the specific
  tables in GAP_PRODUCTS, RACEBASE and RACE_DATA used in the gapindex
  package. Outputs an error if the user does not have access to these
  tables with a message of the point of contact information for access.

## Value

channel of class "RODBC". See `?RODBC::odbcConnect()` for more detail
