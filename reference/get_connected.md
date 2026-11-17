# Define Oracle connection to Oracle

Creates the database connection to Oracle needed to pull SQL queries
from RACE database. Make sure you are connected to the VPN before
running the function. Also support users who use the Rpackage `keyring`
to store usernames and passwords.

## Usage

``` r
get_connected(
  db = "AFSC",
  conn_type = c("RODBC", "DBI")[1],
  check_access = TRUE,
  check_expiry = TRUE,
  warn_days = 7
)
```

## Arguments

- db:

  string. A registered data source name, in this case "AFSC" by default.
  This argument is passed to the `dsn` argument in
  [`RODBC::odbcConnect()`](https://rdrr.io/pkg/RODBC/man/odbcConnect.html)

- conn_type:

  string. Which package should be used to set up the database
  connection. Default is "RODBC" but "DBI" can also be used.

- check_access:

  boolean. If TRUE (by default), checks whether you have the specific
  tables in GAP_PRODUCTS, RACEBASE and RACE_DATA used in the gapindex
  package. Outputs an error if the user does not have access to these
  tables with a message of the point of contact information for access.

- check_expiry:

  boolean. If TRUE (by default), checks whether the user's Oracle
  password is in its grace period or within `warn_days` of expiring.

- warn_days:

  numeric. Number of days before password expiration to trigger a
  warning message. Default is 7.

## Value

channel either of class "RODBC" or "Oracle". See `?RODBC::odbcConnect()`
or `?DBI::dbConnect()`for more detail
