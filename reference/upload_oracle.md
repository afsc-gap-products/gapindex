# Upload a table to Oracle with associated metadata

Upload a table to Oracle with associated metadata

## Usage

``` r
upload_oracle(
  x = NULL,
  table_name = NULL,
  metadata_column = NULL,
  table_metadata = NULL,
  channel = NULL,
  schema = NULL,
  share_with_all_users = TRUE
)
```

## Arguments

- x:

  Either a character string of the path to the .csv file or the
  data.frame of the table to upload.

- table_name:

  Name of the table. (Add error checks to make sure table name follows
  any peculiarities of Oracle tables).

- metadata_column:

  data.frame describing the metadata for each of the fields in the
  table. Must contain these columns: 1) colname: name of field 2)
  colname_long: longer version of name for printing purposes. 3) units:
  units of field 4) dataype: Oracle data type 5) colname_desc: Full
  description of field

- table_metadata:

  Description of what the table is.

- channel:

  Establish your oracle connection using a function like
  [`gapindex::get_connected()`](get_connected.md).

- schema:

  character string. The name of the schema to save table.

- share_with_all_users:

  boolean. Default = TRUE. Give all users in Oracle select permissions.
