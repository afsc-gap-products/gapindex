# gapindex 2.2.0

* Modified the get_connected() function to specify the user to enter AFSC Oracle database credentials, per issue #45. Also added functionality for integrating the keyring R package to input credentials, per commit 3f9e0b3aaace9c377130762f76fadddd6acfcfd6. Also added check to make sure user has access to the relevant tables in the RACEBASE and RACE_DATA schemata and an error message to contact nmfs.afsc.gap.metadata@@noaa.gov, per commit 1505097. 
* added `taxonomic_source` argument to the get_data() function to allow users to toggle between using RACEBASE vs GAP_PRODUCTS for taxonomic information, per commit 9544050558c060db33579c0fc76aecf9af5ab12b. Defaults to RACEBASE.SPECIES.
