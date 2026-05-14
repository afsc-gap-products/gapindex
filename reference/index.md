# Package index

## Get Data

- [`get_connected()`](get_connected.md) : Define RODBC connection to
  Oracle
- [`get_data()`](get_data.md) : Pull AFSC GAP BTS survey data
- [`get_station_info()`](get_station_info.md) : Pull station/grid data

## Design-Based Index and Composition Estimation

- [`calc_cpue()`](calc_cpue.md) : Calculate haul-level catch per unit
  effort
- [`calc_biomass_stratum()`](calc_biomass_stratum.md) : Calculate index
  of total biomass per stratum
- [`calc_biomass_subarea()`](calc_biomass_subarea.md) : Calculate index
  of total biomass across aggregated subareas
- [`calc_sizecomp_stratum()`](calc_sizecomp_stratum.md) : Calculate
  numerical CPUE at length by haul
- [`calc_sizecomp_subarea()`](calc_sizecomp_subarea.md) : Calculate size
  composition across aggregated subareas
- [`calc_alk()`](calc_ALK.md) : Calculate age-length key (alk)
- [`calc_agecomp_stratum()`](calc_agecomp_stratum.md) : Calculate
  stratum-level age composition and mean/std.dev length at age
- [`calc_agecomp_region()`](calc_agecomp_region.md) : Calculate
  region-level age composition and mean/std.dev length at age
- [`do_one_bootstrap()`](do_one_bootstrap.md) : Bootstrap a CPUE table
  from gapindex
- [`run_bootstraps()`](run_bootstraps.md) : Bootstrap haul data and
  return index information

## Utility Functions

- [`upload_oracle()`](upload_oracle.md) : Upload a table to Oracle with
  associated metadata
- [`stitch_entries()`](stitch_entries.md) : Helper function to format
  sql queries
- [`remove_temp_tables()`](remove_temp_tables.md) : Drop Temporary
  Tables Created within the get_data() call
