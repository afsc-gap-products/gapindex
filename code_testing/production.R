##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Standard Index Products
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:   Calculate CPUE, Biomass/Abundance, size composition and
##                age compositions for all species of interest. 
##                Write tables to GAP_PRODUCTS oracle schema. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import Libraries
##   Connect to Oracle (Make sure to connect to network or VPN)
##   Be sure to use the username and password for the GAP_PRODUCTS schema
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(gapindex)
library(googledrive)
library(RODBC)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Specify the range of years to calculate indices
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
range_of_years <- 1980:2022
regions <- c("AI", "GOA", "EBS", "NBS", "EBS_SLOPE")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create temporary folder to put downloaded metadata files. Double-check 
## that the temp/ folder is in the gitignore file. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!dir.exists(paths = "temp")) dir.create(path = "temp")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Authorize R to pull content from google drive
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
googledrive::drive_deauth()
googledrive::drive_auth()
1

sql_channel <- gapindex::get_connected()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import metadata
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
metadata_id <- googledrive::as_id(x = "https://docs.google.com/spreadsheets/d/1wgAJPPWif1CC01iT2S6ZtoYlhOM0RSGFXS9LUggdLLA/edit#gid=1204955465")
googledrive::drive_download(file = metadata_id, 
                            path = "temp/metadata.xlsx", 
                            overwrite = TRUE )
main_metadata_columns <- readxl::read_xlsx("temp/metadata.xlsx" , 
                                           sheet = "METADATA_COLUMN",
                                           skip = 1)
main_metadata_tables_info <- readxl::read_xlsx("temp/metadata.xlsx" , 
                                               sheet = "METADATA_TABLE",
                                               skip = 1)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Loop over regions
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (iregion in (1:length(x = regions))[5]) { ## Loop over regions -- start
  
  ## Pull data for all years and species from Oracle
  start_time <- Sys.time()
  production_data <- gapindex::get_data(year_set = range_of_years,
                                        survey_set = regions[iregion],
                                        spp_codes = NULL,
                                        pull_lengths = TRUE, 
                                        haul_type = 3, 
                                        abundance_haul = "Y",
                                        sql_channel = sql_channel)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ## Calculate and zero-fill CPUE
  cat("\nCalculating and zero-filling CPUE\n")
  start_time <- Sys.time()
  production_cpue <-
    gapindex::calc_cpue(racebase_tables = production_data)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ## Calculate biomass/abundance with associated variance, mean/variance CPUE
  ## across strata
  cat("\nCalculating biomass/abundance across strata\n")
  start_time <- Sys.time()
  production_biomass_stratum <-
    gapindex::calc_biomass_stratum(racebase_tables = production_data,
                                   cpue = production_cpue)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ## Aggregate `production_biomass_stratum` to subareas and regions
  cat("\nAggregate biomass/abundance to subareas and regions\n")
  start_time <- Sys.time()
  production_biomass_subarea <-
    gapindex::calc_biomass_subarea(racebase_tables = production_data,
                                   biomass_strata = production_biomass_stratum)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ## Calculate size composition by stratum. Since the two regions have
  ## different functions, sizecomp_fn toggles which function to use
  ## and then it is called in the do.call function.
  cat("\nCalculate size composition across strata\n")
  sizecomp_fn <- ifelse(test = regions[iregion] %in% c("GOA", "AI"),
                        yes = "calc_sizecomp_aigoa_stratum",
                        no = "calc_sizecomp_bs_stratum")
  sizecomp_args <- list(racebase_tables = production_data,
                        racebase_cpue = production_cpue,
                        racebase_stratum_popn = production_biomass_stratum)
  
  start_time <- Sys.time()
  production_sizecomp_stratum <- 
    do.call(what = sizecomp_fn, args = sizecomp_args)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ## Aggregate `production_sizecomp_stratum` to subareas and regions
  cat("\nAggregate size composition to subareas and regions\n")
  start_time <- Sys.time()
  production_sizecomp_subarea <- gapindex::calc_sizecomp_subareas(
    racebase_tables = production_data,
    size_comps = production_sizecomp_stratum)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ## Calculate age composition and mean/sd length at age
  cat("\nCalculate age composition across strata\n")
  start_time <- Sys.time()
  production_agecomp_stratum <-
    gapindex::calc_agecomp_stratum(racebase_tables = production_data,
                                   size_comp = production_sizecomp_stratum)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ## Aggregate `production_agecomp_stratum` to subareas and regions
  cat("\nAggregate age composition to regions\n")
  start_time <- Sys.time()
  production_agecomp <- #gapindex::
    calc_agecomp_region(
    racebase_tables = production_data,
    age_comps_stratum = production_agecomp_stratum)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ## Prep tables for Oracle
  cat("\nPrepping Tables for Oracle Uploading\n")
  
  ## Remove extra columns from `production_cpue`
  production_cpue <- subset(x = production_cpue,
                            select = c(HAULJOIN, SPECIES_CODE,
                                       WEIGHT_KG, COUNT, AREA_SWEPT_KM2,
                                       CPUE_KGKM2, CPUE_NOKM2) )
  
  ## Change "STRATUM" field name to "AREA_ID"
  names(x = production_biomass_stratum)[
    names(x = production_biomass_stratum) == "STRATUM"
  ] <- "AREA_ID"
  
  names(x = production_sizecomp_stratum)[
    names(x = production_sizecomp_stratum) == "STRATUM"
  ] <- "AREA_ID"
  
  ## Combine stratum, subarea, and region estimates for the biomass and 
  ## size composition tables
  production_biomass <- 
    rbind(production_biomass_stratum[, names(production_biomass_subarea)],
          production_biomass_subarea)
  
  production_sizecomp <- 
    rbind(production_sizecomp_subarea,
          production_sizecomp_stratum[, names(production_sizecomp_subarea)])
  
  ## Upload to Oracle
  cat("\nUploading to Oracle\n")
  for (idata in c("production_cpue", 
                  "production_biomass", 
                  "production_sizecomp", 
                  "production_agecomp")[3]) { ## Loop over table -- start
    
    append_table_ <- ifelse(test = iregion == 1, yes = FALSE, no = TRUE)
    
    match_idx <- 
      match(x = names(get(idata)), 
            table = toupper(x = main_metadata_columns$METADATA_COLNAME))
    
    metadata_columns <- 
      with(main_metadata_columns,
           data.frame( colname = toupper(METADATA_COLNAME[match_idx]), 
                       colname_long = METADATA_COLNAME_LONG[match_idx], 
                       units = METADATA_UNITS[match_idx], 
                       datatype = METADATA_DATATYPE[match_idx], 
                       colname_desc = METADATA_COLNAME_DESC[match_idx]))
    
    table_metadata <- "This is a test table."
    
    ## Large tables are split and uploaded in smaller chunks of 100000 
    ## records so that you can keep track of progress on SQL_Developer
    idx <- data.frame(from = seq(from = 1, 
                                 to = nrow(x = get(x = idata)), 
                                 by = 100000),
                      to = c(seq(from = 1, 
                                 to = nrow(x = get(x = idata)), 
                                 by = 100000)[-1] - 1, 
                             nrow(x = get(x = idata))))
    
    for (irow in 1:nrow(x = idx) ) { ## loop over chunk -- start
      gapindex::upload_oracle(
        channel = sql_channel,
        x = get(idata)[idx$from[irow]:idx$to[irow], ],
        schema = "GAP_PRODUCTS",
        table_name = toupper(x = gsub(x = idata,
                                      pattern = "production_",
                                      replacement = "")),
        # table_name = "NEW_CPUE",
        table_metadata = table_metadata,
        metadata_column = metadata_columns,
        append_table = append_table_, 
        update_metadata = TRUE)
      append_table_ <- TRUE
    }  ## loop over chunk -- end
    
  } ## Loop over table -- end
  
}  ## Loop over regions -- end
