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

sql_channel <- gapindex::get_connected()

## Create temporary folder to put downloaded VAST files. The temp/ folder
## is in the gitignore file. 
if (!dir.exists(paths = "temp")) dir.create(path = "temp")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Calculate biomass/abundance
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
start_time <- Sys.time()
production_data <- 
  gapindex::get_data(
    year_set = c(2018:2022),
    survey_set = c("AI", "GOA",
                   "EBS", "NBS"),
    spp_codes = NULL,
    pull_lengths = TRUE, 
    haul_type = 3, abundance_haul = "Y",
    sql_channel = sql_channel)
end_time <- Sys.time()
end_time - start_time 

start_time <- Sys.time()
production_cpue <- 
  gapindex::calc_cpue(racebase_tables = production_data)
end_time <- Sys.time()
end_time - start_time 

production_cpue_write <- 
  subset(x = production_cpue, 
         select = c(HAULJOIN, SPECIES_CODE, WEIGHT_KG, COUNT, AREA_SWEPT_KM2,
                    CPUE_KGKM2, CPUE_NOKM2) )

start_time <- Sys.time()
production_biomass_stratum <- 
  gapindex::calc_biomass_stratum(racebase_tables = production_data,
                                 cpue = production_cpue)
end_time <- Sys.time()
end_time - start_time 

start_time <- Sys.time()
production_agg_biomass <- 
  gapindex::calc_biomass_subarea(racebase_tables = production_data, 
                                 biomass_strata = production_biomass_stratum)
end_time <- Sys.time()
end_time - start_time 


sizecomp_bs_stratum <- 
  calc_sizecomp_bs_stratum(racebase_tables = production_data, 
                           racebase_cpue = production_cpue, 
                           racebase_stratum_popn = production_biomass_stratum)
sizecomp_aigoa_stratum <- 
  calc_sizecomp_aigoa_stratum(racebase_tables = production_data, 
                              racebase_cpue = production_cpue, 
                              racebase_stratum_popn = production_biomass_stratum)

start_time <- Sys.time()
production_agecomp <- 
  calc_agecomp_stratum(racebase_tables = production_data,
                       size_comp = rbind(sizecomp_bs_stratum,
                                         sizecomp_aigoa_stratum))
end_time <- Sys.time()
end_time - start_time

production_cpue <- production_cpue_write

names(x = production_biomass_stratum)[
  names(x = production_biomass_stratum) == "STRATUM"
  ] <- "AREA_ID"

production_biomass <- rbind(production_biomass_stratum[, names(production_agg_biomass)],
                            production_agg_biomass)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create metadata
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
metadata_id <- googledrive::as_id(x = "https://docs.google.com/spreadsheets/d/1wgAJPPWif1CC01iT2S6ZtoYlhOM0RSGFXS9LUggdLLA/edit#gid=1204955465")
googledrive::drive_download(file = metadata_id, path = "temp/metadata.xlsx" )
main_metadata_columns <- readxl::read_xlsx("temp/metadata.xlsx" , 
                                           sheet = "METADATA_COLUMN",
                                           skip = 1)
main_metadata_tables_info <- readxl::read_xlsx("temp/metadata.xlsx" , 
                                               sheet = "METADATA_TABLE",
                                               skip = 1)
repo_name <- "www.github.com/afsc-gap-products/gapindex"
today_date <- Sys.Date()

for (idata in c("production_cpue", "production_biomass", 
                "production_sizecomp", "production_agecomp")[1]) {
  match_idx <- match(x = names(get(idata)), 
                     table = toupper(x = main_metadata_columns$METADATA_COLNAME))
  
  metadata_columns <- 
    with(main_metadata_columns,
         data.frame( colname = toupper(METADATA_COLNAME[match_idx]), 
                     colname_long = METADATA_COLNAME_LONG[match_idx], 
                     units = METADATA_UNITS[match_idx], 
                     datatype = METADATA_DATATYPE[match_idx], 
                     colname_desc = METADATA_COLNAME_DESC[match_idx]))
  
  table_metadata <- "This is a test table."
  
  idx <- data.frame(from = seq(from = 1, 
                               to = nrow(x = get(x = idata)), 
                               by = 10000),
                    to = c(seq(from = 1, 
                             to = nrow(x = get(x = idata)), 
                             by = 10000)[-1] - 1, nrow(x = get(x = idata))))
  append_table <- FALSE
  
  for (irow in 457:nrow(x = idx) ) {
    upload_oracle(channel = sql_channel,
                  # x = get(idata)[idx$from[irow]:idx$to[irow], ],
                  x = get(idata)[4567817:nrow(x = get(idata)), ],
                  schema = "GAP_PRODUCTS",
                  table_name = toupper(idata),
                  table_metadata = table_metadata,
                  metadata_column = metadata_columns,
                  append_table = append_table, 
                  update_metadata = TRUE)
    append_table <- TRUE
  }

}
