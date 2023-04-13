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
library(AFSC.GAP.DBE)
sql_channel <- AFSC.GAP.DBE::get_connected()

## Create temporary folder to put downloaded VAST files. The temp/ folder
## is in the gitignore file. 
if (!dir.exists(paths = "temp")) dir.create(path = "temp")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Calculate biomass/abundance
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
start_time <- Sys.time()
production_data <- 
  AFSC.GAP.DBE::get_data(
    year_set = c(2018:2022),
    survey_set = c("AI", "GOA",
                   "EBS", "NBS"),
    spp_codes = NULL,
    pull_lengths = F, 
    haul_type = 3, abundance_haul = "Y",
    sql_channel = sql_channel)
end_time <- Sys.time()
end_time - start_time 

start_time <- Sys.time()
production_cpue <- 
  AFSC.GAP.DBE::calc_cpue(racebase_tables = production_data)
end_time <- Sys.time()
end_time - start_time 

production_cpue_write <- 
  subset(x = production_cpue, 
         select = c(HAULJOIN, SPECIES_CODE, WEIGHT_KG, COUNT, AREASWEPT_KM2,
                    CPUE_KGKM2, CPUE_NOKM2) )

start_time <- Sys.time()
production_biomass_stratum <- 
  AFSC.GAP.DBE::calc_biomass_stratum(racebase_tables = production_data,
                                     cpue = production_cpue)
end_time <- Sys.time()
end_time - start_time 

start_time <- Sys.time()
production_agg_biomass <- 
  calc_agg_biomass(racebase_tables = production_data, 
                   biomass_strata = production_biomass_stratum)
end_time <- Sys.time()
end_time - start_time 

production_biomass <- rbind(production_biomass_stratum,
                            production_agg_biomass)

write.csv(x = production_biomass,
          file = "temp/production_biomass.csv",
          row.names = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Calculate size comps separately for each region
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_cruisejoins <- production_data$cruise$CRUISEJOIN[
  production_data$cruise$SURVEY %in% c("EBS", "NBS")]
bs_cruisejoins_vec <- AFSC.GAP.DBE::stitch_entries(stitch_what = bs_cruisejoins) 

sizecomp_bs_taxa <- sort(x = unlist(
  RODBC::sqlQuery(channel = sql_channel, 
                  query = paste0("SELECT DISTINCT SPECIES_CODE FROM ",
                                 "RACEBASE.LENGTH WHERE ", 
                                 "CRUISEJOIN IN ", bs_cruisejoins_vec)),
  use.names = FALSE))

aigoa_cruisejoins <- production_data$cruise$CRUISEJOIN[
  production_data$cruise$SURVEY %in% c("AI", "GOA")]
aigoa_cruisejoins_vec <- 
  AFSC.GAP.DBE::stitch_entries(stitch_what = aigoa_cruisejoins) 

sizecomp_aigoa_taxa <- sort(x = unlist(
  RODBC::sqlQuery(channel = sql_channel, 
                  query = paste0("SELECT DISTINCT SPECIES_CODE FROM ",
                                 "RACEBASE.LENGTH WHERE ", 
                                 "CRUISEJOIN IN ", aigoa_cruisejoins_vec)),
  use.names = FALSE))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Production Size comps
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
production_data_size <-  
  AFSC.GAP.DBE::get_data(year_set = c(2018:2022),
                         survey_set = c("AI", "GOA", "EBS", "NBS"),
                         spp_codes = sizecomp_aigoa_taxa,
                         pull_lengths = TRUE, 
                         haul_type = 3, abundance_haul = "Y",
                         sql_channel = sql_channel)

production_cpue_size <-  
  AFSC.GAP.DBE::calc_cpue(racebase_tables = production_data_size)

production_biomass_size <- 
  AFSC.GAP.DBE::calc_biomass_stratum(racebase_tables = production_data_size,
                                     cpue = production_cpue_size)

production_sizecomp_aigoa <- 
  AFSC.GAP.DBE::calc_size_stratum_AIGOA(racebase_tables = production_data_size, 
                                        racebase_cpue = production_cpue_size, 
                                        racebase_stratum_popn = production_biomass_size)

production_sizecomp_bs <- 
  AFSC.GAP.DBE::calc_size_stratum_BS(racebase_tables = production_data_size, 
                                     racebase_cpue = production_cpue_size, 
                                     racebase_stratum_popn = production_biomass_size)

production_sizecomp_agg <- AFSC.GAP.DBE::calc_agg_size_comp(
  racebase_tables = production_data_size, 
  size_comps = rbind(production_sizecomp_aigoa,
                     production_sizecomp_bs))

production_sizecomp_all <- rbind(production_sizecomp_aigoa,
                                 production_sizecomp_bs)
production_sizecomp_all$AREA_ID <- production_sizecomp_all$STRATUM

production_sizecomp_all <- rbind(production_sizecomp_all[, names(production_sizecomp_agg)], 
                                 production_sizecomp_agg)

production_agecomp <- 
  calc_age_comp(racebase_tables = production_data_size,
                size_comp = rbind(production_sizecomp_aigoa,
                                  production_sizecomp_bs))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create metadata
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

main_metadata_columns <- 
  RODBC::sqlQuery(channel = sql_channel, 
                  query = "SELECT * FROM GAP_PRODUCTS.METADATA_COLUMN")

match_idx <- match(x = names(production_cpue), 
                   table = toupper(x = main_metadata_columns$METADATA_COLNAME))

metadata_columns <- 
  with(main_metadata_columns,
       data.frame( colname = toupper(METADATA_COLNAME[match_idx]), 
                   colname_long = METADATA_COLNAME_LONG[match_idx], 
                   units = METADATA_UNITS[match_idx], 
                   datatype = METADATA_DATATYPE[match_idx], 
                   colname_desc = METADATA_COLNAME_DESC[match_idx]))

table_metadata <- "This is a test table."

sql_channel <- AFSC.GAP.DBE::get_connected()
AFSC.GAP.DBE::upload_oracle(channel = sql_channel,
                            x = production_cpue_write,
                            schema = "GAP_PRODUCTS",
                            table_name = "PRODUCTION_CPUE",
                            table_metadata = table_metadata,
                            metadata_column = metadata_columns,
                            update_table = TRUE, 
                            update_metadata = TRUE)

head( RODBC::sqlQuery(channel = sql_channel, 
                      query = "SELECT * FROM GAP_PRODUCTS.PRODUCTION_BIOMASS"))

