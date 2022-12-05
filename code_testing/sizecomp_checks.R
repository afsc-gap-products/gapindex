##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Code testing
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:   Compare estimates from \
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import Library
##   Connect to Oracle
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(AFSC.GAP.DBE)
sql_channel <- AFSC.GAP.DBE::get_connected()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Get biomass stratum from Oracle
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (ireg in c("EBS_SHELF", "GOA")[1]) {
  stratum_query <- paste0("select * from ", 
                          c("GOA" = "GOA.SIZECOMP_STRATUM", 
                            "AI" = "AI.BIOMASS_STRATUM",
                            "EBS_SHELF" = "HAEHNR.SIZECOMP_EBS_STANDARD_STRATUM",
                            "EBS_PLUSNW" = "HAEHNR.BIOMASS_EBS_PLUSNW")[ireg])
  
  db_stratum <- RODBC::sqlQuery(
    channel = sql_channel, 
    query = stratum_query)
  
  assign(x = paste0(ireg, "_db_stratum"), value = db_stratum)
  
  print(paste0("Finished with ", ireg))
  rm(db_stratum, stratum_query, ireg)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Get sizecomps from package 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (ireg in c("EBS_SHELF", "GOA")[1]) {
  
  species_of_interest <- 
    data.frame(species_code = sort(unique(
      get(paste0(ireg, "_db_stratum"))$SPECIES_CODE)),
      group = sort(unique(
        get(paste0(ireg, "_db_stratum"))$SPECIES_CODE)))
  
  racebase_data <- AFSC.GAP.DBE::get_data( 
    year_set = sort(unique(get(paste0(ireg, "_db_stratum"))$YEAR)),
    survey_set = ireg,
    spp_codes = species_of_interest,
    haul_type = 3,
    abundance_haul = c("Y"),
    sql_channel = sql_channel, 
    pull_lengths = TRUE)
  
  cat("Calculating CPUE...\n")
  racebase_cpue <- AFSC.GAP.DBE::get_cpue(racebase_tables = racebase_data)
  racebase_cpue$SPECIES_CODE <- racebase_cpue$group
  
  assign(x = paste0(ireg, "_pck_cpue"), value = racebase_cpue)
  
  racebase_biomass_stratum <-
    AFSC.GAP.DBE::get_biomass_stratum(cpue = racebase_cpue,
                                      haul = racebase_data$haul,
                                      strata = racebase_data$strata,
                                      vulnerability = 1, 
                                      ci_val = 95)
  racebase_biomass_stratum$SPECIES_CODE <- racebase_biomass_stratum$group
  racebase_biomass_stratum$biomass_mt <- 
    round(x = racebase_biomass_stratum$biomass_mt, digits = 1)
  racebase_biomass_stratum$REGION <- ireg
  assign(value = racebase_biomass_stratum, 
         x = paste0(ireg, "_pck_stratum_biomass"))
  
  cat("Calculating Size comp...\n")
  racebase_size <- AFSC.GAP.DBE::calc_size_stratum(
    racebase_tables = racebase_data, 
    racebase_cpue = racebase_cpue, 
    racebase_stratum_stats = racebase_biomass_stratum)
  
  assign(x = paste0(ireg, "_pck_stratum"), value = racebase_size)
  
  cat(paste0("Finished with ", ireg, "\n"))
  
  rm(racebase_biomass_stratum, racebase_cpue, racebase_data, racebase_size,
     species_of_interest, ireg)
  
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Merge Datasets
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merged_df <- 
  merge(x = EBS_SHELF_db_stratum,
        y = EBS_SHELF_pck_stratum,
        by = c("YEAR", "STRATUM", "SPECIES_CODE", "LENGTH"), 
        all.x = TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Identify and remove records in Oracle but not in the package
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
na.recs <- subset(merged_df, is.na(TOTAL.y))
merged_df <- subset(merged_df, !is.na(TOTAL.y))

## What is the 999999 stratum in the SIZECOMP_EBS_STANDARD_STRATUM? 
## What does a -9 length mean?

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merged_df$diff <- abs(merged_df$TOTAL.x - merged_df$TOTAL.y)
record_checks <- merged_df[merged_df$diff > 1, ]
# 12% difference for pollock (21740) in 1991, STRATUM 61, males, 150 and 190 mm

table(unique(record_checks[, c("YEAR", "STRATUM", "SPECIES_CODE")])$YEAR)
