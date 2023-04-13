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
##   Get age from Oracle
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (ireg in c("EBS_SHELF", "NBS_SHELF")[1]) {
  stratum_query <- paste0("select * from ", 
                          c("GOA" = "GOA.AGECOMP_TOTAL", 
                            "AI" = "AI.AGECOMP_TOTAL",
                            "EBS_SHELF" = "HAEHNR.AGECOMP_EBS_STANDARD_STRATUM where STRATUM = 999999"
                            # "NBS_SHELF" = "HAEHNR.SIZECOMP_NBS_STRATUM",
                            # "EBS_PLUSNW" = "HAEHNR.BIOMASS_EBS_PLUSNW"
                            )[ireg])
  
  db_age <- RODBC::sqlQuery(
    channel = sql_channel, 
    query = stratum_query)
  
  names(db_age)[c(1, 2, 6)] <- c("REGION", "YEAR", "agepop")
  # db_age <- cbind(REGION = ireg, 
  #                 db_age[, c('YEAR', 'STRATUM', 'SPECIES_CODE', 
  #                                    'LENGTH', 'MALES', 'FEMALES', 
  #                                    'UNSEXED', 'TOTAL')])
  
  assign(x = paste0(ireg, "_db_age"), value = db_age)
  
  print(paste0("Finished with ", ireg))
  rm(db_age, stratum_query, ireg)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Get sizecomps from package 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goa_spp <- RODBC::sqlQuery(channel = sql_channel, 
                           query = "select * from GOA.ANALYSIS_SPECIES")
for (ireg in c("AI", "GOA", "EBS_SHELF", "NBS_SHELF")[3]) {
  
  if (ireg %in% c("EBS_SHELF", "NBS_SHELF")) {
    species_of_interest <-
      data.frame(species_code = sort(unique(
        get(paste0(ireg, "_db_age"))$SPECIES_CODE)),
        group = sort(unique(
          get(paste0(ireg, "_db_age"))$SPECIES_CODE)))
  }
  
  
  if (ireg %in% c("GOA", "AI")){
    species_of_interest <-
      subset(x = goa_spp, subset = SIZECOMP_FLAG %in% c(ireg, "BOTH"))
    
    species_of_interest$species_code <- species_of_interest$group <-
      species_of_interest$SPECIES_CODE
    
    species_of_interest <-
      species_of_interest[order(species_of_interest$SPECIES_CODE),]
  }
  
  species_of_interest = data.frame(group = 10110, species_code = 10110)
  
  racebase_data <- AFSC.GAP.DBE::get_data( 
    year_set = sort(unique(get(paste0(ireg, "_db_age"))$YEAR)),
    survey_set = ireg,
    spp_codes = species_of_interest[, c("group", "species_code")],
    haul_type = 3,
    abundance_haul = c("Y"),
    sql_channel = sql_channel, 
    pull_lengths = T)
  
  assign(x = paste0(ireg, "_pck_data"), value = racebase_data)
  
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
  size_settings <- list(racebase_tables = racebase_data, 
                        racebase_cpue = racebase_cpue, 
                        racebase_stratum_popn = racebase_biomass_stratum)
  
  racebase_size <- do.call(what = switch(ireg, 
                                         "GOA" = "calc_size_stratum_AIGOA",
                                         "AI" = "calc_size_stratum_AIGOA",
                                         "EBS_SHELF" = "calc_size_stratum_BS",
                                         "NBS_SHELF" = "calc_size_stratum_BS"),
                           args = size_settings)
  
  assign(x = paste0(ireg, "_pck_size"), value = racebase_size)
  
  cat(paste0("Finished with ", ireg, "\n"))
  
  racebase_age <- calc_age_pop_AIGOA(racebase_tables = racebase_data,
                                     size_comp = racebase_size)
  assign(x = paste0(ireg, "_pck_age"), value = racebase_age)
  
  rm(racebase_biomass_stratum, racebase_cpue, racebase_data, racebase_size,
     species_of_interest, ireg)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Merge Datasets
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merged_df <- 
  merge(x = rbind(GOA_db_age[, c("REGION", "YEAR", "SPECIES_CODE", 
                                 "SEX", "AGE", "agepop")],
                  AI_db_age[, c("REGION", "YEAR", "SPECIES_CODE", 
                                "SEX", "AGE", "agepop")]#,
                  # EBS_SHELF_db_stratum#, NBS_SHELF_db_stratum
  ),
  y = rbind(cbind(REGION = "GOA", GOA_pck_age), 
            cbind(REGION = "AI", AI_pck_age)#,
            # cbind(REGION = "EBS_SHELF", EBS_SHELF_pck_stratum)#,
            # cbind(REGION = "NBS_SHELF", NBS_SHELF_pck_stratum)
  ),
  by = c("REGION", "YEAR", "SPECIES_CODE", "SEX","AGE"),
  all.x = TRUE)

merged_df <- subset(merged_df, !is.na(agepop.y))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Identify and remove records in Oracle but not in the package
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# na.recs <- subset(merged_df, is.na(TOTAL.y))
# merged_df <- subset(merged_df, !is.na(TOTAL.y))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merged_df$diff <- abs(merged_df$agepop.x - merged_df$agepop.y)
record_checks <- merged_df[merged_df$diff/merged_df$agepop.x * 100 > 1, ]
record_checks <- subset(record_checks, STRATUM != 999999)

table(record_checks$YEAR,  record_checks$SPECIES_CODE, record_checks$REGION)

check_these <- rbind(record_checks[, c("YEAR", "STRATUM", "REGION")][!duplicated(record_checks[, c("YEAR", "STRATUM", "REGION")]), ],
                     data.frame(YEAR = 2006, STRATUM = 323, REGION = 'AI'))

for (irow in 1:nrow(check_these)) {
  sum_from_sizecomp_db <- sum(subset(x = AI_db_stratum,
                                     subset = STRATUM == check_these$STRATUM[irow] &
                                       YEAR == check_these$YEAR[irow] &
                                       SPECIES_CODE == 10110,
                                     select = TOTAL))
  
  sum_from_sizecomp_pck <- sum(subset(x = AI_pck_stratum, 
                                      subset = STRATUM == check_these$STRATUM[irow] & 
                                        YEAR == check_these$YEAR[irow] & 
                                        SPECIES_CODE == 10110,
                                      select = TOTAL))
  
  stratum_query <- paste0("select STRATUM_POP FROM AI.BIOMASS_STRATUM WHERE ",
                          "SPECIES_CODE = 10110 AND ",
                          "STRATUM = ", check_these$STRATUM[irow], " AND ",
                          "YEAR = ", check_these$YEAR[irow] )
  
  sum_from_biomass <- RODBC::sqlQuery(
    channel = sql_channel, 
    query = stratum_query)
  
  check_these[irow, c("db", "pck", "biomass")] <- 
    c(sum_from_sizecomp_db, sum_from_sizecomp_pck, sum_from_biomass)
}
