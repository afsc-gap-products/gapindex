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
for (ireg in c("AI", "GOA", "EBS_SHELF", "NBS_SHELF")[3:4]) {
  stratum_query <- paste0("select * from ", 
                          c("GOA" = "GOA.SIZECOMP_STRATUM", 
                            "AI" = "AI.SIZECOMP_STRATUM",
                            "EBS_SHELF" = "HAEHNR.SIZECOMP_EBS_STANDARD_STRATUM",
                            "NBS_SHELF" = "HAEHNR.SIZECOMP_NBS_STRATUM",
                            "EBS_PLUSNW" = "HAEHNR.BIOMASS_EBS_PLUSNW")[ireg])
  
  db_stratum <- RODBC::sqlQuery(
    channel = sql_channel, 
    query = stratum_query)
  
  db_stratum <- cbind(REGION = ireg, 
                      db_stratum[, c('YEAR', 'STRATUM', 'SPECIES_CODE', 
                                     'LENGTH', 'MALES', 'FEMALES', 
                                     'UNSEXED', 'TOTAL')])
  
  assign(x = paste0(ireg, "_db_stratum"), value = db_stratum)
  
  print(paste0("Finished with ", ireg))
  rm(db_stratum, stratum_query, ireg)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Get sizecomps from package 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goa_spp <- RODBC::sqlQuery(channel = sql_channel, 
                           query = "select * from GOA.ANALYSIS_SPECIES")
for (ireg in c("AI", "GOA", "EBS_SHELF", "NBS_SHELF")[3:4]) {
  
  if (ireg %in% c("EBS_SHELF", "NBS_SHELF")) {
    species_of_interest <-
      data.frame(species_code = sort(unique(
        get(paste0(ireg, "_db_stratum"))$SPECIES_CODE)),
        group = sort(unique(
          get(paste0(ireg, "_db_stratum"))$SPECIES_CODE)))
  }
  
  
  if (ireg %in% c("GOA", "AI")){
    species_of_interest <-
      subset(x = goa_spp, subset = SIZECOMP_FLAG %in% c(ireg, "BOTH"))
    
    species_of_interest$species_code <- species_of_interest$group <-
      species_of_interest$SPECIES_CODE
    
    species_of_interest <-
      species_of_interest[order(species_of_interest$SPECIES_CODE),]
  }

  # species_of_interest = data.frame(group = c(30060, 21740, 10110),
  #                                  species_code = c(30060, 21740, 10110))
  # species_of_interest = data.frame(group = c(30060),
  #                                  species_code = c(30060))
  
  racebase_data <- AFSC.GAP.DBE::get_data(
    year_set = sort(unique(get(paste0(ireg, "_db_stratum"))$YEAR)),
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
  
  racebase_tables = racebase_data
  racebase_cpue = racebase_cpue
  racebase_stratum_popn = racebase_biomass_stratum
  
  cat("Calculating Size comp...\n")
  size_settings <- list(racebase_tables = racebase_data, 
                        racebase_cpue = racebase_cpue, 
                        racebase_stratum_popn = racebase_biomass_stratum)
  
  # racebase_size <- calc_size_stratum_AIGOA(racebase_tables = racebase_data, 
  #                                          racebase_cpue = racebase_cpue, 
  #                                          racebase_stratum_popn = racebase_biomass_stratum)
  
  racebase_size <- do.call(what = switch(ireg,
                                         "GOA" = "calc_size_stratum_AIGOA",
                                         "AI" = "calc_size_stratum_AIGOA",
                                         "EBS_SHELF" = "calc_size_stratum_BS",
                                         "NBS_SHELF" = "calc_size_stratum_BS"),
                           args = size_settings)
  
  assign(x = paste0(ireg, "_pck_stratum"), value = racebase_size)
  
  cat(paste0("Finished with ", ireg, "\n"))
  
  rm(racebase_biomass_stratum, racebase_cpue, racebase_data, racebase_size,
     species_of_interest, ireg)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Merge Datasets
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merged_df <- 
  merge(x = rbind(#GOA_db_stratum[, names(GOA_db_stratum)],
                  # AI_db_stratum[, names(AI_db_stratum)],
                  EBS_SHELF_db_stratum#, NBS_SHELF_db_stratum
  ),
  y = rbind(#cbind(REGION = "GOA", GOA_pck_stratum), 
            #cbind(REGION = "AI", AI_pck_stratum)#,
            cbind(REGION = "EBS_SHELF", EBS_SHELF_pck_stratum),
            cbind(REGION = "NBS_SHELF", NBS_SHELF_pck_stratum)
  ),
  by = c("REGION", "YEAR", "STRATUM", "SPECIES_CODE", "LENGTH"), 
  all.y = TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Identify and remove records in Oracle but not in the package
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
na.recs <- subset(merged_df, is.na(TOTAL.y))
merged_df <- subset(merged_df, !is.na(TOTAL.y))

## What is the 999999 stratum in the SIZECOMP_EBS_STANDARD_STRATUM? 
## What does a -9 length mean?

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Take out records where we know they are different:
##      In the GOA and AI, oracle tables assume NA catch is zero, 
##      negatively biasing the numbers per area swept calculation
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
num_fish_na <- rbind(cbind(REGION = "GOA", 
                           GOA_pck_cpue[is.na(GOA_pck_cpue$NUMBER_FISH), 
                                        c("YEAR", "STRATUM", "SPECIES_CODE")]),
                     cbind(REGION = "AI",
                           AI_pck_cpue[is.na(AI_pck_cpue$NUMBER_FISH),
                                       c("YEAR", "STRATUM", "SPECIES_CODE")])
)
num_fish_na_idx <- c()

for (i in 1:nrow(num_fish_na)) {
  num_fish_na_idx <- 
    c(num_fish_na_idx, 
      which(merged_df$REGION == num_fish_na$REGION[i] & 
              merged_df$YEAR == num_fish_na$YEAR[i] & 
              merged_df$STRATUM == num_fish_na$STRATUM[i] & 
              merged_df$SPECIES_CODE == num_fish_na$SPECIES_CODE[i]))
}
merged_df <- merged_df[-unique(num_fish_na_idx), ]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Take out records where we know they are different:
##      In the GOA and AI, there are some hauls that are included in the 
##      CPUE calculations in GOA.CPUE that should not be included due to 
##      having a negative performance code, ABUDNANCE_HAUL == "N" or no effort 
##      was not calculated. 
##
## AI: 1 haul in 2022 is incorrectly included in the Oracle dataset, has a 
## negative performance code, ABUNDANCE_HAUL == "N", (-21552). 1 haul in 1980 
## does not have effort information, ABUNDANCE_HAUL == "N" assume CPUE of zero
## because the weight was also zero. c(30165)
##
## GOA: 2 hauls in 1984 and 1 haul in 1987 don’t have effort information, 
## ABUNDANCE_HAUL == “N”, assume CPUE of zero because the weight was also zero.
## c(32450, 32710, 33554)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
num_fish_na <- 
  data.frame(REGION = c("AI", "AI", 
                        "GOA", "GOA", "GOA"
                        ),
             YEAR = c(1980, 2022, 
                      1984, 1984, 1987
                      ),
             STRATUM = c(423, 313, 
                         20, 111, 21
                         ),
             NOTE = c("NO EFFORT", "NEG PERFORM", 
                      "NO EFFORT", "NO EFFORT", "NO EFFORT"
                      ))

num_fish_na_idx <- c()

for (i in 1:nrow(num_fish_na)) {
  num_fish_na_idx <- 
    c(num_fish_na_idx, 
      which(merged_df$REGION == num_fish_na$REGION[i] & 
              merged_df$YEAR == num_fish_na$YEAR[i] & 
              merged_df$STRATUM == num_fish_na$STRATUM[i] ))
}
merged_df <- merged_df[-num_fish_na_idx, ]


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merged_df$diff <- abs(merged_df$TOTAL.x - merged_df$TOTAL.y)
record_checks <- merged_df[merged_df$diff > 1, ]
record_checks <- subset(record_checks, STRATUM != 999999)

table(record_checks$SPECIES_CODE, record_checks$YEAR, record_checks$REGION)

## For those records still unmatched, check to see first whether the total size
## composition matches the total abundance