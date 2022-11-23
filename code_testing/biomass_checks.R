##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Code testing
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:   Compare estimates from get_agg_biomass() and 
##                get_biomass_stratum().
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
for (ireg in c("GOA", "AI", "EBS_SHELF", "EBS_PLUSNW")) {
  
  stratum_query <- paste0("select * from ", 
                          c("GOA" = "GOA.BIOMASS_STRATUM", 
                            "AI" = "AI.BIOMASS_STRATUM",
                            "EBS_SHELF" = "HAEHNR.BIOMASS_EBS_STANDARD",
                            "EBS_PLUSNW" = "HAEHNR.BIOMASS_EBS_PLUSNW")[ireg])
  
  db_stratum <- RODBC::sqlQuery(
    channel = sql_channel, 
    query = stratum_query)
  
  db_stratum$REGION <- ireg
  
  bio_column <- list("GOA" = c("STRATUM_BIOMASS", "BIOMASS_VAR", 
                               "STRATUM_POP", "POP_VAR"), 
                     "AI" = c("STRATUM_BIOMASS", "BIOMASS_VAR", 
                              "STRATUM_POP", "POP_VAR"),
                     "EBS_SHELF" = c("BIOMASS", "VARBIO", 
                                     "POPULATION", "VARPOP"),
                     "EBS_PLUSNW" = c("BIOMASS", "VARBIO", 
                                      "POPULATION", "VARPOP"))[[ireg]]
  
  db_stratum <- db_stratum[, c("REGION", "SPECIES_CODE", "YEAR", "STRATUM",
                               bio_column)]
  names(db_stratum) <- c("REGION", "SPECIES_CODE", "YEAR", "STRATUM", 
                         "BIOMASS", "BIOMASS_VAR", "POP", "POP_VAR")
  
  assign(x = paste0(ireg, "_db_stratum"), value = db_stratum)
  
  print(paste0("Finished with ", ireg))
  rm(db_stratum, bio_column, stratum_query, ireg)
}

EBS_PLUSNW_db_stratum$REGION <- "EBS_SHELF"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Get GOA biomass from AFSC.GAP.DBE package
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (ireg in c("GOA", "AI", "EBS_SHELF")[]) {
  
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
    sql_channel = sql_channel)
  
  racebase_cpue <- AFSC.GAP.DBE::get_cpue(racebase_tables = racebase_data)
  racebase_cpue$SPECIES_CODE <- racebase_cpue$group
  
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
  
  racebase_biomass_stratum <- 
    racebase_biomass_stratum[, c("REGION","SPECIES_CODE", "YEAR", "STRATUM", 
                                 "biomass_mt", "biomass_var", 
                                 "pop", "pop_var")]
  names(racebase_biomass_stratum) <- c("REGION", "SPECIES_CODE", "YEAR", 
                                       "STRATUM", "BIOMASS", "BIOMASS_VAR", 
                                       "POP", "POP_VAR")
  
  assign(value = racebase_biomass_stratum, 
         x = paste0(ireg, "_biomass_stratum"))
  assign(value = racebase_cpue, 
         x = paste0(ireg, "_cpue"))
  
  rm(racebase_biomass_stratum, racebase_cpue, racebase_data, 
     species_of_interest, ireg)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Merge datasets together
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EBS_PLUSNW_biomass_stratum <- subset(EBS_SHELF_biomass_stratum,
                                     YEAR >= 1987 & STRATUM %in% c(82, 90)) 
EBS_SHELF_biomass_stratum <- subset(EBS_SHELF_biomass_stratum,
                                    !STRATUM %in% c(82, 90))

rpackage_stratum <- rbind(GOA_biomass_stratum, 
                          AI_biomass_stratum, 
                          EBS_SHELF_biomass_stratum,
                          EBS_PLUSNW_biomass_stratum)

oracle_stratum <- 
  rbind(GOA_db_stratum, 
        AI_db_stratum, 
        subset(x = EBS_SHELF_db_stratum, 
               subset = STRATUM %in% 
                 sort(unique(EBS_SHELF_biomass_stratum$STRATUM))),
        subset(x = EBS_PLUSNW_db_stratum, 
               subset = STRATUM %in% c(82, 90) & YEAR >= 1987))

merged_stratum <- merge(x = rpackage_stratum,
                        y = oracle_stratum, 
                        by = c("REGION", "YEAR", "STRATUM", "SPECIES_CODE"),
                        all.x = TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Take out records where we know they are different:
##      In the GOA and AI, oracle tables assume NA catch is zero, 
##      negatively biasing the numbers per area swept calculation
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
num_fish_na <- rbind(cbind(REGION = "GOA", 
                           GOA_cpue[is.na(GOA_cpue$NUMBER_FISH), 
                                    c("YEAR", "STRATUM", "SPECIES_CODE")]),
                     cbind(REGION = "AI", 
                           AI_cpue[is.na(AI_cpue$NUMBER_FISH), 
                                   c("YEAR", "STRATUM", "SPECIES_CODE")]))
num_fish_na_idx <- c()

for (i in 1:nrow(num_fish_na)) {
  num_fish_na_idx <- 
    c(num_fish_na_idx, 
      which(merged_stratum$REGION == num_fish_na$REGION[i] & 
              merged_stratum$YEAR == num_fish_na$YEAR[i] & 
              merged_stratum$STRATUM == num_fish_na$STRATUM[i] & 
              merged_stratum$SPECIES_CODE == num_fish_na$SPECIES_CODE[i]))
}
merged_stratum <- merged_stratum[-unique(num_fish_na_idx), ]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Take out records where we know they are different:
##      In the GOA and AI, oracle tables assume NA catch is zero, 
##      negatively biasing the numbers per area swept calculation
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
num_fish_na <- 
  data.frame(REGION = c("AI", "AI", "GOA", "GOA", "GOA"),
             YEAR = c(1980, 2022, 1984, 1984, 1987),
             STRATUM = c(423, 313, 20, 111, 21),
             NOTE = c("NO EFFORT", "NEG PERFORM", "NO EFFORT",
                      "NO EFFORT", "NO EFFORT"))

## HAULJOIN = -5799 AND SPECIES_CODE = 420: GOA.CPUE has NUMBER_FISH = 1 
## while RACEBASE.CATCH has NUMBER_FISH = 2

## HAULJOIN = 991071: record of a positive catch of SPECIES_CODE = 150 (unid. 
## shark) in GOA.CPUE is shown as a zero in RACEBASE.CATCH and instead as SPECIES_CODE = 222 
## 

## Population estimates for purple-orange seastar (81742) not consistent for 
## 1983 STRATUM  10. 

num_fish_na_idx <- c()

for (i in 1:nrow(num_fish_na)) {
  num_fish_na_idx <- 
    c(num_fish_na_idx, 
      which(merged_stratum$REGION == num_fish_na$REGION[i] & 
              merged_stratum$YEAR == num_fish_na$YEAR[i] & 
              merged_stratum$STRATUM == num_fish_na$STRATUM[i] ))
}
merged_stratum <- merged_stratum[-num_fish_na_idx, ]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare datasets
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merged_stratum$diff_biomass <- merged_stratum$POP.x - merged_stratum$POP.y

na_recs <- merged_stratum[is.na(merged_stratum$diff_biomass), ]
subset(na_recs, !SPECIES_CODE %in% c(10260:10262, 30150:30152))
merged_stratum <- merged_stratum[!is.na(merged_stratum$diff_biomass), ]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   What is the breakdown of the records in the rpackage results that are 
##   not in oracle?
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 10261, and 10262 are N. and S. rock sole and don't exist pre 1996, 1274 recs
## STRATUM 80 and 92 are not in the standard area, 1128 records.
## 10260 are rock soles (N and S merged), are not in oracle after 1996 for 
## the most part.
##
## 30151 and 30152 are dark and dusky rockfish and don't exist consistently
## in Oracle until 1996. 30150 is dark/dusky rockfish combined and don't exist
## after 1996. 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
record_checks <- merged_stratum[round(merged_stratum$diff_biomass) != 0, ]
# record_checks$diff_percent <- with(record_checks, 
#                                    round(100 * diff_biomass / POP.y) )
subset(record_checks, !is.na(REGION) & 
         !SPECIES_CODE %in% c(21200, 23000, 400, 23800, 66000, 
                                           79000, 78010, 79000, 21300))

## AI: 1 haul in 2022 is incorrectly included in the Oracle dataset, has a 
## negative performance code, ABUNDANCE_HAUL == “N”, (-21552). 1 haul in 1980 
## does not have effort information, ABUNDANCE_HAUL == “N” assume CPUE of zero
## because the weight was also zero. c(30165)

## GOA: 2 hauls in 1984 and 1 haul in 1987 don’t have effort information, 
## ABUNDANCE_HAUL == “N”, assume CPUE of zero because the weight was also zero.
## c(32450, 32710, 33554)
