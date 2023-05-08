##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import packages, connect to Oracle
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readxl)
library(gapindex)
library(reshape2)

sql_channel <- gapindex::get_connected()

decimalplaces <- Vectorize( function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', 
                       as.character(x)), ".", 
                   fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
})

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Toggle region and pull biomass/abundance data from gapindex
##   Calculate/zero-fill CPUE data
##   Calculate stratum and subarea/region-level abundance/biomass
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ireg = c("EBS")
ispp = 10110
iyear = 2010:2022

gapindex_data <- gapindex::get_data(year_set = iyear,
                                    survey_set = ireg,
                                    spp_codes = ispp, 
                                    sql_channel = sql_channel,
                                    pull_lengths = TRUE)

# gapindex_data_neg <- gapindex::get_data(year_set = iyear,
#                                         survey_set = ireg,
#                                         spp_codes = ispp, 
#                                         sql_channel = sql_channel,
#                                         PERFORMANCE = 
#                                         pull_lengths = TRUE)


gapindex_cpue <- gapindex::calc_cpue(racebase_tables = gapindex_data)
gapindex_biomass_stratum <- 
  gapindex::calc_biomass_stratum(racebase_tables = gapindex_data, 
                                 cpue = gapindex_cpue, 
                                 vulnerability = 1)
# gapindex_biomass_stratum$AREA_ID <- gapindex_biomass_stratum$STRATUM
# gapindex_biomass_subareas <- 
#   gapindex::calc_biomass_subarea(racebase_tables = gapindex_data, 
#                                  biomass_strata = gapindex_biomass_stratum)

# gapindex_biomass_allareas <- 
#   rbind(gapindex_biomass_stratum[, names(x = gapindex_biomass_subareas)],
#         gapindex_biomass_subareas)

gapindex_sizecomp_stratum <- gapindex::calc_sizecomp_bs_stratum(
  racebase_tables = gapindex_data, 
  racebase_cpue = gapindex_cpue, 
  racebase_stratum_popn = gapindex_biomass_stratum)

# gapindex_sizecomp_subareas <- gapindex::calc_(
#   racebase_tables = gapindex_data, 
#   racebase_cpue = gapindex_cpue, 
#   racebase_stratum_popn = gapindex_biomass_stratum)
# 
gapindex_agecomp_stratum <-
  calc_agecomp_stratum(racebase_tables = gapindex_data,
                       size_comp = gapindex_sizecomp_stratum)

# gapindex_agecomp_region <-
# calc_agecomp_region(racebase_tables = gapindex_data,
#                       age_comps_stratum = gapindex_agecomp_stratum)

names(gapindex_agecomp_stratum$age_comp)[
  names(gapindex_agecomp_stratum$age_comp) == "STRATUM"] <- "AREA_ID"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import metric fields lookup tables
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

metric_fields <- as.data.frame(x = readxl::read_xlsx(
  path = "code_testing/table_comparisons/data_sources.xlsx",
  sheet = "field_lookup"))

# data_sources <- as.data.frame(x = readxl::read_xlsx(
#   path = "code_testing/table_comparisons/data_sources.xlsx", 
#   sheet = "BIOMASS"))

## Stitch species and year vectors

ispp = gapindex::stitch_entries(stitch_what = ispp)
iyear = gapindex::stitch_entries(stitch_what = iyear)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare BIOMASS across strata
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Result datafrme to append comparisons 
main_df <- data.frame()

for (ireg in c("NBS", "EBS")[2]) { ## Loop over region -- start
  ## Pull temp table
  temp_table <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM HAEHNR.BIOMASS_", 
                                   switch(ireg, 
                                          "EBS" = "EBS_PLUSNW", 
                                          "NBS" = "NBS_SAFE"), 
                                   " WHERE SPECIES_CODE in ", ispp, 
                                   " AND YEAR in ", iyear))
  
  temp_table$STRATUM[temp_table$STRATUM == 999] <- 
    switch(ireg, "EBS" = 99900, "NBS" = 99902)
  temp_table$SURVEY = ireg
  temp_table$MEANWGTCPUE <- temp_table$MEANWGTCPUE * 100
  temp_table$VARMNWGTCPUE <- temp_table$VARMNWGTCPUE * 10000
  temp_table$MEANNUMCPUE <- temp_table$MEANNUMCPUE * 100
  temp_table$VARMNNUMCPUE <- temp_table$VARMNNUMCPUE * 10000
  
  
  temp_strata <- sort(x = unique(x = temp_table$STRATUM))
  
  ## Lengthen Oracle table so that all the columns that we will compare are melted
  ## into one column called "METRIC". 
  long_table <- reshape2::melt(
    data = temp_table, 
    id.vars = c("SURVEY", "YEAR", "STRATUM", "SPECIES_CODE"),
    measure.vars = names(x = temp_table)[names(x = temp_table) %in% 
                                           metric_fields$old_name], 
    variable.name = "METRIC", value.name = "OLD.VALUE")
  
  ## Convert name of the field to the new name so it can be compared to the
  ## biomass output tables from gapindex. 
  long_table$METRIC <- 
    metric_fields$new_name[match(x = long_table$METRIC, 
                                 table = metric_fields$old_name)]
  
  ## Lengthen table produced from gapindex so that all the columns that we 
  ## will compare are melted into one column called "METRIC". 
  long_gapindex <- 
    reshape2::melt(data = subset(x = gapindex_biomass_stratum, 
                                 subset = AREA_ID %in% temp_strata),
                   id.vars = c("YEAR", "AREA_ID", "SPECIES_CODE"),
                   measure.vars = metric_fields$new_name,
                   variable.name = "METRIC", value.name = "GAPINDEX.VALUE")
  
  ## Merge the Oracle table with the table from gapindex, using "YEAR", 
  ## "SPECIES_CODE", "METRIC", "AREA_ID" as a composite key. 
  merged_long_table <- merge(x = long_table,
                             by.x = c("YEAR", "STRATUM", 
                                      "SPECIES_CODE", "METRIC"),
                             y = long_gapindex,
                             by.y = c("YEAR", "AREA_ID", 
                                      "SPECIES_CODE", "METRIC"))
  
  names(x = merged_long_table)[names(x = merged_long_table) == "STRATUM"] <- 
    "AREA_ID"
  
  merged_long_table$EXISTING_TABLE <- 
    paste0("HAEHNR.BIOMASS_", switch(ireg, 
                                     "EBS" = "EBS_PLUSNW", 
                                     "NBS" = "NBS_SAFE"))
  
  ## Merge to `main_df`
  main_df <- rbind(main_df, unique(merged_long_table))
  
} ## Loop over region -- start

main_df$PERC_DIFF <- 
  round(x = 100*(main_df$OLD.VALUE - main_df$GAPINDEX.VALUE) / 
          main_df$GAPINDEX.VALUE,  
        digits = 0)

head(subset(main_df, PERC_DIFF != 0))
nrow(subset(main_df, PERC_DIFF != 0))

errors <- subset(main_df, PERC_DIFF != 0)

## Check to see that the errors are due to truncations of decimal places
errors$GAPINDEX.VALUE <- 
  round(x = errors$GAPINDEX.VALUE, 
        digits = decimalplaces(errors$OLD.VALUE))

errors$PERC_DIFF <- 
  round(x = 100*(errors$OLD.VALUE - errors$GAPINDEX.VALUE) / 
          errors$GAPINDEX.VALUE,  
        digits = 0)

head(subset(errors, PERC_DIFF != 0))
nrow(subset(errors, PERC_DIFF != 0))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare SIZECOMPs
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main_df <- data.frame()

for (ireg in c("NBS", "EBS")[2]) {
  
  ## Pull temp table
  temp_table <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM HAEHNR.SIZECOMP_", 
                                   switch(ireg, 
                                          "EBS" = "EBS_PLUSNW_STRATUM", 
                                          "NBS" = "NBS_STRATUM"),
                                   " WHERE SPECIES_CODE in ", ispp, 
                                   " AND YEAR in ", iyear))
  
  temp_table$STRATUM[temp_table$STRATUM == 999999] <- 
    switch(ireg, "EBS" = 99900, "NBS" = 99902)
  temp_table$SURVEY = ireg
  
  temp_strata <- sort(x = unique(x = temp_table$STRATUM ))
  
  ## Lengthen Oracle table so that all the columns that we will compare are melted
  ## into one column called "METRIC". 
  long_table <- reshape2::melt(
    data = temp_table, 
    id.vars = c("SURVEY", "YEAR", "LENGTH",
                "STRATUM", "SPECIES_CODE"),
    measure.vars = c("MALES", "FEMALES", "UNSEXED"),
    variable.name = "METRIC", value.name = "OLD.VALUE")
  
  ## Convert name of the field to the new name so it can be compared to the
  ## biomass output tables from gapindex. 
  # long_table$METRIC <- 
  # metric_fields$new_name[match(x = long_table$METRIC,
  # table = metric_fields$old_name)]
  
  ## Lengthen table produced from gapindex so that all the columns that we 
  ## will compare are melted into one column called "METRIC". 
  long_gapindex <- 
    reshape2::melt(data = subset(x = gapindex_sizecomp_stratum, 
                                 subset = STRATUM %in% temp_strata),
                   id.vars = c("YEAR", "STRATUM", "SPECIES_CODE", "LENGTH_MM"),
                   measure.vars = c("MALES", "FEMALES", "UNSEXED"),
                   variable.name = "METRIC", value.name = "GAPINDEX.VALUE")
  
  ## Merge the Oracle table with the table from gapindex, using "YEAR", 
  ## "SPECIES_CODE", "METRIC", "AREA_ID" as a composite key. 
  merged_long_table <- merge(x = long_table,
                             by.x = c("YEAR", "STRATUM", 
                                      "LENGTH", "SPECIES_CODE", "METRIC"),
                             y = long_gapindex,
                             by.y = c("YEAR", "STRATUM", "LENGTH_MM", 
                                      "SPECIES_CODE", "METRIC"))
  merged_long_table$EXISTING_TABLE <- paste0("HAEHNR.SIZECOMP_", 
                                             switch(ireg, 
                                                    "EBS" = "EBS_PLUSNW_STRATUM", 
                                                    "NBS" = "NBS_STRATUM"))
  
  main_df <- rbind(main_df, merged_long_table)
  
}

main_df$PERC_DIFF <- 
  round(x = 100*(main_df$OLD.VALUE - main_df$GAPINDEX.VALUE) / 
          main_df$GAPINDEX.VALUE,  
        digits = 0)

head(subset(main_df, PERC_DIFF != 0))
nrow(subset(main_df, PERC_DIFF != 0))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare ages
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main_df <- data.frame()

for (ireg in c("EBS_STANDARD", "EBS_PLUSNW", "NBS")[2]) {
  
  ## Pull temp table
  temp_table <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM HAEHNR.AGECOMP_", 
                                   ireg, "_STRATUM",
                                   " WHERE SPECIES_CODE in ", ispp,
                                   " AND YEAR in ", iyear))
  
  if (ireg == "EBS_STANDARD") temp_table <- subset(x = temp_table, 
                                                   subset = STRATUM == 999999)
  
  temp_table$STRATUM[temp_table$STRATUM == 999999] <- 
    switch(ireg, "EBS_PLUSNW" = 99900, "EBS_STANDARD" = 99901, "NBS" = 99902)
  temp_table$SURVEY <-     
    switch(ireg, "EBS_PLUSNW" = "EBS", "EBS_STANDARD" = "EBS", "NBS" = "NBS")
  
  ## Lengthen Oracle table so that all the columns that we will compare are melted
  ## into one column called "METRIC". 
  long_table <- reshape2::melt(
    data = temp_table, 
    id.vars = c("SURVEY", "YEAR", "STRATUM", "SPECIES_CODE", "SEX", "AGE"),
    measure.vars = c("AGEPOP", "MEANLEN", "SDEV"), 
    variable.name = "METRIC", 
    value.name = "OLD.VALUE")
  
  long_table$METRIC <-   c("AGEPOP" = "AGEPOP", 
                           "MEANLEN" = "LENGTH_MM_MEAN", 
                           "SDEV" = "LENGTH_MM_SD")[long_table$METRIC]
  
  ## Lengthen table produced from gapindex so that all the columns that we 
  ## will compare are melted into one column called "METRIC". 
  long_gapindex <- 
    reshape2::melt(data = rbind(#gapindex_agecomp_region, 
      gapindex_agecomp_stratum$age_comp),
      id.vars = c("YEAR", "SPECIES_CODE", "AREA_ID", "SEX", "AGE"),
      measure.vars = c("AGEPOP", "LENGTH_MM_MEAN", "LENGTH_MM_SD"),
      variable.name = "METRIC", 
      value.name = "GAPINDEX.VALUE")
  
  ## Merge the Oracle table with the table from gapindex, using "YEAR", 
  ## "SPECIES_CODE", "METRIC", "AREA_ID" as a composite key. 
  merged_long_table <- merge(x = long_table,
                             by.x = c("YEAR", "STRATUM", "SPECIES_CODE", 
                                      "SEX", "AGE", "METRIC"),
                             y = long_gapindex,
                             by.y = c("YEAR", "AREA_ID", "SPECIES_CODE", 
                                      "SEX", "AGE", "METRIC"),
                             all.x = TRUE)
  
  merged_long_table$EXISTING_TABLE <- paste0("HAEHNR.AGECOMP_", 
                                             ireg, "_STRATUM")
  
  main_df <- rbind(main_df, merged_long_table)
  
}

main_df$PERC_DIFF <- 
  round(x = 100*(round(main_df$OLD.VALUE) - round(main_df$GAPINDEX.VALUE)) / 
          round(main_df$GAPINDEX.VALUE),  
        digits = 0)

nrow(subset(main_df, PERC_DIFF != 0))
head(subset(main_df, PERC_DIFF != 0))
errors <- subset(main_df, PERC_DIFF != 0)

## Check to see that the errors are due to truncations of decimal places
errors$GAPINDEX.VALUE <-
  round(x = errors$GAPINDEX.VALUE,
        digits = decimalplaces(errors$OLD.VALUE))

errors$PERC_DIFF <-
  round(x = 100*(errors$OLD.VALUE - errors$GAPINDEX.VALUE) /
          errors$GAPINDEX.VALUE,
        digits = 0)

errors <- subset(errors, PERC_DIFF != 0)
head(subset(errors, PERC_DIFF != 0))
nrow(subset(errors, PERC_DIFF != 0))

table(errors$YEAR, errors$SEX)

