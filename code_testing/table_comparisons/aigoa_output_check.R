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

options(scipen = 999)
decimalplaces <- function(x) {
  # split each number by the decimal point
  x_split <- strsplit(x = as.character(x = x), split = ".", fixed = TRUE)
  
  # count the number of characters after the decimal point
  x_digits <- sapply(X = x_split, 
                     FUN = function(x) ifelse(test = length(x) > 1, 
                                              yes = nchar(x[[2]]), 
                                              no = 0))
  
  # print the number of digits after the decimal point for each number
  return(x_digits)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Toggle region and pull biomass/abundance data from gapindex
##   Calculate/zero-fill CPUE data
##   Calculate stratum and subarea/region-level abundance/biomass
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Pull data
start_time <- Sys.time()
gapindex_data <- gapindex::get_data(year_set = (1980:2022),
                                    survey_set = c("GOA", "AI"),
                                    spp_codes = NULL, 
                                    sql_channel = sql_channel,
                                    pull_lengths = TRUE, 
                                    good_performance = TRUE, 
                                    abundance_haul = "Y")
end_time <- Sys.time()
print(end_time - start_time)

## Calculate and zero-fill CPUE
start_time <- Sys.time()
gapindex_cpue <- gapindex::calc_cpue(racebase_tables = gapindex_data)
end_time <- Sys.time()
print(end_time - start_time)

## Calcualte biomass by strata
start_time <- Sys.time()
gapindex_biomass_stratum <- 
  gapindex::calc_biomass_stratum(racebase_tables = gapindex_data, 
                                 cpue = gapindex_cpue, 
                                 vulnerability = 1)
end_time <- Sys.time()
print(end_time - start_time)

## Calculate size composition by stratum
start_time <- Sys.time()
gapindex_sizecomp <- gapindex::calc_sizecomp_aigoa_stratum(
  racebase_tables = gapindex_data, 
  racebase_cpue = gapindex_cpue, 
  racebase_stratum_popn = gapindex_biomass_stratum)
end_time <- Sys.time()
print(end_time - start_time)

start_time <- Sys.time()
gapindex_agecomp_stratum <- calc_agecomp_stratum(
  racebase_tables = gapindex_data,
  size_comp = gapindex_sizecomp)
end_time <- Sys.time()
print(end_time - start_time)

start_time <- Sys.time()
gapindex_agecomp_region <- gapindex::calc_agecomp_stratum(
  racebase_tables = gapindex_data,
  size_comp = gapindex_sizecomp)
end_time <- Sys.time()
print(end_time - start_time)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import metric fields lookup tables
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

metric_fields <- as.data.frame(x = readxl::read_xlsx(
  path = "Y:/RACE_GF/Oyafuso/data_sources.xlsx", 
  sheet = "field_lookup"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare BIOMASS across strata
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Result datafrme to append comparisons 
main_df_biomass <- data.frame()

for (ireg in c("GOA", "AI")) { ## Loop over region -- start
  
  ## Pull temp table
  temp_table <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM ", 
                                   ireg, ".BIOMASS_STRATUM" ))
  
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
    reshape2::melt(data = gapindex_biomass_stratum,
                   id.vars = c("YEAR", "STRATUM", "SPECIES_CODE"),
                   measure.vars = metric_fields$new_name,
                   variable.name = "METRIC", 
                   value.name = "GAPINDEX.VALUE")
  
  ## Merge the Oracle table with the table from gapindex, using "YEAR", 
  ## "SPECIES_CODE", "METRIC", "AREA_ID" as a composite key. 
  merged_long_table <- merge(x = long_table,
                             by.x = c("YEAR", "STRATUM", 
                                      "SPECIES_CODE", "METRIC"),
                             y = long_gapindex,
                             by.y = c("YEAR", "STRATUM", 
                                      "SPECIES_CODE", "METRIC"))
  
  ## Merge to `main_df_biomass`
  main_df_biomass <- rbind(main_df_biomass, unique(merged_long_table))
} ## Loop over region -- start

rm(temp_table, long_table, long_gapindex, merged_long_table)

main_df_biomass$GAPINDEX.VALUE.ROUNDED <- 
  round(x = main_df_biomass$GAPINDEX.VALUE,
        digits = decimalplaces(x = main_df_biomass$OLD.VALUE))

main_df_biomass$PERC_DIFF <- 
  round(x = 100*(main_df_biomass$OLD.VALUE - main_df_biomass$GAPINDEX.VALUE.ROUNDED) / 
          main_df_biomass$GAPINDEX.VALUE.ROUNDED,  
        digits = 0)
nrow(subset(main_df_biomass, PERC_DIFF != 0))
head(subset(main_df_biomass, PERC_DIFF != 0))


errors_biomass <- subset(main_df_biomass, PERC_DIFF != 0)

## There are instances where 
unique_records_biomass <- unique(x = errors_biomass[, c("YEAR", "SURVEY", "STRATUM", "SPECIES_CODE")])
unique_records_biomass$error_code <- 0


for (i in 1:nrow(x = unique_records_biomass)) {
  
  if (i%%100 == 0) {
    print(paste0("Done with ", i, " of ", nrow(unique_records_biomass)))
  }
  
  temp_table <- RODBC::sqlQuery(
    channel = sql_channel, 
    query = paste0("SELECT STRATUM, SPECIES_CODE, YEAR, HAUL_COUNT FROM ", 
                   unique_records_biomass$SURVEY[i], 
                   ".BIOMASS_STRATUM WHERE YEAR = ", unique_records_biomass$YEAR[i],
                   " AND STRATUM = ", unique_records_biomass$STRATUM[i],
                   " AND SPECIES_CODE = ", unique_records_biomass$SPECIES_CODE[i]) )
  
  temp_gapindex_biomass <- 
    subset(x = gapindex_biomass_stratum, 
           subset = SURVEY == unique_records_biomass$SURVEY[i] &
             YEAR == unique_records_biomass$YEAR[i] & 
             STRATUM == unique_records_biomass$STRATUM[i] & 
             SPECIES_CODE == unique_records_biomass$SPECIES_CODE[i])
  
  temp_cpue <- RODBC::sqlQuery(
    channel = sql_channel, 
    query = paste0("SELECT SURVEY, YEAR, HAULJOIN, STRATUM, WEIGHT, NUMBER_FISH FROM ", 
                   unique_records_biomass$SURVEY[i], 
                   ".CPUE WHERE YEAR = ", unique_records_biomass$YEAR[i],
                   " AND STRATUM = ", unique_records_biomass$STRATUM[i],
                   " AND SPECIES_CODE = ", unique_records_biomass$SPECIES_CODE[i]) )
  temp_catch <- 
    RODBC::sqlQuery(
      channel = sql_channel, 
      query = paste0("SELECT HAULJOIN, SPECIES_CODE, WEIGHT, NUMBER_FISH FROM RACEBASE.CATCH WHERE HAULJOIN IN ", 
                     gapindex::stitch_entries(temp_cpue$HAULJOIN), 
                     " AND SPECIES_CODE = ", unique_records_biomass$SPECIES_CODE[i]) )
  
  ## Check whether there are records where thee is positive catch rate with 
  ## zero counts recorded. If so, error_code == 1
  if (any(temp_cpue$NUMBER_FISH == 0 & temp_cpue$WEIGHT > 0)) {
    unique_records_biomass$error_code[i] <- 1
    next
  }
  
  ## Check whether the number of positive records in the CPUE table equals
  ## the number of positive catch records in RACEBASE.CATCH. 
  ## If not, error_code == 2. This happens when records change in 
  ## RACEBASE.CATCH but since we don't rerun biomass calculations, those
  ## changes are not propagated to the CPUE table, which causes the mismatch
  ## with the values in gapindex. 
  if (nrow(x = temp_catch) != 
      length(x = temp_cpue$WEIGHT[temp_cpue$WEIGHT > 0])) {
    unique_records_biomass$error_code[i] <- 2 
    next
  }
  
  ##
  if (temp_table$HAUL_COUNT != temp_gapindex_biomass$COUNT_HAUL) {
    unique_records_biomass$error_code[i] <- 3 
    next
  }
  
  ## Check whether there were changes in the number of fish in RACEBASE.CATCH
  ## that were not propagated to the CPUE table becuase we don't rerun 
  ## biomass calculations every year. If there was a change, that will cause 
  ## a mismatch with the gapindex output. Set error_code = 4 
  if ( sum(temp_catch$NUMBER_FISH) != sum(temp_cpue$NUMBER_FISH) ) {
    unique_records_biomass$error_code[i] <- 4
    next
  }
}

# for (irow in which(unique_records_biomass$error_code == 0)) {
#   temp_sub <- 
#     subset(x = errors_biomass,
#            subset = YEAR == unique_records_biomass$YEAR[irow] &
#              SURVEY == unique_records_biomass$SURVEY[irow] &
#              STRATUM == unique_records_biomass$STRATUM[irow] &
#              SPECIES_CODE == unique_records_biomass$SPECIES_CODE[irow])[1, ]
#   if (temp_sub$OLD.VALUE == round(x = temp_sub$GAPINDEX.VALUE,
#                                   digits = decimalplaces(temp_sub$OLD.VALUE)))
#     unique_records_biomass$error_code[irow] <- 5
#   
# }

table(unique_records_biomass$error_code)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare SIZECOMPs
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main_df_sizecomps <- data.frame()

for (ireg in c("GOA", "AI")) {
  
  ## Pull temp table
  temp_table <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM ", 
                                   ireg, ".SIZECOMP_STRATUM"))
  
  ## Lengthen Oracle table so that all the columns that we will compare are melted
  ## into one column called "METRIC". 
  long_table <- reshape2::melt(
    data = temp_table, 
    id.vars = c("SURVEY", "YEAR", "LENGTH",
                "STRATUM", "SPECIES_CODE"),
    measure.vars = c("MALES", "FEMALES", "UNSEXED"),
    variable.name = "SEX", value.name = "OLD.VALUE")
  long_table$SEX <- c("MALES" = 1, "FEMALES" = 2, "UNSEXED" = 3)[long_table$SEX]
  
  ## Lengthen table produced from gapindex so that all the columns that we 
  ## will compare are melted into one column called "METRIC". 
  long_gapindex <- subset(gapindex_sizecomp, SURVEY == ireg)
  names(long_gapindex)[names(long_gapindex) == "POPULATION_COUNT"] <- 
    "GAPINDEX.VALUE"
  
  ## Merge the Oracle table with the table from gapindex, using "YEAR", 
  ## "SPECIES_CODE", "METRIC", "AREA_ID" as a composite key. 
  merged_long_table <- merge(x = long_table,
                             by.x = c("SURVEY", "YEAR", "STRATUM", "SEX",
                                      "LENGTH", "SPECIES_CODE"),
                             y = long_gapindex,
                             by.y = c("SURVEY", "YEAR", "STRATUM", "SEX", 
                                      "LENGTH_MM", "SPECIES_CODE"))
  
  main_df_sizecomps <- rbind(main_df_sizecomps, merged_long_table)
  
}

main_df_sizecomps$PERC_DIFF <- 
  round(x = 100*(main_df_sizecomps$OLD.VALUE - main_df_sizecomps$GAPINDEX.VALUE) / 
          main_df_sizecomps$GAPINDEX.VALUE,  
        digits = 0)

head(subset(main_df_sizecomps, PERC_DIFF != 0))
nrow(subset(main_df_sizecomps, PERC_DIFF != 0))

errors_sizecomp <- subset(main_df_sizecomps, PERC_DIFF != 0)
nrow(errors_sizecomp)
# errors_sizecomp$error_code <- 0

unique_records_sizecomps <- 
  unique(x = subset(x = errors_sizecomp, 
                    select = c("YEAR", "SURVEY", "STRATUM", "SPECIES_CODE")) )
unique_records_sizecomps$error_code <- 0

for (irow in 1:nrow(x = unique_records_biomass)) {
  
  idx <- which(unique_records_sizecomps$YEAR == unique_records_biomass$YEAR[irow] &
                 unique_records_sizecomps$SURVEY == unique_records_biomass$SURVEY[irow] &
                 unique_records_sizecomps$STRATUM == unique_records_biomass$STRATUM[irow] &
                 unique_records_sizecomps$SPECIES_CODE == unique_records_biomass$SPECIES_CODE[irow])
  unique_records_sizecomps[idx, "error_code"] <- unique_records_biomass$error_code[irow] 
}

table(unique_records_sizecomps$error_code)

for (irow in which(unique_records_sizecomps$error_code == 0)) {
  temp_sub <- 
    subset(x = main_df_sizecomps,
           subset = YEAR == unique_records_sizecomps$YEAR[irow] &
             SURVEY == unique_records_sizecomps$SURVEY[irow] &
             STRATUM == unique_records_sizecomps$STRATUM[irow] &
             SPECIES_CODE == unique_records_sizecomps$SPECIES_CODE[irow])
  temp_count <- 
    subset(x = main_df_biomass,
           subset = YEAR == unique_records_sizecomps$YEAR[irow] &
             SURVEY == unique_records_sizecomps$SURVEY[irow] &
             STRATUM == unique_records_sizecomps$STRATUM[irow] &
             SPECIES_CODE == unique_records_sizecomps$SPECIES_CODE[irow] &
             METRIC == "POPULATION_COUNT")
  
  error_6 <- round(100 * (sum(temp_sub$OLD.VALUE) - temp_count$OLD.VALUE) / temp_count$OLD.VALUE, 2)
  if (error_6 != 0 ) 
    unique_records_sizecomps$error_code[irow] <- 6
  
  # print(error_6)
}
table(unique_records_sizecomps$error_code)
subset(unique_records_sizecomps, error_code == 0)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare ages
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main_df_age <- data.frame()

for (ireg in c("GOA", "AI")[]) {
  
  ## Pull temp table
  temp_table <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM ", 
                                   ireg, ".AGECOMP_TOTAL"))
  
  ## Lengthen Oracle table so that all the columns that we will compare are melted
  ## into one column called "METRIC". 
  long_table <- reshape2::melt(
    data = temp_table, 
    id.vars = c("SURVEY", "SURVEY_YEAR", "SPECIES_CODE", "SEX", "AGE"),
    measure.vars = "AGEPOP", 
    variable.name = "METRIC", value.name = "OLD.VALUE")
  
  ## Lengthen table produced from gapindex so that all the columns that we 
  ## will compare are melted into one column called "METRIC". 
  long_gapindex <- 
    reshape2::melt(data = gapindex_agecomp_region,
                   id.vars = c("YEAR", "SPECIES_CODE", "SEX", "AGE"),
                   measure.vars = c("AGEPOP", "LENGTH_MM_MEAN", "LENGTH_MM_SD"),
                   variable.name = "METRIC", value.name = "GAPINDEX.VALUE")
  
  ## Merge the Oracle table with the table from gapindex, using "YEAR", 
  ## "SPECIES_CODE", "METRIC", "AREA_ID" as a composite key. 
  merged_long_table <- merge(x = long_table,
                             by.x = c("SURVEY_YEAR", "SPECIES_CODE", 
                                      "SEX", "AGE", "METRIC"),
                             y = long_gapindex,
                             by.y = c("YEAR", "SPECIES_CODE", 
                                      "SEX", "AGE", "METRIC"))
  merged_long_table$EXISTING_TABLE <- paste0(ireg, ".AGECOMP_TOTAL")
  
  main_df <- rbind(main_df, merged_long_table)
  
}

main_df$PERC_DIFF <- 
  round(x = 100*(main_df$OLD.VALUE - main_df$GAPINDEX.VALUE) / 
          main_df$GAPINDEX.VALUE,  
        digits = 0)

nrow(subset(main_df, PERC_DIFF != 0))
head(subset(main_df, PERC_DIFF != 0))


errors <- subset(main_df, PERC_DIFF != 0)

nrow(errors)
for (irow in 1:nrow(x = error_cases)) {
  rm_idx <- which(errors$SURVEY == error_cases$SURVEY[irow] &
                    errors$SURVEY_YEAR == error_cases$YEAR[irow])
  if (length(x = rm_idx) > 0)  errors <- errors[-rm_idx, ]
}
nrow(errors)

subset(main_df, SURVEY == "GOA" & SURVEY_YEAR == 2007 & SPECIES_CODE == 10110)
## Check how AGE == -9, SEX == 3 calculations are done in agecomp calculations