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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Helper function to match significant figures to prevent truncation errors
##   Set options to 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
##   Pull Data
##   Calculate/zero-fill CPUE data
##   Calculate stratum and subarea/region-level abundance/biomass
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Pull data
start_time <- Sys.time()
gapindex_data <- gapindex::get_data(year_set = 1980:2022,
                                    survey_set = c("NBS"),
                                    spp_codes = NULL, 
                                    sql_channel = sql_channel,
                                    pull_lengths = TRUE)
end_time <- Sys.time()
print(end_time - start_time)

## Calculate and zero-fill CPUE
start_time <- Sys.time()
gapindex_cpue <- gapindex::calc_cpue(racebase_tables = gapindex_data)
end_time <- Sys.time()
print(end_time - start_time)

## Calculate biomass by strata
start_time <- Sys.time()
gapindex_biomass_stratum <- 
  gapindex::calc_biomass_stratum(racebase_tables = gapindex_data,
                                 cpue = gapindex_cpue, 
                                 vulnerability = 1)
end_time <- Sys.time()
print(end_time - start_time)

## Calculate size composition by stratum
start_time <- Sys.time()
gapindex_sizecomp <- gapindex::calc_sizecomp_bs_stratum(
  racebase_tables = gapindex_data,
  racebase_cpue = gapindex_cpue,
  racebase_stratum_popn = gapindex_biomass_stratum)
end_time <- Sys.time()
print(end_time - start_time)

## Calcualte age composition by stratum
start_time <- Sys.time()
gapindex_agecomp_stratum <- gapindex::calc_agecomp_stratum(
  racebase_tables = gapindex_data,
  size_comp = gapindex_sizecomp)
end_time <- Sys.time()
print(end_time - start_time)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import metric fields lookup tables
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
metric_fields <- as.data.frame(x = readxl::read_xlsx(
  path = "G:/Oyafuso/data_sources.xlsx", 
  sheet = "field_lookup"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare BIOMASS across strata
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pull Oracle table
oracle_table <- 
  RODBC::sqlQuery(channel = sql_channel, 
                  query = "SELECT * FROM HAEHNR.BIOMASS_NBS_AKFIN")

## Lengthen Oracle table so that all the columns that we will compare are melted
## into one column called "METRIC". 
long_oracle_table <- reshape2::melt(
  data = oracle_table, 
  id.vars = c("SURVEY", "YEAR", "STRATUM", "SPECIES_CODE"),
  measure.vars = names(x = oracle_table)[names(x = oracle_table) %in% 
                                           metric_fields$old_name], 
  variable.name = "METRIC", value.name = "OLD.VALUE")

## Convert name of the field to the new name so it can be compared to the
## biomass output tables from gapindex. 
long_oracle_table$METRIC <- 
  metric_fields$new_name[match(x = long_oracle_table$METRIC, 
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
merged_long_oracle_table <- merge(x = long_oracle_table,
                                  by.x = c("YEAR", "STRATUM", 
                                           "SPECIES_CODE", "METRIC"),
                                  y = long_gapindex,
                                  by.y = c("YEAR", "STRATUM", 
                                           "SPECIES_CODE", "METRIC"))

## Merge to `main_df_biomass`
main_df_biomass <- unique(merged_long_oracle_table)
rm(oracle_table, long_oracle_table, long_gapindex, merged_long_oracle_table)

main_df_biomass_na <- subset(main_df_biomass, is.na(OLD.VALUE))
main_df_biomass <- subset(main_df_biomass, !is.na(OLD.VALUE))

main_df_biomass$GAPINDEX.VALUE_rounded <- 
  round(x = main_df_biomass$GAPINDEX.VALUE,
        digits = decimalplaces(main_df_biomass$OLD.VALUE))

main_df_biomass$PERC_DIFF <- 
  round(x = 100 * (main_df_biomass$OLD.VALUE - 
                   main_df_biomass$GAPINDEX.VALUE_rounded) / 
          main_df_biomass$OLD.VALUE,  
        digits = 0)
nrow(subset(main_df_biomass, PERC_DIFF != 0))
head(subset(main_df_biomass, PERC_DIFF != 0))

errors_biomass <- subset(main_df_biomass, PERC_DIFF != 0)

## Tabulate unique combinations of year/stratum/taxon where there are errors 
unique_records_biomass <- 
  unique(x = errors_biomass[, c("YEAR", "SURVEY", "STRATUM", "SPECIES_CODE")])
unique_records_biomass$error_code <- 0

for (i in 1:nrow(x = unique_records_biomass)) {
  ## Check whether the number of hauls with positive catch counts is equal 
  ## to the number of hauls with positive catch weights. In the Bering Sea 
  ## scripts, when this occurs, the variance of the numbers-CPUE is calculated
  ## incorrectly and is the source of the mismatch. Assign these instances
  ## with error code = 1.
  
  temp_table <- RODBC::sqlQuery(
    channel = sql_channel,
    query = paste0(
      "SELECT * FROM HAEHNR.BIOMASS_NBS_SAFE",
      " WHERE SPECIES_CODE = ", unique_records_biomass$SPECIES_CODE[i], 
      " AND YEAR = ", unique_records_biomass$YEAR[i], 
      " AND STRATUM = ", unique_records_biomass$STRATUM[i]))
  
  names(temp_table)[which(names(temp_table) %in% c("CATCH_COUNT", 
                                                   "NUMBER_COUNT"))] <-
    c("CATCOUNT", "NUMCOUNT")
  
  if (temp_table$CATCOUNT != temp_table$NUMCOUNT) {
    unique_records_biomass$error_code[i] <- 1
    next
  }
  
  subset(gapindex_biomass_stratum, 
         SPECIES_CODE == unique_records_biomass$SPECIES_CODE[i] &
           YEAR ==  unique_records_biomass$YEAR[i] &
           STRATUM == unique_records_biomass$STRATUM[i])
  mean(subset(gapindex_cpue, 
              SPECIES_CODE == unique_records_biomass$SPECIES_CODE[i] &
                YEAR ==  unique_records_biomass$YEAR[i] &
                STRATUM == unique_records_biomass$STRATUM[i])$CPUE_KGKM2)
  
  ## Check that the number of positive records in the CPUE table is equal 
  ## to the number of positive catch records in RACEBASE.CATCH. If not, assign
  ## error code = 2. These instances occur when changes happen in 
  ## RACEBASE.CATCH and then are not propagated to the CPUE table. 
  temp_cpue <- RODBC::sqlQuery(
    channel = sql_channel,
    query = paste0(
      "SELECT * FROM HAEHNR.CPUE_NBS",
      " WHERE SPECIES_CODE = ", unique_records_biomass$SPECIES_CODE[i],
      " AND YEAR = ", unique_records_biomass$YEAR[i],
      " AND STRATUM = ", unique_records_biomass$STRATUM[i]))
  
  temp_catch <-
    RODBC::sqlQuery(
      channel = sql_channel,
      query = paste0("SELECT * FROM RACEBASE.CATCH WHERE HAULJOIN IN ",
                     gapindex::stitch_entries(temp_cpue$HAULJOIN),
                     "AND SPECIES_CODE = ", 
                     unique_records_biomass$SPECIES_CODE[i]) )
  
  if (nrow(x = temp_catch) != sum(temp_cpue$CPUE_KGHA > 0)) {
    unique_records_biomass$error_code[i] <- 2 
    next
  }
}
rm(temp_catch, temp_cpue, i, table_name)

table(unique_records_biomass$error_code)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare SIZECOMPs
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main_df_sizecomps <- data.frame()

## Pull temp table
temp_table <- 
  RODBC::sqlQuery(channel = sql_channel, 
                  query = "SELECT * FROM HAEHNR.SIZECOMP_NBS_STRATUM")

## Set any records with length -9 as unsexed
temp_table$UNSEXED[temp_table$LENGTH == -9] <- 
  temp_table$TOTAL[temp_table$LENGTH == -9]

## Lengthen Oracle table so that all the columns that we will compare are melted
## into one column called "METRIC". 
long_table <- reshape2::melt(
  data = temp_table, 
  id.vars = c("YEAR", "LENGTH",
              "STRATUM", "SPECIES_CODE"),
  measure.vars = c("MALES", "FEMALES", "UNSEXED"),
  variable.name = "SEX", value.name = "OLD.VALUE")
long_table$SEX <- c("MALES" = 1, "FEMALES" = 2, "UNSEXED" = 3)[long_table$SEX]

## Lengthen table produced from gapindex so that all the columns that we 
## will compare are melted into one column called "METRIC". 
long_gapindex <- gapindex_sizecomp
names(long_gapindex)[names(long_gapindex) == "POPULATION_COUNT"] <- 
  "GAPINDEX.VALUE"

## Merge the Oracle table with the table from gapindex, using "YEAR", 
## "SPECIES_CODE", "METRIC", "AREA_ID" as a composite key. 
merged_long_table <- merge(x = long_table,
                           by.x = c("YEAR", "STRATUM", "SEX",
                                    "LENGTH", "SPECIES_CODE"),
                           y = long_gapindex,
                           by.y = c("YEAR", "STRATUM", "SEX", 
                                    "LENGTH_MM", "SPECIES_CODE"))

main_df_sizecomps <- rbind(main_df_sizecomps, merged_long_table)

rm(merged_long_table, long_gapindex, long_table, temp_table)

main_df_sizecomps$PERC_DIFF <- 
  round(x = 100*(main_df_sizecomps$OLD.VALUE - main_df_sizecomps$GAPINDEX.VALUE) / 
          main_df_sizecomps$GAPINDEX.VALUE,  
        digits = 0)

nrow(subset(main_df_sizecomps, PERC_DIFF != 0))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare ages
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main_df_age <- data.frame()

temp_table <- 
  RODBC::sqlQuery(channel = sql_channel, 
                  query = "SELECT * FROM HAEHNR.AGECOMP_NBS_STRATUM")
names(temp_table)[names(temp_table) == "AGEPOP"] <- "POPULATION_COUNT"

## Lengthen Oracle table so that all the columns that we will compare are melted
## into one column called "METRIC". 
long_table <- reshape2::melt(
  data = temp_table, 
  id.vars = c("YEAR", "STRATUM", "SPECIES_CODE", "SEX", "AGE"),
  measure.vars = "POPULATION_COUNT", 
  variable.name = "METRIC", value.name = "OLD.VALUE")

## Lengthen table produced from gapindex so that all the columns that we 
## will compare are melted into one column called "METRIC". 
long_gapindex <- 
  reshape2::melt(data = gapindex_agecomp_stratum$age_comp,
                 id.vars = c("YEAR", "STRATUM", "SPECIES_CODE", "SEX", "AGE"),
                 measure.vars = c("POPULATION_COUNT", "LENGTH_MM_MEAN", "LENGTH_MM_SD"),
                 variable.name = "METRIC", value.name = "GAPINDEX.VALUE")

## Merge the Oracle table with the table from gapindex, using "YEAR", 
## "SPECIES_CODE", "METRIC", "AREA_ID" as a composite key. 
main_df_age <- merge(x = long_table,
                           by.x = c("YEAR", "SPECIES_CODE", "STRATUM", 
                                    "SEX", "AGE", "METRIC"),
                           y = long_gapindex,
                           by.y = c("YEAR", "SPECIES_CODE", "STRATUM",
                                    "SEX", "AGE", "METRIC"))

rm(long_gapindex, long_table, temp_table)

main_df_age$PERC_DIFF <- 
  round(x = 100*(round(main_df_age$OLD.VALUE) - main_df_age$GAPINDEX.VALUE) / 
          main_df_age$GAPINDEX.VALUE,  
        digits = 0)

nrow(subset(main_df_age, PERC_DIFF != 0))
