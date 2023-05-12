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
gapindex_data <- gapindex::get_data(year_set = (1982:2022),
                                    survey_set = "EBS",
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
gapindex_sizecomp <- gapindex::calc_sizecomp_bs_stratum(
  racebase_tables = gapindex_data,
  racebase_cpue = gapindex_cpue,
  racebase_stratum_popn = gapindex_biomass_stratum)
end_time <- Sys.time()
print(end_time - start_time)

## Calcualte age composition by stratum
start_time <- Sys.time()
gapindex_data_allhauls <- gapindex::get_data(year_set = (1982:2022),
                                             survey_set = "EBS",
                                             spp_codes = NULL, 
                                             sql_channel = sql_channel,
                                             pull_lengths = TRUE,
                                             abundance_haul = c("Y", "N"),
                                             good_performance = FALSE)
end_time <- Sys.time()
print(end_time - start_time)

start_time <- Sys.time()
gapindex_agecomp_stratum <- gapindex::calc_agecomp_stratum(
  racebase_tables = gapindex_data_allhauls,
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

## Pull temp table
temp_table <- 
  RODBC::sqlQuery(channel = sql_channel, 
                  query = paste0("SELECT * FROM HAEHNR.BIOMASS_EBS_PLUSNW"))

temp_table[, c("MEANWGTCPUE",  "MEANNUMCPUE")] <- 
  temp_table[, c("MEANWGTCPUE",  "MEANNUMCPUE")] * 100

temp_table[, c("VARMNWGTCPUE",  "VARMNNUMCPUE")] <- 
  temp_table[, c("VARMNWGTCPUE",  "VARMNNUMCPUE")] * 10000


## Lengthen Oracle table so that all the columns that we will compare are melted
## into one column called "METRIC". 
long_table <- reshape2::melt(
  data = temp_table, 
  id.vars = c("YEAR", "STRATUM", "SPECIES_CODE"),
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

rm(temp_table, long_table, long_gapindex, merged_long_table)

main_df_biomass_na <- subset(main_df_biomass, is.na(OLD.VALUE))
main_df_biomass <- subset(main_df_biomass, !is.na(OLD.VALUE))

main_df_biomass$GAPINDEX.VALUE_rounded <- 
  round(x = main_df_biomass$GAPINDEX.VALUE,
        digits = decimalplaces(main_df_biomass$OLD.VALUE))
main_df_biomass$ABS_DIFF <- 
  abs(main_df_biomass$OLD.VALUE - main_df_biomass$GAPINDEX.VALUE_rounded)

main_df_biomass$PERC_DIFF <- 
  ifelse(test = main_df_biomass$ABS_DIFF == 0,
         yes = 0,
         no = round(x = 100*(main_df_biomass$ABS_DIFF / 
                               main_df_biomass$GAPINDEX.VALUE_rounded),  
                    digits = 1))

main_df
nrow(subset(main_df_biomass, PERC_DIFF != 0))
head(subset(main_df_biomass, PERC_DIFF != 0))

errors_biomass <- subset(main_df_biomass, PERC_DIFF != 0)


## There are instances where 
unique_records_biomass <- 
  unique(x = errors_biomass[, c("YEAR", "STRATUM", "SPECIES_CODE")])
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
      "SELECT * FROM HAEHNR.BIOMASS_EBS_PLUSNW", 
      " WHERE SPECIES_CODE = ", unique_records_biomass$SPECIES_CODE[i], 
      " AND YEAR = ", unique_records_biomass$YEAR[i], 
      " AND STRATUM = ", unique_records_biomass$STRATUM[i]))
  
  if (temp_table$CATCOUNT != temp_table$NUMCOUNT) {
    unique_records_biomass$error_code[i] <- 1
    next
  }
  
  temp_gapindex_biomass <- 
    subset(x = gapindex_biomass_stratum, 
             YEAR == unique_records_biomass$YEAR[i] & 
             STRATUM == unique_records_biomass$STRATUM[i] & 
             SPECIES_CODE == unique_records_biomass$SPECIES_CODE[i])
  
  ## Check that the number of positive records in the CPUE table is equal 
  ## to the number of positive catch records in RACEBASE.CATCH. If not, assign
  ## error code = 2. These instances occur when changes happen in 
  ## RACEBASE.CATCH and then are not propagated to the CPUE table. 
  if (temp_gapindex_biomass$COUNT_CATCH != temp_table$CATCOUNT)
    unique_records_biomass$error_code[i] <- 3
  
}
rm(temp_gapindex_biomass, temp_table, i)

table(unique_records_biomass$error_code)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare SIZECOMPs
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main_df_sizecomps <- data.frame()

## Pull temp table
temp_table <- 
  RODBC::sqlQuery(channel = sql_channel, 
                  query = "SELECT * FROM HAEHNR.SIZECOMP_EBS_PLUSNW_STRATUM")

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
main_df_sizecomps <- merge(x = long_table,
                           by.x = c("YEAR", "STRATUM", "SEX",
                                    "LENGTH", "SPECIES_CODE"),
                           y = long_gapindex,
                           by.y = c("YEAR", "STRATUM", "SEX", 
                                    "LENGTH_MM", "SPECIES_CODE"))

rm(long_gapindex, long_table, temp_table)

main_df_sizecomps$PERC_DIFF <- 
  round(x = 100*(main_df_sizecomps$OLD.VALUE - main_df_sizecomps$GAPINDEX.VALUE) / 
          main_df_sizecomps$GAPINDEX.VALUE,  
        digits = 0)

head(subset(main_df_sizecomps, PERC_DIFF != 0))
nrow(subset(main_df_sizecomps, PERC_DIFF != 0))

errors_sizecomp <- subset(main_df_sizecomps, PERC_DIFF != 0)
nrow(errors_sizecomp)
errors_sizecomp$error_code <- 0

for (irow in 1:nrow(x = unique_records_biomass)) {
  
  idx <- which(errors_sizecomp$YEAR == unique_records_biomass$YEAR[irow] &
                 errors_sizecomp$STRATUM == unique_records_biomass$STRATUM[irow] &
                 errors_sizecomp$SPECIES_CODE == unique_records_biomass$SPECIES_CODE[irow])
  
  if (length(idx) > 0) {
    errors_sizecomp[idx, "error_code"] <- 
      unique_records_biomass$error_code[irow]
    next
  }
}
rm(idx)

for (irow in which(errors_sizecomp$error_code == 0)) {
  temp_sizecomp <- 
    subset(x = main_df_sizecomps, 
           subset = YEAR == errors_sizecomp$YEAR[irow] &
             SURVEY == errors_sizecomp$SURVEY[irow] &
             STRATUM == errors_sizecomp$STRATUM[irow] &
             SPECIES_CODE == errors_sizecomp$SPECIES_CODE[irow])
  
  temp_biomass <- 
    subset(main_df_biomass,
           METRIC == "POPULATION_COUNT" &
             YEAR == errors_sizecomp$YEAR[irow] &
             STRATUM == errors_sizecomp$STRATUM[irow] &
             SPECIES_CODE == errors_sizecomp$SPECIES_CODE[irow])
  
  abs(100 * (sum(temp_sizecomp$OLD.VALUE) - temp_biomass$OLD.VALUE) / 
        temp_biomass$OLD.VALUE) > 0.01

}
rm(irow, temp_sizecomp, temp_biomass)

table(errors_sizecomp$error_code)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Compare ages
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main_df_age <- data.frame()

temp_table <- 
  RODBC::sqlQuery(channel = sql_channel, 
                  query = "SELECT * FROM HAEHNR.AGECOMP_EBS_PLUSNW_STRATUM")
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
                 measure.vars = c("POPULATION_COUNT", 
                                  "LENGTH_MM_MEAN", 
                                  "LENGTH_MM_SD"),
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

errors_agecomp <- subset(x = main_df_age, subset = PERC_DIFF != 0)

unique_records_agecomp <- unique(errors_agecomp[, c("YEAR", "SPECIES_CODE",
                                                    "STRATUM")])
