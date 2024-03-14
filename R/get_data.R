#' Pull AFSC GAP BTS survey data
#' 
#' @description Pulls survey, cruise, haul, catch, size, specimen (age), 
#'              stratum, subarea, stratum grouping information for the 
#'              region, years, and species of interest from the RACEBASE,
#'              RACE_DATA, and GAP_PRODUCTS schemata in the AFSC Oracle database. 
#' 
#' @param year_set  numeric or integer vector of years
#' @param survey_set character string. One of c("GOA", "AI", "EBS", "NBS", "BSS"). 
#' @param spp_codes two-column data.frame of species codes (column name 
#'                  SPECIES_CODE) and GROUP_CODE name (column name GROUP_CODE). 
#'                  For single-species, the GROUP_CODE and species codes can be the
#'                  same. Examples: 
#'                  1) For a mixture of individual taxa and taxon groups: 
#'                  data.frame("SPECIES_CODE" = c(21720, 21220, 21230, 21232), 
#'                  "GROUP_CODE" = c(21720, "Grenadiers", "Grenadiers", "Grenadiers"))
#'                  2) For single taxa: c(21720, 21740, 10110) 
#'                  3) FOR PRODUCTION PURPOSES: a NULL value by default uses 
#'                  GAP_PRODUCTS.TAXON_GROUPS from the AFSC Oracle database. 
#' @param haul_type integer. Defaults to haul type "3" for "standard bottom 
#'                  sample (preprogrammed station)" used for production purposes. 
#' @param abundance_haul character string. "Y" are standardized hauls used in 
#'                       production and "N" are non-standard hauls due to 
#'                       bad performance, different gear, etc.
#' @param na_rm_strata `r lifecycle::badge("deprecated")` Use the 
#'                     `remove_na_strata` argument instead. 
#' @param remove_na_strata boolean. Remove hauls with NA stratum information.
#'                         Defaults to FALSE. 
#' @param sql_channel  `r lifecycle::badge("deprecated")` Use the `channel` 
#'                     argument instead.
#' @param channel      connection to Oracle created via 
#'                     gapindex::get_connected() or RODBC::odbcConnect().
#' @param pull_lengths boolean T/F. Should length and specimen data be pulled? 
#'                     Defaults to FALSE for speed.
#' 
#' @return a named list containing survey_design, survey, cruise, haul, catch, 
#'         size (if pull_lengths == TRUE), specimen (if pull_lengths == TRUE), 
#'         species, stratum, subarea, and stratum_groups information for the 
#'         survey, years, and species of interest. 
#' 
#' @export
#'  

# library(data.table)
# year_set = c(1996, 1999)
# survey_set = c("GOA")
# spp_codes = c(21720, 30060, 10110)
# haul_type = 3
# abundance_haul = c("Y", "N")[1]
# remove_na_strata = FALSE
# channel = gapindex::get_connected(check_access = F)
# pull_lengths = T

get_data <- function(year_set = c(1996, 1999),
                     survey_set = c("GOA"),
                     spp_codes = c(21720, 30060, 10110),
                     haul_type = 3,
                     abundance_haul = c("Y", "N")[1],
					 pull_lengths = FALSE,
                     remove_na_strata = FALSE,
                     channel = NULL,
                     sql_channel = deprecated(),
					 na_rm_strata = deprecated()) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   1) Set up channel if channel = NULL
  ##      Clear schema of temporary tables created in this function if present 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (lifecycle::is_present(sql_channel)) {
    lifecycle::deprecate_warn("2.2.0", 
                              "get_data(sql_channel)", 
                              "get_data(channel)")
    channel <- sql_channel
  }
    if (lifecycle::is_present(na_rm_strata)) {
    lifecycle::deprecate_warn("2.2.0", 
                              "get_data(na_rm_strata)", 
                              "get_data(remove_na_strata)")
    remove_na_strata <- na_rm_strata
  }
  
  if (is.null(x = channel)) channel <- gapindex::get_connected()
  
  for (itable in c("AVAIL_SPP", "CATCH", "CRUISE", "HAUL", "INPUT_SPP", 
                   "SIZE", "SPECIMEN", "STRATUM", "STRATUM_GROUPS", "SURVEY",
                   "SURVEY_DESIGN", "SUBAREA", "USER_TAXONOMIC_INFO", 
                   "UNAVAIL_SPP", "USER_INPUT_SPP")) {
    
    ## check if temporary table exists and if so...
    if (nrow(x = RODBC::sqlQuery(channel = channel,
                                 query = paste0("SELECT table_name
                                   FROM user_tables
                                   WHERE TABLE_NAME = 'GAPINDEX_TEMPORARY_",  
                                                itable, "_QUERY'"))) != 0)
      
      ## ...drop the table
      RODBC::sqlQuery(channel = channel, 
                      query = paste0("DROP TABLE ", "GAPINDEX_TEMPORARY_", 
                                     itable, "_QUERY"))
    
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   2) Get survey designs for the survey regions and years queried
  ##      Format survey_def_ids, year)set, haul_type vectors into a format used
  ##      in SQL queries.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Error Query: check that argument survey_set is one the correct options.
  if (is.null(x = survey_set) | 
      !all(survey_set %in% c("GOA", "AI", "EBS", "NBS", "BSS"))) {
    stop(paste0("arg survey_set must contain one or more of these options",
                " (case-sensitive): 
                'GOA', 'AI', 'EBS', 'BSS', or 'NBS'."))
  }
  
  ## re-concatenate survey_set for use in a SQL query
  survey_def_ids <- c("AI" = 52, "GOA" = 47, "EBS" = 98, 
                      "BSS" = 78, "NBS" = 143)[survey_set]
  survey_def_ids_vec <- gapindex::stitch_entries(stitch_what = survey_def_ids)
  
  ## re-concatenate year_set for use in a SQL query
  year_vec <- gapindex::stitch_entries(stitch_what = year_set)
  
  ## Re-concatenate haul_type arg for use in SQL queries.
  haultype_vec <- gapindex::stitch_entries(haul_type)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   3) Query survey table. This table tells you which survey design 
  ##      (captured in the DESIGN_YEAR field) for a given survey and year. This 
  ##      is useful for figuring out which version of the Bering Sea survey 
  ##      strata to use or when querying GOA years before and after the 2025 
  ##      restratified survey design. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  cat("Pulling survey table...\n")
  
  survey_sql <- 
    paste("CREATE TABLE GAPINDEX_TEMPORARY_SURVEY_QUERY AS
SELECT SURVEY_DEFINITION_ID, 
CASE 
  WHEN SURVEY_DEFINITION_ID = 143 THEN 'NBS'
  WHEN SURVEY_DEFINITION_ID = 98 THEN 'EBS'
  WHEN SURVEY_DEFINITION_ID = 47 THEN 'GOA'
  WHEN SURVEY_DEFINITION_ID = 52 THEN 'AI'
  WHEN SURVEY_DEFINITION_ID = 78 THEN 'BSS'
  ELSE NULL
END AS SURVEY, YEAR, DESIGN_YEAR 
FROM GAP_PRODUCTS.SURVEY_DESIGN 
WHERE SURVEY_DEFINITION_ID IN", survey_def_ids_vec,
          "
AND YEAR IN", year_vec)
  
  RODBC::sqlQuery(channel = channel, query = survey_sql)
  
  survey_df <- data.table::data.table(
    RODBC::sqlQuery(channel = channel,
                    query = "SELECT * FROM GAPINDEX_TEMPORARY_SURVEY_QUERY"))
  attributes(survey_df)$sql_query <- (survey_sql)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   4) Query Survey Design table. This table reports which survey designs 
  ##   are included given the surveys and years inputted by the user
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  cat("Pulling survey design table...\n")
  survey_design_sql <- 
    "CREATE TABLE GAPINDEX_TEMPORARY_SURVEY_DESIGN_QUERY AS
SELECT DISTINCT SURVEY_DEFINITION_ID, SURVEY, DESIGN_YEAR 
FROM GAPINDEX_TEMPORARY_SURVEY_QUERY"
  
  RODBC::sqlQuery(channel = channel, query = survey_design_sql)
  
  survey_design <- data.table::data.table(
    RODBC::sqlQuery(channel = channel,
                    query = "SELECT * 
                    FROM GAPINDEX_TEMPORARY_SURVEY_DESIGN_QUERY"))
  attributes(survey_design)$sql_query <- survey_design_sql
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   5) Query Cruise data: This table reports the cruise information for the 
  ##   given the surveys and years inputted by the user.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling cruise table...\n")
  
  cruise_sql <- paste(
    "CREATE TABLE GAPINDEX_TEMPORARY_CRUISE_QUERY AS

SELECT DISTINCT A.CRUISEJOIN, B.CRUISE, FLOOR(B.CRUISE/100) YEAR, 
D.SURVEY_DEFINITION_ID, B.VESSEL_ID, E.NAME VESSEL_NAME, 
CASE 
 WHEN D.SURVEY_DEFINITION_ID = 143 THEN 'NBS'
 WHEN D.SURVEY_DEFINITION_ID = 98 THEN 'EBS'
 WHEN D.SURVEY_DEFINITION_ID = 47 THEN 'GOA'
 WHEN D.SURVEY_DEFINITION_ID = 52 THEN 'AI'
 WHEN D.SURVEY_DEFINITION_ID = 78 THEN 'BSS'
 ELSE NULL
END AS SURVEY, F.DESIGN_YEAR

FROM RACEBASE.HAUL A, RACE_DATA.CRUISES B, RACE_DATA.SURVEYS C,
RACE_DATA.SURVEY_DEFINITIONS D, RACE_DATA.VESSELS E, 
GAP_PRODUCTS.SURVEY_DESIGN F

WHERE A.VESSEL = B.VESSEL_ID 
AND B.VESSEL_ID = E.VESSEL_ID
AND A.CRUISE = B.CRUISE 
AND C.SURVEY_ID = B.SURVEY_ID
AND C.SURVEY_DEFINITION_ID = D.SURVEY_DEFINITION_ID
AND D.SURVEY_DEFINITION_ID IN", survey_def_ids_vec,
    "
AND F.SURVEY_DEFINITION_ID = D.SURVEY_DEFINITION_ID
AND F.YEAR = FLOOR(B.CRUISE/100)
AND A.ABUNDANCE_HAUL IN", gapindex::stitch_entries(abundance_haul),
    "
AND FLOOR(B.CRUISE/100) IN", year_vec)
  
  RODBC::sqlQuery(channel = channel, query = cruise_sql)
  
  cruise_data <- data.table::data.table(
    RODBC::sqlQuery(channel = channel, 
                    query = "SELECT * FROM GAPINDEX_TEMPORARY_CRUISE_QUERY")
  )
  attributes(cruise_data)$sql_query <- cruise_sql
  
  ## Error Check: stop if there are no cruise data for the 
  ## queried year and region.
  if (nrow(x = cruise_data) == 0) {
    stop("No data exist for survey area '", 
         gapindex::stitch_entries(survey_set), 
         "' for the choosen set of years ", year_vec, ".")
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   6) Query Stratum data: This table reports the various strata and 
  ##   information given the surveys and years inputted by the user.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling stratum data...\n")
  
  stratum_sql <- paste(
    "CREATE TABLE GAPINDEX_TEMPORARY_STRATUM_QUERY AS
SELECT AREA.SURVEY_DEFINITION_ID, 
CASE 
 WHEN AREA.SURVEY_DEFINITION_ID = 143 THEN 'NBS'
 WHEN AREA.SURVEY_DEFINITION_ID = 98 THEN 'EBS'
 WHEN AREA.SURVEY_DEFINITION_ID = 47 THEN 'GOA'
 WHEN AREA.SURVEY_DEFINITION_ID = 52 THEN 'AI'
 WHEN AREA.SURVEY_DEFINITION_ID = 78 THEN 'BSS'
 ELSE NULL
END AS SURVEY, AREA.DESIGN_YEAR, AREA.AREA_ID AS STRATUM, AREA.AREA_KM2, 
AREA.DESCRIPTION, AREA.AREA_NAME 
FROM GAP_PRODUCTS.AREA AREA, 
GAPINDEX_TEMPORARY_SURVEY_DESIGN_QUERY SURVEY_DESIGN
WHERE AREA_TYPE = 'STRATUM'
AND SURVEY_DESIGN.SURVEY_DEFINITION_ID = AREA.SURVEY_DEFINITION_ID
AND SURVEY_DESIGN.DESIGN_YEAR = AREA.DESIGN_YEAR
AND AREA.SURVEY_DEFINITION_ID IN", survey_def_ids_vec,
    "ORDER BY SURVEY_DEFINITION_ID, DESIGN_YEAR, STRATUM"
  )
  
  RODBC::sqlQuery(channel = channel, query = stratum_sql)
  
  stratum_data <- data.table::data.table(
    RODBC::sqlQuery(channel = channel,
                    query = "SELECT * FROM GAPINDEX_TEMPORARY_STRATUM_QUERY")
  )
  attributes(stratum_data)$sql_query <- stratum_sql
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   7) Query Stratum data: This table reports the various strata and 
  ##   information given the surveys and years inputted by the user.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling subarea data...\n")
  
  subarea_sql <- paste(
    "CREATE TABLE GAPINDEX_TEMPORARY_SUBAREA_QUERY AS
SELECT AREA.SURVEY_DEFINITION_ID, 
CASE 
 WHEN AREA.SURVEY_DEFINITION_ID = 143 THEN 'NBS'
 WHEN AREA.SURVEY_DEFINITION_ID = 98 THEN 'EBS'
 WHEN AREA.SURVEY_DEFINITION_ID = 47 THEN 'GOA'
 WHEN AREA.SURVEY_DEFINITION_ID = 52 THEN 'AI'
 WHEN AREA.SURVEY_DEFINITION_ID = 78 THEN 'BSS'
 ELSE NULL
END AS SURVEY, AREA.DESIGN_YEAR, AREA.AREA_ID, AREA.AREA_KM2, 
AREA.DESCRIPTION, AREA.AREA_NAME 
FROM GAP_PRODUCTS.AREA AREA, 
GAPINDEX_TEMPORARY_SURVEY_DESIGN_QUERY SURVEY_DESIGN
WHERE AREA_TYPE != 'STRATUM'
AND SURVEY_DESIGN.SURVEY_DEFINITION_ID = AREA.SURVEY_DEFINITION_ID
AND SURVEY_DESIGN.DESIGN_YEAR = AREA.DESIGN_YEAR
AND AREA.SURVEY_DEFINITION_ID IN", survey_def_ids_vec,
    "ORDER BY SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID"
  )
  
  RODBC::sqlQuery(channel = channel, query = subarea_sql)
  
  subarea_data <- data.table::data.table(
    RODBC::sqlQuery(channel = channel,
                    query = "SELECT * FROM GAPINDEX_TEMPORARY_SUBAREA_QUERY")
  )
  attributes(subarea_data)$sql_query <- subarea_sql
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   8) Query Stratum Groups data: This table reports the strata that are
  ##   contained in each subarea/region AREA_ID for the given surveys and years
  ##   inputted by the users. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stratum_groups_sql <- paste(
    "CREATE TABLE GAPINDEX_TEMPORARY_STRATUM_GROUPS_QUERY AS

SELECT STRATUM_GROUPS.SURVEY_DEFINITION_ID,
CASE 
 WHEN STRATUM_GROUPS.SURVEY_DEFINITION_ID = 143 THEN 'NBS'
 WHEN STRATUM_GROUPS.SURVEY_DEFINITION_ID = 98 THEN 'EBS'
 WHEN STRATUM_GROUPS.SURVEY_DEFINITION_ID = 47 THEN 'GOA'
 WHEN STRATUM_GROUPS.SURVEY_DEFINITION_ID = 52 THEN 'AI'
 WHEN STRATUM_GROUPS.SURVEY_DEFINITION_ID = 78 THEN 'BSS'
 ELSE NULL
END AS SURVEY, 
STRATUM_GROUPS.DESIGN_YEAR, STRATUM_GROUPS.AREA_ID, STRATUM_GROUPS.STRATUM

FROM GAP_PRODUCTS.STRATUM_GROUPS STRATUM_GROUPS,
GAPINDEX_TEMPORARY_SURVEY_DESIGN_QUERY SURVEY_DESIGN

WHERE SURVEY_DESIGN.SURVEY_DEFINITION_ID = STRATUM_GROUPS.SURVEY_DEFINITION_ID
AND SURVEY_DESIGN.DESIGN_YEAR = STRATUM_GROUPS.DESIGN_YEAR
AND STRATUM_GROUPS.SURVEY_DEFINITION_ID IN", survey_def_ids_vec, 
    "

ORDER BY DESIGN_YEAR, SURVEY, AREA_ID, STRATUM")
  
  RODBC::sqlQuery(channel = channel, query = stratum_groups_sql)
  
  stratum_groups <- data.table::data.table(
    RODBC::sqlQuery(channel = channel, 
                    query = "SELECT * 
                     FROM GAPINDEX_TEMPORARY_STRATUM_GROUPS_QUERY")
  )
  attributes(stratum_groups)$sql_query <- stratum_groups_sql
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 9) Query Haul data. This table reports hauls based on the CRUISEJOIN 
  ##   values in the cruise_data and further filtering for well-performing tows
  ##   (PERFORMANCE >= 0) and haul type (e.g., 3 is the standard bottom sample 
  ##   (pre-programmed station)). For production runs, ABUNDANCE_TYPE == "Y" 
  ##   is also filtered. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling haul data...\n")
  
  haul_sql <- paste(
    "CREATE TABLE GAPINDEX_TEMPORARY_HAUL_QUERY AS

SELECT * 

FROM RACEBASE.HAUL 

JOIN (SELECT CRUISEJOIN FROM GAPINDEX_TEMPORARY_CRUISE_QUERY) USING (CRUISEJOIN)
", 
    "
WHERE HAUL_TYPE IN", haultype_vec,
    "
AND PERFORMANCE >= 0 
AND ABUNDANCE_HAUL IN", gapindex::stitch_entries(abundance_haul), 
    ifelse(test = remove_na_strata == T,
           yes = "\nAND STRATUM IS NOT NULL",
           no = "")
  )
  
  RODBC::sqlQuery(channel = channel, query = haul_sql)
  
  haul_data <- data.table::data.table(
    RODBC::sqlQuery(channel = channel, 
                    query = "SELECT * FROM GAPINDEX_TEMPORARY_HAUL_QUERY")
  )
  attributes(haul_data)$sql_query <- haul_sql
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   10) Query available species based on the HAULJOINs queried so far
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling available species...\n")
  ## Query available species given the surveys queried
  avail_spp_sql <-
    "CREATE TABLE GAPINDEX_TEMPORARY_AVAIL_SPP_QUERY AS
SELECT DISTINCT SPECIES_CODE
FROM RACEBASE.CATCH
JOIN GAPINDEX_TEMPORARY_HAUL_QUERY USING (HAULJOIN)
ORDER BY SPECIES_CODE"
  
  RODBC::sqlQuery(channel = channel, query = avail_spp_sql)
  
  avail_spp <- data.table::data.table(
    RODBC::sqlQuery(channel = channel, 
                    query = "SELECT * FROM GAPINDEX_TEMPORARY_AVAIL_SPP_QUERY")
  )
  attributes(avail_spp)$sql_query <- avail_spp_sql
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   11) Format user-inputted species and taxon group information
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  ## Check that spp_codes can either be:
  ## 1) dataframe with columns "GROUP_CODE" and "SPECIES_CODE" for instances 
  ##    where species complexes are defined (e.g., rock soles).
  if (is.data.frame(x = spp_codes)) {
    if (!all(c("SPECIES_CODE", "GROUP_CODE") %in% names(x = spp_codes)))
      stop("If argument `spp_codes` is a dataframe, it must contain column names
         `GROUP_CODE` and `SPECIES_CODE`. See ?gapindex::get_data for
         more details and examples.")
    
    RODBC::sqlSave(channel = channel, dat = spp_codes, 
                   tablename = "GAPINDEX_TEMPORARY_USER_INPUT_SPP_QUERY", 
                   rownames = F, append = F#, 
                   # varTypes = c("SPECIES_CODE" = "NUMBER(5,0)", 
                                # "GROUP_CODE" = "NUMBER(5,0)")
                   )
    
    species_sql <- "CREATE TABLE GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY AS
SELECT * 
FROM GAPINDEX_TEMPORARY_USER_INPUT_SPP_QUERY
JOIN GAPINDEX_TEMPORARY_AVAIL_SPP_QUERY USING (SPECIES_CODE)
JOIN RACEBASE.SPECIES USING (SPECIES_CODE)
JOIN RACEBASE.SPECIES_CLASSIFICATION USING (SPECIES_CODE)"
    
    RODBC::sqlQuery(channel = channel, query = species_sql)
    species_info <- data.table::data.table(
      RODBC::sqlQuery(channel = channel, 
                      query = "SELECT * 
                    FROM GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY")
    )
    
    attributes(species_info)$sql_query <- species_sql
  }
  
  ## 2) a vector with SPECIES_CODES for single taxa. 
  if (is.numeric(x = spp_codes)) {

    spp_codes <- data.table(SPECIES_CODE = spp_codes, GROUP_CODE = spp_codes)
    RODBC::sqlSave(channel = channel, dat = spp_codes, 
                   tablename = "GAPINDEX_TEMPORARY_USER_INPUT_SPP_QUERY", 
                   rownames = F, append = F, 
                   varTypes = c("SPECIES_CODE" = "NUMBER(5,0)", 
                                "GROUP_CODE" = "NUMBER(5,0)"))
    
    species_sql <- "CREATE TABLE GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY AS
SELECT * 
FROM GAPINDEX_TEMPORARY_USER_INPUT_SPP_QUERY
JOIN GAPINDEX_TEMPORARY_AVAIL_SPP_QUERY USING (SPECIES_CODE)
JOIN RACEBASE.SPECIES USING (SPECIES_CODE)
JOIN RACEBASE.SPECIES_CLASSIFICATION USING (SPECIES_CODE)"
    
    RODBC::sqlQuery(channel = channel, query = species_sql)
    species_info <- data.table::data.table(
      RODBC::sqlQuery(channel = channel, 
                      query = "SELECT * 
                    FROM GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY")
    )
    
    attributes(species_info)$sql_query <- species_sql
  }
  
  ## 3) NULL: usually for production purposes
  ## Pull from the GAP_PRODUCTS.TAXON_GROUPS 
  if (is.null(x = spp_codes)) {
    RODBC::sqlQuery(channel = channel,
                    query = "CREATE TABLE GAPINDEX_TEMPORARY_USER_INPUT_SPP_QUERY AS
                    SELECT SPECIES_CODE, GROUP_CODE
                    FROM GAP_PRODUCTS.TAXON_GROUPS
                    WHERE GROUP_CODE IS NOT NULL")
    
    RODBC::sqlQuery(channel = channel,
                    query = "CREATE TABLE GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY AS
                    SELECT *
                    FROM GAP_PRODUCTS.TAXON_GROUPS
                    WHERE GROUP_CODE IS NOT NULL")
    
    species_info <- data.table::data.table(
      RODBC::sqlQuery(channel = channel, 
                      query = "SELECT * 
                    FROM GAP_PRODUCTS.TAXON_GROUPS
                      WHERE GROUP_CODE IS NOT NULL"))
    
    attributes(species_info)$sql_query <- 
    "SELECT * 
FROM GAP_PRODUCTS.TAXON_GROUPS
WHERE GROUP_CODE IS NOT NULL"
  
    }
  
  ## Error check if there are no data for the user-inputted species_code values
  if (!is.data.frame(x = species_info) | nrow(x = species_info) == 0)
    stop("There are no catch records for any of the species codes in argument
         spp_codes for survey area '", gapindex::stitch_entries(survey_set),
         "' in the chosen years ", year_vec)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   12) Identify any user-inputted SPECIES_CODE values not present in the data
  ##   based on the queried HAULJOIN values queried thus far, e.g., an English
  ##   sole in Bering Slope data.  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cat("Identifying any unavailable taxa...\n")
    unavailable_spp_sql <- "CREATE TABLE GAPINDEX_TEMPORARY_UNAVAIL_SPP_QUERY AS
SELECT *
FROM GAPINDEX_TEMPORARY_USER_INPUT_SPP_QUERY
WHERE SPECIES_CODE NOT IN (
      SELECT SPECIES_CODE
      FROM GAPINDEX_TEMPORARY_AVAIL_SPP_QUERY
)"
    RODBC::sqlQuery(channel = channel, query = unavailable_spp_sql)
    unavail_species_info <- data.table::data.table(
      RODBC::sqlQuery(channel = channel,
                      query = "SELECT *
                      FROM GAPINDEX_TEMPORARY_UNAVAIL_SPP_QUERY")
    )
    attributes(unavail_species_info)$sql_query <- unavailable_spp_sql
    
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   13) Query catch data: This table reports the catch in weight and numbers
  ##   for each available user-inputted species code from the HAULJOIN values
  ##   queried in the haul data. Then the catch and numbers are summed, grouped
  ##   by GROUP_CODE and HAULJOIN. Note for a taxon complex, if there is a 
  ##   null count for any taxon in the complex within a given HAULJOIN, it is 
  ##   assumed that the aggregated count is also null. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling catch data...\n")
  
  catch_sql <- paste(
    "CREATE TABLE GAPINDEX_TEMPORARY_CATCH_QUERY AS

WITH CATCH_QUERY AS (
  SELECT HAULJOIN, SPECIES_CODE, GROUP_CODE, WEIGHT, NUMBER_FISH
  FROM RACEBASE.CATCH

  JOIN (SELECT SPECIES_CODE, GROUP_CODE
        FROM GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY) 
        USING (SPECIES_CODE)
      
  JOIN (SELECT HAULJOIN 
        FROM GAPINDEX_TEMPORARY_HAUL_QUERY) 
        USING (HAULJOIN)
) 

-- Aggregate numbers and weights by GROUP_CODE and HAULJOIN
SELECT HAULJOIN, GROUP_CODE AS SPECIES_CODE, 
SUM(WEIGHT) AS WEIGHT, SUM(NUMBER_FISH) AS NUMBER_FISH

FROM CATCH_QUERY

GROUP BY (HAULJOIN, GROUP_CODE)
")
  
  RODBC::sqlQuery(channel = channel, query = catch_sql)
  
  catch_data <-  data.table::data.table(
    RODBC::sqlQuery(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_CATCH_QUERY")
  )
  attributes(catch_data)$sql_query <- catch_sql
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   14) Query Size data (if pull_lengths == TRUE): This table reports the 
  ##   recorded length (in mm) frequencies for each available user-inputted 
  ##   species code from the HAULJOIN values queried in the haul data. Then the
  ##   size frequencies are summed, grouped by GROUP_CODE and HAULJOIN.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  size_data <- NULL
  
  if (pull_lengths) {
    
    cat("Pulling size data...\n")
    
    size_sql <- paste(
      "CREATE TABLE GAPINDEX_TEMPORARY_SIZE_QUERY AS

WITH SIZE_QUERY AS (
  SELECT CRUISEJOIN, HAULJOIN, SPECIES_CODE, GROUP_CODE, SEX, LENGTH, FREQUENCY
  
  FROM RACEBASE.LENGTH

  JOIN (SELECT SPECIES_CODE, GROUP_CODE
        FROM GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY) 
        USING (SPECIES_CODE)
      
  JOIN (SELECT HAULJOIN 
        FROM GAPINDEX_TEMPORARY_HAUL_QUERY) 
        USING (HAULJOIN)
) 

SELECT CRUISEJOIN, HAULJOIN, GROUP_CODE AS SPECIES_CODE, SEX, LENGTH, 
SUM(FREQUENCY) AS FREQUENCY

FROM SIZE_QUERY

GROUP BY (CRUISEJOIN, HAULJOIN, GROUP_CODE, SEX, LENGTH)

ORDER BY CRUISEJOIN, HAULJOIN, SPECIES_CODE, LENGTH, SEX")
    
    RODBC::sqlQuery(channel = channel, query = size_sql) 
    
    size_data <- data.table::data.table(
      RODBC::sqlQuery(channel = channel,
                      query = "SELECT * FROM 
                               GAPINDEX_TEMPORARY_SIZE_QUERY")
    )
    
    attributes(size_data)$sql_query <- size_sql
    
    ## Error Query: send out a warning if there are no lengths in the dataset
    if (nrow(x = size_data) == 0) {
      warning("There are no length data for any of the species_codes for 
              survey area(s) '", gapindex::stitch_entries(survey_set),
              "' in the chosen years ", year_vec)
      size_data <- NULL
    }
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   15) Query Specimen data. This table reports specimen (otolith) data
  ##   for each available user-inputted species code from the HAULJOIN values 
  ##   queried in the haul data.  Then, records are filter for only read 
  ##   otoliths (i.e., AGE IS NOT NULL)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  speclist <- NULL
  
  if (pull_lengths) {
    cat("Pulling specimen data...\n")
    
    specimen_sql <- "CREATE TABLE GAPINDEX_TEMPORARY_SPECIMEN_QUERY AS
    
SELECT CRUISEJOIN, HAULJOIN, SPECIES_CODE, LENGTH, SEX, WEIGHT, AGE

FROM RACEBASE.SPECIMEN
  
JOIN (SELECT SPECIES_CODE 
      FROM GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY) 
      USING (SPECIES_CODE)
      
JOIN (SELECT HAULJOIN 
      FROM GAPINDEX_TEMPORARY_HAUL_QUERY) 
      USING (HAULJOIN)

WHERE AGE IS NOT NULL

ORDER BY HAULJOIN, SPECIES_CODE, AGE, LENGTH"
    
    RODBC::sqlQuery(channel = channel, query = specimen_sql) 
    
    speclist <- data.table::data.table(
      RODBC::sqlQuery(channel = channel,
                      query = "SELECT * FROM 
                               GAPINDEX_TEMPORARY_SPECIMEN_QUERY"))
    
    attributes(speclist)$sql_query <- specimen_sql
    
    ## Warning Query: send out a warning if there are no ages in the dataset
    if (nrow(x = speclist) == 0) {
      warning("There are no age data for any the species_codes for 
            survey area(s) '", gapindex::stitch_entries(survey_set), 
              "' in the chosen years ", year_vec)
      speclist <- NULL
    }
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   16) Clear temporary tables
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Clearing temporary tables...")
  for (itable in c("AVAIL_SPP", "CATCH", "CRUISE", "HAUL", "INPUT_SPP", 
                   "SIZE", "SPECIMEN", "STRATUM_GROUPS", "STRATUM", 
                   "SURVEY_DESIGN", "SURVEY", "SUBAREA", "USER_TAXONOMIC_INFO", 
                   "UNAVAIL_SPP", "USER_INPUT_SPP")) {
    
    if (nrow(x = RODBC::sqlQuery(channel = channel,
                                 query = paste0("SELECT table_name
                                   FROM user_tables
                                   WHERE TABLE_NAME = 'GAPINDEX_TEMPORARY_",  
                                                itable, "_QUERY'"))) != 0)
      RODBC::sqlQuery(channel = channel, 
                      query = paste0("DROP TABLE ", "GAPINDEX_TEMPORARY_", 
                                     itable, "_QUERY"))
  }
  
  cat("Finished.\n")
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Collate survey, cruise, haul, catch, size, specimen, subarea, strata, 
  ##   and species data into a list and return
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(do.call(what = list,
                 args = list(survey = survey_df,
                             survey_design = survey_design,
                             cruise = cruise_data,
                             haul = haul_data,
                             catch = catch_data,
                             size = size_data,
                             specimen = speclist,
                             species = species_info,
                             avail_species = avail_spp,
                             unavail_species = unavail_species_info,
                             strata = stratum_data,
                             stratum_groups = stratum_groups,
                             subarea = subarea_data)))
}

