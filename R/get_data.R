#' Pull data from RACEBASE
#' 
#' @description Pulls cruise, haul, catch, and stratum information for the 
#'              region, years, and species of interest. 
#' 
#' @param year_set numeric or integer vector of years
#' @param survey_set character string. One of c("GOA", "AI", "EBS", "NBS", "BSS"). 
#' @param spp_codes two-column dataframe of species codes (column name 
#'                  SPECIES_CODE) and GROUP name (column name GROUP). 
#'                  For single-species, the GROUP and species codes can be the
#'                  same. Examples: 
#'                  1) data.frame("SPECIES_CODE" = c(21720, 21220, 
#'                  21230, 21232), 
#'                  "GROUP" = c(21720, "Grenadiers", "Grenadiers", 
#'                  "Grenadiers")) # for taxon groups
#'                  2) c(21720, 21740, 10110) # for single taxa
#'                  3) NULL # for all species present in within the combination
#'                     of survey_set and year_set
#' @param haul_type integer. Defaults to haul type "3" for Standard bottom 
#'                  sample (preprogrammed station) used for biomass estimation
#' @param abundance_haul character string. "Y" are abundance hauls and "N" are 
#'                       other hauls.
#' @param na_rm_strata boolean. Remove hauls with NA stratum information. 
#' Defaults to FALSE. 
#' @param sql_channel connection created via gapindex::get_connected()
#' @param pull_lengths boolean T/F. Should lengths and specimen data be pulled? 
#'                     Defaults to FALSE for speed.
#' 
#' @return a named list containing survey, cruise, haul, catch, specimen (if
#'         pull_lengths == TRUE), length (if pull_lengths == TRUE), species, 
#'         stratum information for the survey, years, and species of interest. 
#' 
#' @export
#'  

get_data <- function(year_set = c(1996, 1999),
                     survey_set = c("GOA"),
                     spp_codes = c(21720, 30060, 10110),
                     haul_type = 3,
                     abundance_haul = c("Y", "N")[1],
                     na_rm_strata = FALSE,
                     sql_channel = NULL,
                     pull_lengths = FALSE) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   1) Set up channel if sql_channel = NULL
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(x = sql_channel)) sql_channel <- gapindex::get_connected()
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   2) Get survey designs for the survey regions and years queried
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Error Query: check that argument survey_set is one the correct options.
  if (is.null(x = survey_set) | 
      !all(survey_set %in% c("GOA", "AI", "EBS", "NBS", "BSS"))) {
    stop(paste0("arg survey_set must contain one or more of these options",
                " (case-sensitive): 
                'GOA', 'AI', 'EBS', 'BSS', or 'NBS'."))
  }
  year_vec <- gapindex::stitch_entries(stitch_what = year_set)
  survey_def_ids <- c("AI" = 52, "GOA" = 47, "EBS" = 98, 
                      "BSS" = 78, "NBS" = 143)[survey_set]
  survey_def_ids_vec <- gapindex::stitch_entries(stitch_what = survey_def_ids)
  
  ## Query Survey Design table
  cat("Pulling survey design table...\n")
  survey_design <- 
    RODBC::sqlQuery(channel = sql_channel,
                    query = paste0(
                      "SELECT SURVEY_DEFINITION_ID, ",
                      "CASE 
                       WHEN SURVEY_DEFINITION_ID = 143 THEN 'NBS'
                       WHEN SURVEY_DEFINITION_ID = 98 THEN 'EBS'
                       WHEN SURVEY_DEFINITION_ID = 47 THEN 'GOA'
                       WHEN SURVEY_DEFINITION_ID = 52 THEN 'AI'
                       WHEN SURVEY_DEFINITION_ID = 78 THEN 'BSS'
                       ELSE NULL
                       END AS SURVEY, ",
                      "YEAR, DESIGN_YEAR FROM GAP_PRODUCTS.SURVEY_DESIGN ",
                      " WHERE SURVEY_DEFINITION_ID IN ", 
                      survey_def_ids_vec, 
                      " AND YEAR IN ", year_vec) )
  
  ##  `survey_df` summarizes the unique surveys queried. Within a 
  ##  survey year, there are often multiple records in `cruise_data` 
  ##  corresponding to different vessels.
  survey_df <- unique(x = subset(x = survey_design, select = -YEAR))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   2) Get cruise data
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling cruise table...\n")
  
  ## Query cruise data and filter survey and years of interest. Attach year,
  ## survey_definition_id, and survey columns. 
  cruise_data <- 
    RODBC::sqlQuery(
      channel = sql_channel, 
      query = paste0(
        "select distinct a.cruisejoin, b.cruise, floor(b.cruise/100) year, ",
        "d.survey_definition_id, b.vessel_id, e.name vessel_name, ",
        "CASE 
         WHEN d.SURVEY_DEFINITION_ID = 143 THEN 'NBS'
         WHEN d.SURVEY_DEFINITION_ID = 98 THEN 'EBS'
         WHEN d.SURVEY_DEFINITION_ID = 47 THEN 'GOA'
         WHEN d.SURVEY_DEFINITION_ID = 52 THEN 'AI'
         WHEN d.SURVEY_DEFINITION_ID = 78 THEN 'BSS'
         ELSE NULL
         END AS SURVEY ",
        "from racebase.haul a, race_data.cruises b, race_data.surveys c, ",
        "race_data.survey_definitions d, race_data.vessels e ",
        "where a.vessel = b.vessel_id and b.vessel_id = e.vessel_id ",
        "and a.cruise = b.cruise and c.survey_id = b.survey_id ",
        "and c.survey_definition_id = d.survey_definition_id ",
        "and d.survey_definition_id in ", survey_def_ids_vec,
        "and a.abundance_haul in ", gapindex::stitch_entries(abundance_haul),
        "and year in ", year_vec))
  
  ## Merge "DESIGN_YEAR" column from `survey_design` to `cruise_data` using 
  ## columns "YEAR" AND "SURVEY_DEFINITION_ID" as a composite key. 
  cruise_data <- merge(x = cruise_data,
                       y = survey_design,
                       by = c("YEAR", "SURVEY_DEFINITION_ID"))
  
  ## Error Query: stop if there is no cruise data for the year and region.
  if (nrow(x = cruise_data) == 0) {
    stop("No data exist for survey area '", survey_set, 
         "' for the choosen set of years ", year_vec, ".")
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   4) Get stratum, subarea, and stratum grouping data
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling stratum data...\n")
  
  ## Query all area records for the queried surveys
  area_info <- 
    RODBC::sqlQuery(channel = sql_channel,
                    query = paste0(
                      "SELECT SURVEY_DEFINITION_ID, ",
                      "CASE 
                       WHEN SURVEY_DEFINITION_ID = 143 THEN 'NBS'
                       WHEN SURVEY_DEFINITION_ID = 98 THEN 'EBS'
                       WHEN SURVEY_DEFINITION_ID = 47 THEN 'GOA'
                       WHEN SURVEY_DEFINITION_ID = 52 THEN 'AI'
                       WHEN SURVEY_DEFINITION_ID = 78 THEN 'BSS'
                       ELSE NULL
                       END AS SURVEY, ",
                      "DESIGN_YEAR, AREA_ID, TYPE, AREA_KM2, DESCRIPTION, ",
                      "AREA_NAME FROM GAP_PRODUCTS.AREA",
                      " WHERE SURVEY_DEFINITION_ID IN ", survey_def_ids_vec))
  
  ## Subset stratum info out of `area_info`
  stratum_data <- subset(x = area_info,
                         subset = TYPE == "STRATUM" & 
                           DESIGN_YEAR %in% survey_df$DESIGN_YEAR)
  
  ## Subset subarea info out of `area_info`
  subarea_data <- subset(x = area_info,
                         subset = TYPE != "STRATUM" & 
                           DESIGN_YEAR %in% survey_df$DESIGN_YEAR)
  
  ## Change "AREA_ID" column name to "STRATUM". 
  ## Reorder `stratum_data` columns and sort records. 
  stratum_data <- 
    stratum_data[order(stratum_data$SURVEY, stratum_data$AREA_ID),
                 c("SURVEY_DEFINITION_ID", "DESIGN_YEAR", "AREA_ID",
                   "AREA_NAME", "DESCRIPTION", "AREA_KM2")]
  names(stratum_data)[names(stratum_data) == "AREA_ID"] <- "STRATUM"
  
  subarea_data <- 
    subarea_data[order(subarea_data$SURVEY, subarea_data$AREA_ID),
                 c("SURVEY_DEFINITION_ID", "DESIGN_YEAR", "TYPE",
                   "AREA_ID", "AREA_NAME", "DESCRIPTION")]
  
  
  ## Query stratum groupings table for the queried surveys. This table tells
  ## you which strata make up a particular subarea/region. 
  stratum_groups <-
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0(
                      "SELECT SURVEY_DEFINITION_ID, ", 
                      "CASE 
                       WHEN SURVEY_DEFINITION_ID = 143 THEN 'NBS'
                       WHEN SURVEY_DEFINITION_ID = 98 THEN 'EBS'
                       WHEN SURVEY_DEFINITION_ID = 47 THEN 'GOA'
                       WHEN SURVEY_DEFINITION_ID = 52 THEN 'AI'
                       WHEN SURVEY_DEFINITION_ID = 78 THEN 'BSS'
                       ELSE NULL
                       END AS SURVEY, ", 
                      "AREA_ID, DESIGN_YEAR, STRATUM ",
                      "FROM GAP_PRODUCTS.STRATUM_GROUPS ", 
                      "WHERE SURVEY_DEFINITION_ID IN ", survey_def_ids_vec, 
                      "ORDER BY SURVEY, AREA_ID, STRATUM"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 5) Query Haul data based on the CRUISEJOIN values in the cruise_data
  ##   Filter for good tows (PERFORMANCE >= 0) and haul type (e.g., 3 is the
  ##   standard bottom sample (pre-programmed station)). ABUNDANCE_TYPE == "Y"
  ##   should be a redundant criterion. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Now pulling haul data...\n")
  
  cruisejoin_vec <- gapindex::stitch_entries(cruise_data$CRUISEJOIN)
  haultype_vec <- gapindex::stitch_entries(haul_type)
  
  haul_data <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0(
                      "SELECT * FROM RACEBASE.HAUL WHERE CRUISEJOIN IN ", 
                      cruisejoin_vec, " AND HAUL_TYPE IN ", haultype_vec,
                      " AND PERFORMANCE >= 0 AND ABUNDANCE_HAUL IN ",
                      gapindex::stitch_entries(abundance_haul)))
  
  if (na_rm_strata)
    haul_data <- subset(x = haul_data, subset = !is.na(x = STRATUM))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   6) Error checks on the `spp_codes` argument
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Query available species given the surveys queried
  avail_spp <-
    RODBC::sqlQuery(channel = sql_channel,
                    query = paste0("SELECT DISTINCT SPECIES_CODE ",
                                   "FROM RACEBASE.CATCH where CRUISEJOIN in ", 
                                   cruisejoin_vec))$SPECIES_CODE
  
  ## Check that spp_codes can either be:
  ## 1) dataframe with columns "GROUP" and "SPECIES_CODE" for instances where
  ##    species complexes are defined (e.g., rock soles).
  if (is.data.frame(x = spp_codes)) {
    if (!all(c("SPECIES_CODE", "GROUP") %in% names(x = spp_codes)))
      stop("If argument `spp_codes` is a dataframe, it must contain column names
         `GROUP` and `SPECIES_CODE`. See ?gapindex::get_data for
         more details and examples.")
    
    query_spp <- avail_spp[avail_spp %in% unique(x = spp_codes$SPECIES_CODE)]
    spp_codes_vec <- gapindex::stitch_entries(stitch_what = query_spp)
  }
  
  ## 2) a vector with SPECIES_CODES for single taxa. 
  if (is.numeric(x = spp_codes)) {
    spp_codes_vec <- gapindex::stitch_entries(stitch_what = spp_codes)
    spp_codes <- data.frame(SPECIES_CODE = spp_codes, GROUP = spp_codes)
  }
  
  ## 3) NULL: usually for production purposes, in which all taxa with a 
  ## SPECIES_CODE value are queried. 
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   7) Query species information
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling species data...\n")
  species_info <- RODBC::sqlQuery(
    channel = sql_channel, 
    query = paste0("SELECT * FROM GAP_PRODUCTS.TAXONOMICS_WORMS where ",
                   "SPECIES_CODE IN ",
                   ifelse(test = is.null(x = spp_codes$SPECIES_CODE),
                          yes = paste0("(SELECT DISTINCT SPECIES_CODE ",
                                       "FROM RACEBASE.CATCH where ",
                                       "CRUISEJOIN in ", cruisejoin_vec, ")"),
                          no = spp_codes_vec)))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   8) Query catch data based `cruisejoin_vec` and `spp_codes_vec`
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling catch data...\n")
  
  catch_data <- RODBC::sqlQuery(
    channel = sql_channel,
    query = paste0( "SELECT * FROM RACEBASE.CATCH ",
                    "where CRUISEJOIN in ", cruisejoin_vec,
                    " and SPECIES_CODE in ",
                    ifelse(test = is.null(x = spp_codes),
                           yes = paste0("(SELECT DISTINCT SPECIES_CODE ",
                                        "FROM RACEBASE.CATCH where ",
                                        "CRUISEJOIN in ", cruisejoin_vec, ")"),
                           no = spp_codes_vec)))
  
  catch_data <- subset(x = catch_data,
                       subset = HAULJOIN %in% haul_data$HAULJOIN)
  
  ## Error Query: check whether there are species data
  if (!is.data.frame(x = catch_data) | nrow(x = catch_data) == 0)
    stop("There are no catch records for any of the species codes in argument
         spp_codes for survey area '", survey_set, "' in the chosen years ",
         year_vec)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   9) Query Size information if pull_lengths == TRUE
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  size_data <- NULL
  
  if (pull_lengths) {
    
    cat("Pulling size data...\n")
    
    size_data <- RODBC::sqlQuery(
      channel = sql_channel, 
      query = paste0("SELECT * FROM RACEBASE.LENGTH ",
                     "where CRUISEJOIN in ", cruisejoin_vec,
                     " and SPECIES_CODE in ",
                     ifelse(test = is.null(x = spp_codes),
                            yes = paste0("(SELECT DISTINCT SPECIES_CODE ",
                                         "FROM RACEBASE.LENGTH where ",
                                         "CRUISEJOIN in ", cruisejoin_vec, ")"),
                            no = spp_codes_vec))) 
    
    size_data <- subset(x = size_data,
                        subset = HAULJOIN %in% haul_data$HAULJOIN)
    
    ## Error Query: send out a warning if there are no lengths in the dataset
    if (nrow(x = size_data) == 0) {
      warning("There are no length data for any of the species_codes for 
              survey area '", survey_set, "' in the chosen years ", year_vec)
      size_data <- NULL
      
    }
    
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   10) Query Specimen information
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  speclist <- NULL
  
  if (pull_lengths) {
    speclist <- RODBC::sqlQuery(
      channel = sql_channel, 
      query = paste0("select s.SPECIES_CODE, s.cruisejoin, s.hauljoin, ",
                     "s.region, s.vessel, s.cruise, s.haul, s.specimenid, ",
                     "s.length, s.sex, s.weight, s.age  from ",
                     "racebase.specimen s where ",
                     "CRUISEJOIN in ", cruisejoin_vec,
                     " AND SPECIES_CODE in ", 
                     ifelse(test = is.null(x = spp_codes),
                            yes = paste0("(SELECT DISTINCT SPECIES_CODE ",
                                         "FROM RACEBASE.SPECIMEN where ",
                                         "CRUISEJOIN in ", cruisejoin_vec, ")"),
                            no = spp_codes_vec),
                     " AND AGE IS NOT NULL"))
    
    speclist <- subset(x = speclist,
                       subset = speclist$HAULJOIN %in% haul_data$HAULJOIN)
    
    ## Error Query: send out a warning if there are no ages in the dataset
    if (nrow(x = speclist) == 0) {
      warning("There are no age data for any the species_codes for 
            survey area '", survey_set, "' in the chosen years ", year_vec)
      speclist <- NULL
    }
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   11) Aggregate species complex information (if any)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## If `spp_codes` = NULL, fill in with species data
  if (is.null(x = spp_codes)) {    
    spp_codes <- data.frame(SPECIES_CODE = avail_spp, GROUP = avail_spp)
    spp_codes_vec <- 
      paste0("(", paste(sort(x = avail_spp), collapse = ", "), ")")
  }
  
  ## Merge "GROUP" column from `spp_codes` into `catch_data` for scenraios
  ## where you are defining species complexes.
  catch_data <- merge(x = catch_data, 
                      y = spp_codes, 
                      by = "SPECIES_CODE")
  
  ## Sum "WEIGHT" and "NUMBER_FISH" aggregated by "GROUP" and "HAULJOIN". 
  catch_data <- stats::aggregate(cbind(WEIGHT, NUMBER_FISH) ~ HAULJOIN + GROUP,
                                 data = catch_data,
                                 na.rm = TRUE, na.action = NULL,
                                 FUN = sum)
  
  ## Rename "GROUP" column 
  names(catch_data)[names(catch_data) == "GROUP"] <- "SPECIES_CODE"
  
  ##   Merge "GROUP" column from `spp_codes` to `species_info` using 
  ##   "SPECIES_CODE" as a key
  species_info <- merge(x = species_info, 
                        y = spp_codes,
                        by = "SPECIES_CODE")
  
  cat("Finished.\n")
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Collate survey, cruise, haul, catch, size, specimen, subarea, strata, 
  ##   and species data into a list and return
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(list(survey = survey_df,
              survey_design = survey_design,
              cruise = cruise_data,
              haul = haul_data,
              catch = catch_data,
              size = size_data,
              specimen = speclist,
              species = species_info,
              strata = stratum_data,
              stratum_groups = stratum_groups,
              subarea = subarea_data))
}
