#' Pull AFSC GAP BTS survey data
#' 
#' @description Pulls survey, cruise, haul, catch, size, specimen (age), 
#'              stratum, subarea, stratum grouping information for the 
#'              region, years, and species of interest from AFSC Oracle database. 
#' 
#' @param year_set  numeric or integer vector of years
#' @param survey_set character string. One of c("GOA", "AI", "EBS", "NBS", "BSS"). 
#' @param spp_codes either a vector of species codes e.g., c(21720, 21740, 10110)
#'                  or a data frame with columns SPECIES_CODE and GROUP_CODE 
#'                  to denote taxonomic complexes, e.g., 
#'                  data.frame("SPECIES_CODE" = c(21720, 21220, 21230, 21232), 
#'                  "GROUP_CODE" = c(21720, "Grenadiers", "Grenadiers", "Grenadiers"))
#' @param haul_type integer. Defaults to haul type "3" for "standard bottom 
#'                  sample (preprogrammed station)" used for production purposes. 
#' @param abundance_haul character string. "Y" are standardized hauls used in 
#'                       production and "N" are non-standard hauls due to 
#'                       bad performance, different gear, etc.
#' @param taxonomic_source  character string. Table used to source taxonomic
#'                         information. One of three options: "RACEBASE.SPECIES" 
#'                         (default), "GAP_TAXONOMY.TAXONOMIC_CLASSIFICATION",
#'                         or "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION" 
#'                         (in the process of deprecation). 
#'                         
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

get_data <- function(
    year_set = c(1996, 1999),
    survey_set = c("GOA", "AI", "EBS", "NBS", "BSS")[1],
    spp_codes = c(21720, 30060, 10110),
    haul_type = 3,
    abundance_haul = c("Y", "N")[1],
    pull_lengths = FALSE,
    taxonomic_source = c("RACEBASE.SPECIES_CLASSIFICATION",
                         "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION",
                         "GAP_TAXONOMY.TAXONOMIC_CLASSIFICATION")[1],
    remove_na_strata = FALSE,
    na_rm_strata = lifecycle::deprecated(),
    channel = NULL,
    sql_channel = lifecycle::deprecated()
)
{
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   1) Repair any deprecated arguments
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Handle argument deprecation on channel if passed
  if (lifecycle::is_present(sql_channel)) {
    lifecycle::deprecate_stop("3.0.0", 
                              "get_data(sql_channel)", 
                              "get_data(channel)")
    channel <- sql_channel
  }
  if (lifecycle::is_present(na_rm_strata)) {
    lifecycle::deprecate_stop("3.0.0", 
                              "get_data(na_rm_strata)", 
                              "get_data(remove_na_strata)")
    remove_na_strata <- na_rm_strata
  }
  
  # Warn if the deprecated taxonomic_source option is selected
  if (identical(taxonomic_source, "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION")) {
    lifecycle::deprecate_warn(
      when = "3.0.0", # adjust to your target version
      what = I("The argument value `taxonomic_source = 'GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION'`"),
      details = "Please use`taxonomic_source = 'GAP_TAXONOMY.TAXONOMIC_CLASSIFICATION'` instead. 'GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION' will not be an option by the end of 2027."
    )
  }
  
  if (is.data.frame(x = spp_codes) &
      ("GROUP" %in% names(x = spp_codes)) & 
      !("GROUP_CODE" %in% names(x = spp_codes))){
    warning(paste("In argument `spp_code`, the field name 'GROUP' is",
                  "deprecated because it is masked by the SQL GROUP command.",
                  "Please use the field name 'GROUP_CODE' when preparing the",
                  "`spp_code` argument."))
    
    data.table::setnames(x = spp_codes, 
                         old = "GROUP",
                         new = "GROUP_CODE")
  }
  
  ## Set up channel if channel = NULL
  if (is.null(x = channel)) channel <- gapindex::get_connected()
  ## Clear schema of temporary tables created in this function if present 
  remove_temp_tables(channel = channel)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   2)  Format survey_def_ids, year_set, haul_type vectors into a format used
  ##      in SQL queries.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Error Query: check that argument survey_set is one the correct options.
  if (is.null(x = survey_set) | 
      !all(survey_set %in% c("GOA", "AI", "EBS", "NBS", "BSS"))) {
    stop(paste0("argument `survey_set` must contain one of these options",
                " (case-sensitive): 'GOA', 'AI', 'EBS', 'BSS', or 'NBS'."))
  }
  
  ## Issue a warning when choosing multiple surveys
  if (length(x = survey_set) > 1)
    warning(
      paste("The gapindex package has only been tested when querying",
            "only one survey region. Use caution when querying", 
            "multiple survey regions until further testing has been done.",
            "If you come across an issue, be sure to post it on",
            "github.com/afsc-gap-products/gapindex/issues")
    )
  
  ## re-concatenate survey_set for use in a SQL query
  survey_def_ids <- c("AI" = 52, "GOA" = 47, "EBS" = 98, 
                      "BSS" = 78, "NBS" = 143)[survey_set]
  survey_def_ids_vec <- gapindex::stitch_entries(stitch_what = survey_def_ids)
  
  ## re-concatenate year_set for use in a SQL query
  year_vec <- gapindex::stitch_entries(stitch_what = year_set)
  
  ## Re-concatenate haul_type arg for use in SQL queries.
  haultype_vec <- gapindex::stitch_entries(haul_type)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   3) Query survey table. This table tells you which survey design 
  ##      (captured in the DESIGN_YEAR field) for a given survey and year. 
  ##      This is useful for figuring out which version of the Bering Sea 
  ##      survey strata to use or when querying GOA years before and after 
  ##      the 2025 restratified survey design. 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling survey table...\n")
  
  gapindex::sql_query(
    channel = channel, 
    query = paste("
      CREATE TABLE GAPINDEX_TEMPORARY_SURVEY_QUERY AS
      
      SELECT 
        SURVEY_DEFINITION_ID, 
        CASE 
          WHEN SURVEY_DEFINITION_ID = 143 THEN 'NBS'
          WHEN SURVEY_DEFINITION_ID = 98 THEN 'EBS'
          WHEN SURVEY_DEFINITION_ID = 47 THEN 'GOA'
          WHEN SURVEY_DEFINITION_ID = 52 THEN 'AI'
          WHEN SURVEY_DEFINITION_ID = 78 THEN 'BSS'
          ELSE NULL
        END AS SURVEY, 
        YEAR, 
        DESIGN_YEAR 
      FROM 
        GAP_PRODUCTS.SURVEY_DESIGN 
      WHERE 
        SURVEY_DEFINITION_ID IN", survey_def_ids_vec, "
        AND YEAR IN", year_vec
    )
  )
  
  ## Pull in queried survey table
  survey_df <- data.table::data.table(
    gapindex::sql_query(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_SURVEY_QUERY"
    ),
    key = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR", "DESIGN_YEAR")
  )
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   4) Query Survey Design table. This table reports which survey designs
  ##   are included given the surveys and years inputted by the user
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling survey design table...\n")
  
  gapindex::sql_query(
    channel = channel,
    query = "
    CREATE TABLE GAPINDEX_TEMPORARY_SURVEY_DESIGN_QUERY AS
    
    SELECT 
      DISTINCT SURVEY_DEFINITION_ID, 
      SURVEY, 
      DESIGN_YEAR
    FROM 
      GAPINDEX_TEMPORARY_SURVEY_QUERY"
  )
  
  survey_design <- data.table::data.table(
    gapindex::sql_query(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_SURVEY_DESIGN_QUERY"
    ),
    key = c("SURVEY_DEFINITION_ID", "SURVEY", "DESIGN_YEAR") 
  )
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   5) Query Cruise data: This table reports the cruise information for the
  ##   given the surveys and years inputted by the user.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling cruise table...\n")
  gapindex::sql_query(
    channel = channel,
    query =
      paste("
    CREATE TABLE GAPINDEX_TEMPORARY_CRUISE_QUERY AS
    
    SELECT 
      DISTINCT A.CRUISEJOIN, 
      B.CRUISE, 
      FLOOR(B.CRUISE/100) YEAR,
      D.SURVEY_DEFINITION_ID, 
      B.VESSEL_ID, 
      E.NAME 
      VESSEL_NAME,
      CASE
        WHEN D.SURVEY_DEFINITION_ID = 143 THEN 'NBS'
        WHEN D.SURVEY_DEFINITION_ID = 98 THEN 'EBS'
        WHEN D.SURVEY_DEFINITION_ID = 47 THEN 'GOA'
        WHEN D.SURVEY_DEFINITION_ID = 52 THEN 'AI'
        WHEN D.SURVEY_DEFINITION_ID = 78 THEN 'BSS'
        ELSE NULL
      END AS SURVEY, 
      F.DESIGN_YEAR

    FROM 
      RACEBASE.HAUL A, 
      RACE_DATA.CRUISES B, 
      RACE_DATA.SURVEYS C,
      RACE_DATA.SURVEY_DEFINITIONS D, 
      RACE_DATA.VESSELS E,
      GAP_PRODUCTS.SURVEY_DESIGN F

    WHERE 
      A.VESSEL = B.VESSEL_ID
      AND B.VESSEL_ID = E.VESSEL_ID
      AND A.CRUISE = B.CRUISE
      AND C.SURVEY_ID = B.SURVEY_ID
      AND C.SURVEY_DEFINITION_ID = D.SURVEY_DEFINITION_ID
      AND D.SURVEY_DEFINITION_ID IN", survey_def_ids_vec, "
      AND F.SURVEY_DEFINITION_ID = D.SURVEY_DEFINITION_ID
      AND F.YEAR = FLOOR(B.CRUISE/100)
      AND A.ABUNDANCE_HAUL IN", gapindex::stitch_entries(abundance_haul), "
      AND FLOOR(B.CRUISE/100) IN", year_vec)
  )
  
  ## Pull queried cruise table
  cruise_data <- data.table::data.table(
    gapindex::sql_query(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_CRUISE_QUERY"
    ),
    key = "CRUISEJOIN"
  )
  
  ## Return error if there are no cruise data for the queried year and region.
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
  gapindex::sql_query(
    channel = channel,
    query = paste("
    CREATE TABLE GAPINDEX_TEMPORARY_STRATUM_QUERY AS
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
                  "\nORDER BY SURVEY_DEFINITION_ID, DESIGN_YEAR, STRATUM")
  )
  
  ## Pull queried stratum table
  stratum_data <- data.table::data.table(
    gapindex::sql_query(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_STRATUM_QUERY"
    ),
    key = c("SURVEY_DEFINITION_ID", "SURVEY", "DESIGN_YEAR", "STRATUM")
  )
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   7) Query Stratum data: This table reports the various strata and
  ##   information given the surveys and years inputted by the user.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling subarea data...\n")
  gapindex::sql_query(
    channel = channel,
    query = paste("
    CREATE TABLE GAPINDEX_TEMPORARY_SUBAREA_QUERY AS
    SELECT AREA.SURVEY_DEFINITION_ID,
    CASE
     WHEN AREA.SURVEY_DEFINITION_ID = 143 THEN 'NBS'
     WHEN AREA.SURVEY_DEFINITION_ID = 98 THEN 'EBS'
     WHEN AREA.SURVEY_DEFINITION_ID = 47 THEN 'GOA'
     WHEN AREA.SURVEY_DEFINITION_ID = 52 THEN 'AI'
     WHEN AREA.SURVEY_DEFINITION_ID = 78 THEN 'BSS'
     ELSE NULL
    END AS SURVEY, AREA.DESIGN_YEAR, AREA.AREA_TYPE, AREA.AREA_ID,
    AREA.AREA_KM2, AREA.DESCRIPTION, AREA.AREA_NAME

    FROM GAP_PRODUCTS.AREA AREA,
    GAPINDEX_TEMPORARY_SURVEY_DESIGN_QUERY SURVEY_DESIGN

    WHERE AREA_TYPE != 'STRATUM'
    AND SURVEY_DESIGN.SURVEY_DEFINITION_ID = AREA.SURVEY_DEFINITION_ID
    AND SURVEY_DESIGN.DESIGN_YEAR = AREA.DESIGN_YEAR
    AND AREA.SURVEY_DEFINITION_ID IN", survey_def_ids_vec,
                  "\nORDER BY SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID")
  )
  
  ## Pull queried subarea table
  subarea_data <- data.table::data.table(
    gapindex::sql_query(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_SUBAREA_QUERY"
    ),
    key = c("SURVEY_DEFINITION_ID", "SURVEY", 
            "DESIGN_YEAR", "AREA_ID", "AREA_TYPE")
  )
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   8) Query Stratum Groups data: This table reports the strata that are
  ##   contained in each subarea/region AREA_ID for the given surveys and
  ##   years inputted by the users.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gapindex::sql_query(
    channel = channel,
    query = paste("
    CREATE TABLE GAPINDEX_TEMPORARY_STRATUM_GROUPS_QUERY AS
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
                  "\nORDER BY DESIGN_YEAR, SURVEY, AREA_ID, STRATUM"))
  
  ## Pull queried stratum groups table
  stratum_groups <- data.table::data.table(
    gapindex::sql_query(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_STRATUM_GROUPS_QUERY"
    ),
    key = c("SURVEY_DEFINITION_ID", "SURVEY", 
            "DESIGN_YEAR", "AREA_ID", "STRATUM")
  )
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 9) Query Haul data. This table reports hauls based on the CRUISEJOIN
  ##   values in the cruise_data and further filtering for well-performing tows
  ##   (PERFORMANCE >= 0) and haul type (e.g., 3 is the standard bottom sample
  ##   (pre-programmed station)). For production runs, ABUNDANCE_TYPE == "Y"
  ##   is also filtered.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling haul data...\n")
  gapindex::sql_query(
    channel = channel,
    query = paste("
    CREATE TABLE GAPINDEX_TEMPORARY_HAUL_QUERY AS
    SELECT *
    FROM RACEBASE.HAUL

    JOIN (SELECT CRUISEJOIN FROM GAPINDEX_TEMPORARY_CRUISE_QUERY)
    USING (CRUISEJOIN)

    WHERE HAUL_TYPE IN", haultype_vec, "
    AND PERFORMANCE >= 0
    AND ABUNDANCE_HAUL IN",
                  gapindex::stitch_entries(abundance_haul),
                  ifelse(test = remove_na_strata == T,
                         yes = "\nAND STRATUM IS NOT NULL",
                         no = "")
    )
  )
  
  ## Pull queried haul table
  haul_data <- data.table::data.table(
    gapindex::sql_query(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_HAUL_QUERY"
    ),
    key = "HAULJOIN"
  )
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   10) Query available species based on the HAULJOINs queried so far
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling available species...\n")
  
  gapindex::sql_query(channel = channel,
                      query = "
    CREATE TABLE GAPINDEX_TEMPORARY_AVAIL_SPP_QUERY AS
    SELECT DISTINCT SPECIES_CODE
    FROM RACEBASE.CATCH
    JOIN GAPINDEX_TEMPORARY_HAUL_QUERY USING (HAULJOIN)
    ORDER BY SPECIES_CODE")
  
  ## Pull queried available species given the surveys queried
  avail_spp <- data.table::data.table(
    gapindex::sql_query(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_AVAIL_SPP_QUERY"
    ),
    key = "SPECIES_CODE"
  )
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   11) Format user-inputted species and taxon group information
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Preparing Species List...\n")
  
  ## Check that spp_codes can either be:
  ## 1) dataframe with columns "GROUP_CODE" and "SPECIES_CODE" for instances
  ##    where species complexes are defined (e.g., rock soles).
  ## 2) a vector with SPECIES_CODES for single taxa.
  if (is.data.frame(x = spp_codes)) {
    if (!all(c("SPECIES_CODE", "GROUP_CODE") %in% names(x = spp_codes)))
      stop("If argument `spp_codes` is a dataframe, it must contain column names
         `GROUP_CODE` and `SPECIES_CODE`. See ?gapindex::get_data for
         more details and examples.")
  }
  
  if (is.numeric(x = spp_codes)) {
    spp_codes <- data.table::data.table(SPECIES_CODE = spp_codes,
                                        GROUP_CODE = spp_codes)
  }
  
  ## Upload the species list 
  if (inherits(channel, "RODBC"))
    RODBC::sqlSave(channel = channel,
                   dat = spp_codes,
                   tablename = "GAPINDEX_TEMPORARY_USER_INPUT_SPP_QUERY",
                   rownames = F,
                   append = F
    )
  
  if (inherits(channel, "Oracle"))
    DBI::dbWriteTable(conn = channel,
                      name = "GAPINDEX_TEMPORARY_USER_INPUT_SPP_QUERY",
                      value = spp_codes,
                      field.types = c("SPECIES_CODE" = "NUMBER(5,0)",
                                      "GROUP_CODE" = "NUMBER(5,0)"))
  
  ## Join the species list with the taxonomic information from the 
  ## user-inputted taxonomic source
  gapindex::sql_query(
    channel = channel,
    query =
      paste(
        "
      CREATE TABLE GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY AS
      SELECT *
      FROM GAPINDEX_TEMPORARY_USER_INPUT_SPP_QUERY
      LEFT JOIN GAPINDEX_TEMPORARY_AVAIL_SPP_QUERY USING (SPECIES_CODE)
      LEFT JOIN", taxonomic_source, "USING (SPECIES_CODE)\n",
        switch(
          taxonomic_source,
          "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION" = "WHERE (SURVEY_SPECIES = 1 OR SURVEY_SPECIES IS NULL)",
          "GAP_TAXONOMY.TAXONOMIC_CLASSIFICATION" = "WHERE (ACTIVE = 1 OR ACTIVE IS NULL)",
          "RACEBASE.SPECIES_CLASSIFICATION" = "")
      )
  )
  
  ## Pull the resulting query 
  species_info <- data.table::data.table(
    gapindex::sql_query(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY"
    ),
    key = "SPECIES_CODE"
  )
  
  ## Error check if there are no data for the user-inputted species_code values
  if (!is.data.frame(x = species_info) | nrow(x = species_info) == 0)
    stop("There are no catch records for any of the species codes in argument
         spp_codes for survey area '", gapindex::stitch_entries(survey_set),
         "' in the chosen years ", year_vec)
  
  ## In the event where the user is using GAP_TAXONOMY.TAXONOMIC_CLASSIFICATION
  ## as the source of the taxonomic information and inputs a SPECIES_CODE value
  ## that is not in GAP_TAXONOMY.TAXONOMIC_CLASSIFICATION, use the taxonomic
  ## information attributed to the updated species code
  if (taxonomic_source == "GAP_TAXONOMY.TAXONOMIC_CLASSIFICATION"
      & any(is.na(x = species_info$ACTIVE))) {
    recovered_tax_info <- data.table::data.table(
      gapindex::sql_query(
        channel = channel,
        query = "
        select 
          user_input.species_code defunct_code, 
          tax_class.*
        from 
          GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY user_input 
        join
          GAP_TAXONOMY.TAXONOMIC_CHANGES changes on 
            changes.old_species_code = user_input.species_code
        join 
          gap_taxonomy.taxonomic_classification tax_class on 
            tax_class.species_code = changes.new_species_code
        where 
          user_input.active is null    
          and tax_class.active = 1;"
      ) 
    )
    
    updated_tax_info <- 
      merge(x = species_info[is.na(ACTIVE), c("SPECIES_CODE", "GROUP_CODE")],
            y = recovered_tax_info,
            by.x = "SPECIES_CODE", by.y = "DEFUNCT_CODE")
    
    species_info <- rbind(updated_tax_info[, -"SPECIES_CODE.y"],
                          species_info[!is.na(ACTIVE)])
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   12) Identify any user-inputted SPECIES_CODE values not present in the
  ##   data based on the queried HAULJOIN values queried thus far, e.g., an
  ##   English sole in Bering Slope data.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Identifying any unavailable taxa...\n")
  gapindex::sql_query(channel = channel,
                      query = "
    CREATE TABLE GAPINDEX_TEMPORARY_UNAVAIL_SPP_QUERY AS
    SELECT * FROM GAPINDEX_TEMPORARY_USER_INPUT_SPP_QUERY
    WHERE SPECIES_CODE NOT IN (
        SELECT SPECIES_CODE FROM GAPINDEX_TEMPORARY_AVAIL_SPP_QUERY
      )")
  
  ## Pull unavailable species codes
  unavail_species_info <- data.table::data.table(
    gapindex::sql_query(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_UNAVAIL_SPP_QUERY"
    ),
    key = "SPECIES_CODE"
  )
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   13) Query catch data: This table reports the catch in weight and numbers
  ##   for each available user-inputted species code from the HAULJOIN values
  ##   queried in the haul data. Then the catch and numbers are summed, grouped
  ##   by GROUP_CODE and HAULJOIN. Note for a taxon complex, if there is a
  ##   null count for any taxon in the complex within a given HAULJOIN, it is
  ##   assumed that the aggregated count is also null.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling catch data...\n")
  
  gapindex::sql_query(channel = channel,
                      query = "
    CREATE TABLE GAPINDEX_TEMPORARY_CATCH_QUERY AS

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
    GROUP BY (HAULJOIN, GROUP_CODE)")
  
  ## Pull queried catch table
  catch_data <-  data.table::data.table(
    gapindex::sql_query(
      channel = channel,
      query = "SELECT * FROM GAPINDEX_TEMPORARY_CATCH_QUERY"
    ),
    key = c("HAULJOIN", "SPECIES_CODE")
  )
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   14) Query Size data (if pull_lengths == TRUE): This table reports the
  ##   recorded length (in mm) frequencies for each available user-inputted
  ##   species code from the HAULJOIN values queried in the haul data. Then the
  ##   size frequencies are summed, grouped by GROUP_CODE and HAULJOIN.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  size_data <- NULL
  if (pull_lengths) {
    cat("Pulling size data...\n")
    gapindex::sql_query(channel = channel,
                        query = "
        CREATE TABLE GAPINDEX_TEMPORARY_SIZE_QUERY AS

         WITH SIZE_QUERY AS (
            SELECT CRUISEJOIN, HAULJOIN, SPECIES_CODE, GROUP_CODE,
            SEX, LENGTH, FREQUENCY

            FROM RACEBASE.LENGTH

            JOIN (SELECT SPECIES_CODE, GROUP_CODE
                  FROM GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY)
            USING (SPECIES_CODE)

            JOIN (SELECT HAULJOIN
                  FROM GAPINDEX_TEMPORARY_HAUL_QUERY)
            USING (HAULJOIN)

            WHERE FREQUENCY IS NOT NULL
            )

          SELECT CRUISEJOIN, HAULJOIN, GROUP_CODE AS SPECIES_CODE,
          SEX, LENGTH, SUM(FREQUENCY) AS FREQUENCY
          FROM SIZE_QUERY
          GROUP BY (CRUISEJOIN, HAULJOIN, GROUP_CODE, SEX, LENGTH)
          ORDER BY CRUISEJOIN, HAULJOIN, SPECIES_CODE, LENGTH, SEX")
    
    ## Pull queried size table
    size_data <- data.table::data.table(
      gapindex::sql_query(
        channel = channel,
        query = "SELECT * FROM GAPINDEX_TEMPORARY_SIZE_QUERY"
      ),
      key = c("HAULJOIN", "SPECIES_CODE", "SEX", "LENGTH")
    )
    
    ## Warning when there are no lengths in the dataset
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
    gapindex::sql_query(channel = channel,
                        query = "
      CREATE TABLE GAPINDEX_TEMPORARY_SPECIMEN_QUERY AS

      SELECT CRUISEJOIN, HAULJOIN, GROUP_CODE AS SPECIES_CODE,
      LENGTH, SEX, WEIGHT, AGE

      FROM RACEBASE.SPECIMEN

      JOIN (SELECT SPECIES_CODE
            FROM GAPINDEX_TEMPORARY_USER_TAXONOMIC_INFO_QUERY)
      USING (SPECIES_CODE)

      JOIN (SELECT HAULJOIN
            FROM GAPINDEX_TEMPORARY_HAUL_QUERY)
      USING (HAULJOIN)

      JOIN (SELECT *
            FROM GAPINDEX_TEMPORARY_USER_INPUT_SPP_QUERY)
      USING (SPECIES_CODE)

      WHERE AGE IS NOT NULL
      ORDER BY HAULJOIN, SPECIES_CODE, AGE, LENGTH")
    
    ## Pull queried specimen table
    speclist <- data.table::data.table(
      gapindex::sql_query(
        channel = channel,
        query = "SELECT * FROM GAPINDEX_TEMPORARY_SPECIMEN_QUERY"
      ),
      key = c("HAULJOIN", "SPECIES_CODE", "SEX", "AGE", "LENGTH")
    )
    
    ## Warning when there are no ages in the dataset
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
  remove_temp_tables(channel = channel)
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
                             subarea = subarea_data))
  )
}
