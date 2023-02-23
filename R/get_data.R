#' Pull data from RACEBASE
#' 
#' @description Pulls cruise, haul, catch, and stratum information for the 
#'              region, years, and species of interest. 
#' 
#' @param year_set numeric or integer vector of years
#' @param survey_set character string. One of c("GOA", "AI", "EBS_SHELF", 
#'                   "EBS_SLOPE", "NBS_SHELF")
#' @param spp_codes two-column dataframe of species codes (column name 
#'                  SPECIES_CODE) and GROUP name (column name GROUP). 
#'                  For single-species, the GROUP and species codes can be the
#'                  same. Example: data.frame("SPECIES_CODE" = c(21720, 21220, 21230, 21232), 
#'                  "GROUP" = c(21720, "Grenadiers", "Grenadiers", "Grenadiers"))
#' @param haul_type integer. Defaults to haul type "3" for Standard bottom 
#'                  sample (preprogrammed station) used for biomass estimation
#' @param abundance_haul character string. "Y" are abundance hauls (what does
#'                        this mean?) and "N" are other hauls.
#' @param sql_channel connection created via AFSC.GAP.DBE::get_connected()
#' @param pull_lengths boolean T/F. Should lengths be called? Defaults to FALSE
#'                     for speed.
#' 
#' @return a named list containing cruise, haul, catch, specimen,  
#'         optional size (length), and stratum information 
#'         for the region, years, and species of interest. 
#' 
#' @export
#' 

get_data <- function(year_set = c(1996, 1999),
                     survey_set = c("GOA", "AI", "EBS_SHELF", "NBS_SHELF")[1],
                     spp_codes = data.frame("SPECIES_CODE" = 21720, 
                                            "GROUP" = 21720),
                     haul_type = 3,
                     abundance_haul = c("Y", "N")[1],
                     sql_channel = NULL,
                     pull_lengths = F) {
  
  #####################################################################
  ## Check that spp_codes is a dataframe with GROUP, SPECIES_CODE
  #####################################################################
  if (class(spp_codes) != "data.frame") {
    stop("argument `spp_codes` must be a dataframe with column names 
         `GROUP` and `SPECIES_CODE`. See ?AFSC.GAP.DBE::get_data for 
         more details and examples.") 
  } else {
    if (!all(c("SPECIES_CODE", "GROUP") %in% names(spp_codes)))
      stop("argument `spp_codes` must be a dataframe with column names 
         `GROUP` and `SPECIES_CODE`. See ?AFSC.GAP.DBE::get_data for 
         more details and examples.")
  }
  
  
  ## Set up channel if sql_channel = NULL
  if (is.null(sql_channel)) sql_channel <- get_connected()
  
  #####################################################################
  ## Pulling Cruise Data
  #####################################################################
  cat("Pulling cruise data...\n")
  
  ## Query cruise data and filter survey regions of interest
  region_vec <- 
    paste0("(", paste0("'", survey_set, "'", collapse=", "), ")") 
  year_vec <- 
    paste0("(", paste0(year_set, collapse=", "), ")") 
  
  
  cruise_data <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM SAFE.SURVEY WHERE SURVEY IN ",
                                   region_vec, " AND YEAR IN ", year_vec))
  names(cruise_data)[names(cruise_data) == "SURVEY"] <- "REGION"
  
  #####################################################################
  ## Query stratum data
  #####################################################################
  cat("Pulling stratum data...\n")
  
  stratum_data <- data.frame()
  if (any(survey_set %in% c("GOA", "AI"))) {
    ## GOA and AI strata currently live in the GOA.GOA_STRATA table
    ## so we pull that table and filter for survey_set
    aigoa_stratum_data <- RODBC::sqlQuery(channel = sql_channel, 
                                          query = "SELECT * FROM GOA.GOA_STRATA")
    aigoa_stratum_data <- subset(x = aigoa_stratum_data, 
                                 subset = aigoa_stratum_data$SURVEY %in% survey_set,
                                 select = c("SURVEY", "STRATUM", 
                                            "AREA", "DESCRIPTION"))
    stratum_data <- rbind(stratum_data, aigoa_stratum_data)
  }
  
  if (any(survey_set %in% c("EBS_SHELF", "NBS_SHELF"))) {
    ## BS strata live in the RACEBASE.STRATUM table. Stratum records are 
    ## are periodically updated when stratum areas change (e.g., dropping
    ## stations). So, we only pull the most recent record for a given stratum.
    stratum_df <- rbind(data.frame(region = "EBS_SHELF",
                                   stratum = c(10,20,31,32,41,42,
                                               43,50,61,62,82,90)),
                        data.frame(region = "NBS_SHELF",
                                   stratum = c(70,71,81)))
    stratum_vec <- 
      paste0("(", 
             paste0(subset(x = stratum_df, 
                           subset = region %in% survey_set)$stratum, 
                    collapse=", "), 
             ")") 
    
    bs_stratum_data <- 
      RODBC::sqlQuery(channel = sql_channel, 
                      query = paste0("SELECT * FROM RACEBASE.STRATUM WHERE ",
                                     "STRATUM IN ", stratum_vec))
    
    ## Grab the most recent year for a given stratum
    bs_stratum_data <- 
      do.call(what = rbind,
              args = lapply(X = split(x = bs_stratum_data, 
                                      f = bs_stratum_data$STRATUM),
                            FUN = function(x) x[which.max(x$YEAR), ] ))
    
    bs_stratum_data <- subset(x = bs_stratum_data, 
                              select = c("REGION", "STRATUM", 
                                         "AREA", "DESCRIPTION"))
    names(bs_stratum_data)[1] <- "SURVEY"
    
    stratum_data <- rbind(stratum_data, bs_stratum_data)
  }
  
  #####################################################################
  ## Query Haul data based on the CRUISEJOIN values in the cruise_data
  ## Filter for good tows (PERFORMANCE >= 0) and haul type (e.g., 3 is the
  ##   Standard bottom sample (preprogrammed station))
  #####################################################################
  cat("Now pulling haul data...\n")
  
  cruisejoin_vec <- 
    paste0("(", paste(cruise_data$CRUISEJOIN, collapse=", "), ")") 
  haultype_vec <- paste0("PERFORMANCE >= 0 AND HAUL_TYPE IN (", 
                         paste(haul_type, collapse=", "), ")") 
  haul_data <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM RACEBASE.HAUL ",
                                   "WHERE CRUISEJOIN IN ", cruisejoin_vec, 
                                   " AND ", haultype_vec))
  
  ## Subset years of interest based on START_TIME and abundance_haul types
  haul_data <- 
    subset(x = haul_data, 
           subset = as.numeric(format(x = haul_data$START_TIME, 
                                      format = "%Y")) %in% year_set &
             haul_data$ABUNDANCE_HAUL %in% abundance_haul)
  
  #####################################################################
  ## Query catch data based on cruisejoin values in haul_data and species set
  #####################################################################
  cat("Pulling catch data...\n")
  
  cruisejoin_vec <-
    paste0("(", paste(unique(haul_data$CRUISEJOIN), collapse=", "), ")")
  
  avail_spp <-
    RODBC::sqlQuery(channel = sql_channel,
                    query = paste0("SELECT DISTINCT SPECIES_CODE ",
                                   "FROM RACEBASE.CATCH where CRUISEJOIN in ", 
                                   cruisejoin_vec))$SPECIES_CODE
  query_spp <- avail_spp[avail_spp %in% unique(spp_codes$SPECIES_CODE)]
  
  spp_codes_vec <- paste0("(", paste(sort(query_spp), collapse=", "), ")")
  
  catch_data <-
    RODBC::sqlQuery(channel = sql_channel,
                    query = paste0("SELECT * FROM RACEBASE.CATCH ",
                                   "where CRUISEJOIN in ", cruisejoin_vec,
                                   ifelse(test = is.null(spp_codes),
                                          yes = "",
                                          no = " and SPECIES_CODE in "),
                                   spp_codes_vec))
  catch_data <- merge(x = catch_data, y = spp_codes, 
                      by.x = "SPECIES_CODE", by.y = "SPECIES_CODE")
  
  catch_data <- stats::aggregate( 
    cbind(WEIGHT, NUMBER_FISH) ~ 
      HAULJOIN + REGION + CRUISE + GROUP,
    data = catch_data,
    na.rm = TRUE, na.action = NULL,
    FUN = sum)
  
  #####################################################################
  ## Query Size information
  #####################################################################   
  size_data = NULL
  if (pull_lengths) {
    cat("Pulling size data...\n")
    size_data <- 
      RODBC::sqlQuery(channel = sql_channel, 
                      query = paste0("SELECT * FROM RACEBASE.LENGTH ",
                                     "where CRUISEJOIN in ", cruisejoin_vec,
                                     ifelse(test = is.null(spp_codes),
                                            yes = "",
                                            no = " and SPECIES_CODE in "),
                                     spp_codes_vec)) 
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Query Specimen information
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  region_vec <- 
    paste0("(", paste0("'", 
                       ifelse(test = survey_set %in% c("EBS_SHELF", 
                                                       "NBS_SHELF"),
                              yes = "BS", 
                              no = survey_set), 
                       "'", collapse=", "), ")") 
  
  speclist <- RODBC::sqlQuery(
    channel = sql_channel, 
    query = paste0("select s.SPECIES_CODE, s.cruisejoin, s.hauljoin, ",
                   "s.region, s.vessel, s.cruise, s.haul, s.specimenid, ",
                   "s.length, s.sex, s.weight, s.age  from ",
                   "racebase.specimen s where ",
                   "REGION in ", region_vec, " and ",
                   "CRUISEJOIN in ", cruisejoin_vec, " and ",
                   "SPECIES_CODE in ", spp_codes_vec))

  
  #####################################################################
  ## Query species information
  #####################################################################
  cat("Pulling species data...\n")
  species_info <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM RACEBASE.SPECIES where ",
                                   ifelse(test = is.null(spp_codes),
                                          yes = "",
                                          no = " SPECIES_CODE in "),
                                   spp_codes_vec))
  
  ## Merge GROUP information
  species_info <- merge(x = species_info, 
                        y = spp_codes,
                        by.x = "SPECIES_CODE", by.y = "SPECIES_CODE")
  
  cat("Finished.\n")
  
  #####################################################################
  ## Put cruise, haul, catch, stratu, and species data into a list and return
  #####################################################################
  return(list(cruise = cruise_data,
              haul = haul_data,
              catch = catch_data,
              size = size_data,
              strata = stratum_data,
              species = species_info,
              specimen = speclist))
  
}
