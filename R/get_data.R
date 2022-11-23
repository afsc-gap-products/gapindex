#' Pull data from RACEBASE
#' 
#' @description Pulls cruise, haul, catch, and stratum information for the 
#'              region, years, and species of interest. 
#' 
#' @param year_set numeric or integer vector of years
#' @param survey_set character string. One of c("GOA", "AI", "EBS_SHELF", 
#'                   "EBS_SLOPE", "NBS_SHELF")
#' @param spp_codes two-column dataframe of species codes (column name 
#'                  species_codes) and group name (column name group). 
#'                  For single-species, the group and species codes can be the
#'                  same (see example). 
#' @param haul_type integer. Defaults to haul type "3" for Standard bottom 
#'                  sample (preprogrammed station) used for biomass estimation
#' @param abundance_haul character string. "Y" are abundance hauls (what does
#'                        this mean?) and "N" are other hauls.
#' @param sql_channel connection created via AFSC.GAP.DBE::get_connected()
#' 
#' @return a named list containing cruise, haul, catch, and stratum information 
#'         for the region, years, and species of interest. 
#' 
#' @examples
#' '\dontrun{
#' AFSC.GAP.DBE::get_data(
#' year_set = 1996:2021, 
#' survey_set = "GOA", 
#' ## This configuration of spp_codes provides single-species estimates for 
#' ## Pacific cod and complex-level estimates for SAFE grenadiers 
#' spp_codes = data.frame("species_code" = c(21720, 21220, 21230, 21232), 
#'                        "group" = c(21720, "Grenadiers", "Grenadiers", "Grenadiers"))
#' )
#' }
#' @export
#' 

get_data <- function(year_set = c(1996, 1999),
                     survey_set = c("GOA", "BS", "AI")[1],
                     spp_codes = data.frame("species_code" = 21720, 
                                            "group" = 21720),
                     haul_type = 3,
                     abundance_haul = c("Y"),
                     sql_channel = NULL) {
  
  #####################################################################
  ## Check that spp_codes is a dataframe with group, species_codes
  #####################################################################
  if (class(spp_codes) != "data.frame") {
    stop("argument `spp_codes` must be a dataframe with column names 
         `group` and `species_code`. See ?AFSC.GAP.DBE::get_data for 
         more details and examples.") 
  } else {
    if (!all(c("species_code", "group") %in% names(spp_codes)))
      stop("argument `spp_codes` must be a dataframe with column names 
         `group` and `species_code`. See ?AFSC.GAP.DBE::get_data for 
         more details and examples.")
  }
  
  
  ## Set up channel if sql_channel = NULL
  if (is.null(sql_channel)) sql_channel <- get_connected()
  
  #####################################################################
  ## Pulling Cruise Data
  #####################################################################
  cat("Pulling cruise data...\n")
  
  ## Query cruise data and filter survey regions of interest
  cruise_data_safe <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM SAFE.SURVEY WHERE SURVEY =",
                                   " '", survey_set, "'"))
  cruise_data <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = "SELECT * FROM RACEBASE.CRUISE")
  cruise_data <- subset(x = cruise_data, 
                        subset = cruise_data$CRUISEJOIN %in% 
                          cruise_data_safe$CRUISEJOIN)
  
  #####################################################################
  ## Query stratum data
  #####################################################################
  cat("Pulling stratum data...\n")
  
  if (survey_set %in% c("GOA", "AI")) {
    ## GOA and AI strata currently live in the GOA.GOA_STRATA table
    ## so we pull that table and filter for survey_set
    stratum_data <- RODBC::sqlQuery(channel = sql_channel, 
                                    query = "SELECT * FROM GOA.GOA_STRATA")
    stratum_data <- subset(x = stratum_data, 
                           subset = stratum_data$SURVEY %in% survey_set,
                           select = c("SURVEY", "STRATUM", 
                                      "AREA", "DESCRIPTION"))
  }
  
  if (survey_set %in% c("EBS_SHELF", "EBS_SLOPE")) {
    ## BS strata live in the RACEBASE.STRATUM table. Stratum records are 
    ## are periodically updated when stratum areas change (e.g., dropping
    ## stations). So, we only pull the most recent record for a given stratum.
    stratum_data <- RODBC::sqlQuery(channel = sql_channel, 
                                    query = "SELECT * FROM RACEBASE.STRATUM")
    stratum_data <- subset(x = stratum_data, 
                           subset = stratum_data$REGION == "BS")
    
    stratum_data <- stratum_data[ifelse(test = survey_set == "EBS_SHELF",
                                        yes = -1,
                                        no = 1) *
                                   grep(x = stratum_data$DESCRIPTION,
                                        pattern = "slope",
                                        ignore.case = TRUE), ]
    
    ## Grab the most recent year for a given stratum
    stratum_data <- 
      do.call(what = rbind,
              args = lapply(X = split(x = stratum_data, 
                                      f = stratum_data$STRATUM),
                            FUN = function(x) x[which.max(x$YEAR), ] ))
    
    stratum_data <- subset(x = stratum_data, 
                           select = c("REGION", "STRATUM", 
                                      "AREA", "DESCRIPTION"))
    names(stratum_data)[1] <- "SURVEY"
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
  
  spp_codes_vec <- paste0("(", 
                          paste(as.character(spp_codes$species_code), 
                                collapse=", "), 
                          ")")
  
  catch_data <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM RACEBASE.CATCH ",
                                   "where CRUISEJOIN in ", cruisejoin_vec,
                                   ifelse(test = is.null(spp_codes),
                                          yes = "",
                                          no = " and SPECIES_CODE in "),
                                   spp_codes_vec))
  catch_data <- merge(x = catch_data, y = spp_codes, 
                      by.x = "SPECIES_CODE", by.y = "species_code")
  
  catch_data <- aggregate( cbind(WEIGHT, NUMBER_FISH) ~ 
                             CATCHJOIN + HAULJOIN + REGION + CRUISE + group,
                           data = catch_data,
                           na.rm=TRUE, na.action=NULL,
                           FUN = sum)
  
  #####################################################################
  ## Query Size information
  #####################################################################    
  cat("Pulling size data...\n")
  size_data <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM RACEBASE.LENGTH ",
                                   "where CRUISEJOIN in ", cruisejoin_vec,
                                   ifelse(test = is.null(spp_codes),
                                          yes = "",
                                          no = " and SPECIES_CODE in "),
                                   spp_codes_vec))
  
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
  
  ## Merge group information
  species_info <- merge(x = species_info, 
                        y = spp_codes,
                        by.x = "SPECIES_CODE", by.y = "species_code")
  
  cat("Finished.\n")
  
  #####################################################################
  ## Put cruise, haul, catch, stratu, and species data into a list and return
  #####################################################################
  return(list(cruise = cruise_data,
              haul = haul_data,
              catch = catch_data,
              size = size_data,
              strata = stratum_data,
              species = species_info))
  
}
