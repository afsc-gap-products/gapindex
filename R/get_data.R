#' Pull data from RACEBASE
#' 
#' @description Pulls cruise, haul, catch, and stratum information for the 
#'              region, years, and species of interest. 
#' 
#' @param year_set numeric or integer vector of years
#' @param survey_set character string. One of c("GOA", "BS", "AI")
#' @param spp_codes character vector of species codes. See ... for full list
#' @param haul_type integer. Defaults to haul type "3" for Standard bottom 
#'                  sample (preprogrammed station) used for biomass estimation
#' @param abundance_haul character string. "Y" are abundance hauls (what does
#'                        this mean?) and "N" are other hauls.
#' @param sql_channel connection created via AFSC.GAP.DBE::get_connected()
#' 
#' @return a named list containing cruise, haul, catch, and stratum information 
#'         for the region, years, and species of interest. 
#' 
#' @export
#' 

get_data <- function(year_set = c(1996, 1999),
                     survey_set = c("GOA", "BS", "AI")[1],
                     spp_codes = c("Pacific cod" = 21720, 
                                   "walleye pollock" = 21740),
                     haul_type = 3,
                     abundance_haul = c("Y"),
                     sql_channel = NULL) {
  
  ## Set up channel if sql_channel = NULL
  if (is.null(sql_channel)) sql_channel <- get_connected()
  
  ## Pulling Cruise Data
  cat("Pulling Cruise Data \n")
  
  ## Query cruise data and filter survey regions of interest
  cruise_data <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = "SELECT * FROM RACEBASE.CRUISE")
  cruise_data <- subset(x = cruise_data, 
                        subset = cruise_data$REGION %in% survey_set)
  
  ## Query Haul data based on the CRUISEJOIN values in the cruise_data
  ## Filter for good tows (PERFORMANCE >= 0) and haul type (e.g., 3 is the
  ##   Standard bottom sample (preprogrammed station))
  cat("Pulling Haul Data \n")
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
  
  ## Query catch data based on cruisejoin values in haul_data and species set
  cat("Pulling Catch Data \n")
  cruisejoin_vec <-
    paste0("(", paste(unique(haul_data$CRUISEJOIN), collapse=", "), ")")
  spp_codes_vec <-
    ifelse(test = is.null(spp_codes),
           yes = "",
           no = paste0("(", paste(as.character(spp_codes), collapse=", "), ")"))
  
  catch_data <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM RACEBASE.CATCH ",
                                   "where CRUISEJOIN in ", cruisejoin_vec,
                                   ifelse(test = is.null(spp_codes),
                                          yes = "",
                                          no = " and SPECIES_CODE in "),
                                   spp_codes_vec))
  
  ## Query stratum data
  cat("Pulling Stratum Data \n")
  
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
  
  if (survey_set == "BS") {
    ## BS strata live in the RACEBASE.STRATUM table. Stratum records are 
    ## are periodically updated when stratum areas change (e.g., dropping
    ## stations). So, we only pull the most recent record for a given stratum.
    stratum_data <- RODBC::sqlQuery(channel = sql_channel, 
                                    query = "SELECT * FROM RACEBASE.STRATUM")
    stratum_data <- subset(x = stratum_data, 
                           subset = haul_data$REGION %in% survey_set)
    
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
  
  ## Query species information
  cat("Pulling Species Data \n")
  species_info <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM RACEBASE.SPECIES where ",
                                   ifelse(test = is.null(spp_codes),
                                          yes = "",
                                          no = " SPECIES_CODE in "),
                                   spp_codes_vec))
  
  ## Put cruise, haul, catch, stratu, and species data into a list and return
  return(list(cruise = cruise_data,
              haul = haul_data,
              catch = catch_data,
              strata = stratum_data,
              species = species_info))
  
}
