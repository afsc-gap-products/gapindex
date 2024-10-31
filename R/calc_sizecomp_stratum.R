#' Calculate numerical CPUE at length by haul
#' 
#' @description Uses Equation 16 in Wakabayashi et al. 1985 to calculate
#'              numbers-catch-per-unit-effort (CPUE, Numbers/km2) at length.
#'            
#' @param racebase_tables `r lifecycle::badge("deprecated")` Use the 
#'                     `gapdata` argument instead. 
#' @param gapdata data object created from `gapindex::get_data()`
#' @param racebase_cpue `r lifecycle::badge("deprecated")` Use the 
#'                     `cpue` argument instead. 
#' @param cpue catch per unit effort object created from `gapindex::calc_cpue()`.
#' @param abundance_stratum biomass/abundance by stratum object created from 
#'                          `gapindex::calc_biomass_stratum()`
#' @param racebase_stratum_popn `r lifecycle::badge("deprecated")` Use the 
#'                     `abundance_stratum` argument instead. 
#' @param spatial_level string, one of c("stratum", "haul"). Should size 
#'                      compositions be calculated at the level of the 
#'                      "stratum" (used for standard size compositions) or 
#'                      "haul" (used for ModSquad model-based data-inputs). 
#' @param fill_NA_method string, one of c("AIGOA", "BS"). This argument changes
#'                       the way hauls with positive weights but no associated 
#'                       size data are dealt with. In the EBS, NBS, and BSS 
#'                       survey regions ("BS"), these hauls contribute to the 
#'                       dummy length -9 category for their respective strata. 
#'                       In the AI and GOA survey regions ("AIGOA"), an average
#'                       size distribution is applied to these hauls so the 
#'                       length -9 category does not exist in the AI or GOA
#'                       versions of the size composition.   
#'                                  
#' @eval c("@return", get_table_metadata("inst/extdata/metadata.csv", 
#' select = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR", "STRATUM", 
#' "SPECIES_CODE", "LENGTH_MM", "SEX", "POPULATION_COUNT")))
#' 
#' @export
#' 

calc_sizecomp_stratum <- function(racebase_tables = deprecated(),
                                  gapdata = NULL,
                                  racebase_cpue = deprecated(),
                                  cpue = NULL,
                                  racebase_stratum_popn = deprecated(),
                                  abundance_stratum = NULL,
                                  spatial_level = c("stratum", "haul")[1],
                                  fill_NA_method = c("AIGOA", "BS")[1]) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Error checks in data input
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Error Check on function arguments
  if (lifecycle::is_present(racebase_tables)) {
    lifecycle::deprecate_warn("3.0.0", 
                              "calc_sizecomp_stratum(racebase_tables)", 
                              "calc_sizecomp_stratum(gapdata)")
    gapdata <- racebase_tables
  }
  if (lifecycle::is_present(racebase_cpue)) {
    lifecycle::deprecate_warn("3.0.0", 
                              "calc_sizecomp_stratum(racebase_cpue)", 
                              "calc_sizecomp_stratum(cpue)")
    cpue <- racebase_cpue
  }
  if (lifecycle::is_present(racebase_stratum_popn)) {
    lifecycle::deprecate_warn("3.0.0", 
                              "calc_sizecomp_stratum(racebase_stratum_popn)", 
                              "calc_sizecomp_stratum(abundance_stratum)")
    abundance_stratum <- racebase_stratum_popn
  }
  
  for (iarg in c("gapdata", "cpue", "abundance_stratum"))
    if (is.null(x = get(x = iarg)))
      stop(paste0("Must provide argument `", iarg, "`. ",
                  "See ?gapindex::calc_sizecomp_stratum for more information"))
  
  ## Check that there are size data
  if (is.null(x = gapdata$size)) 
    stop("gapdata$size must not be NULL. Either the taxon does not 
         have size information or rerun gapindex::get_data() with 
         argument pull_lengths = TRUE")
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Gather datasets from `gapdata`
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cruise <- gapdata$cruise
  haul <- gapdata$haul
  size <- gapdata$size
  
  ## Only subset to species with size data
  abundance_stratum <- 
    subset(x = abundance_stratum,
           subset = SPECIES_CODE %in% unique(x = size$SPECIES_CODE))
  
  ## Attach "YEAR" and "SURVEY" columns from `haul` to `size` using column
  ## "CRUISEJOIN" as the key.
  haul <- merge(x = haul,
                y = cruise[, c("CRUISEJOIN", "SURVEY", "YEAR")],
                by = "CRUISEJOIN")
  
  ## Attach "YEAR", "SURVEY", and "STRATUM" information from `haul` to `size` 
  ## using "HAULJOIN" as the key.
  size <- merge(x = size[, c("HAULJOIN",  "SPECIES_CODE", "LENGTH",
                             "FREQUENCY", "SEX")],
                y = haul[, c("HAULJOIN", "SURVEY", "YEAR", "STRATUM")],
                by = "HAULJOIN")
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Wakabayashi et al. 1985 Equation 16: 
  ##
  ## At each station with length-frequency records, the number of individuals
  ##     within each sex/length class was estimated by expanding the 
  ##     length-frequency subsample to the total catch (per area swept). 
  ##     This is S_ijklm, the estimated relative number of individuals of 
  ##     species-k with sex-m and length-l caught at station-j within stratum-i.
  ##
  ## s_ijklm is the "FREQUENCY" column of the `size`, the number of records
  ##     of sex-m and length-l of species-k recorded at station-j in stratum-i
  ## s_ijk is the frequency of recorded individuals summed over sex and length
  ##        class, i.e., the number of individuals of species-k recorded 
  ##        at station-j in stratum-i summed over all length and sex classes
  ## S_ijk is the estimated numbers per area swept from the cpue df
  ##     of species-k at station-j in stratum-i. The notation S_ijk is not 
  ##     used below, instead CPUE_NOKM2 is used. 
  ##
  ## s_ijk and NUMCPUE_IND_KM2 are merged into the size df by joining 
  ##     via the "HAULJOIN" and "SPECIES_CODE" as a composite key
  ##     so that calculations can be vectorized easily.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  s_ijk <- size[, 
                .("s_ijk" = sum(FREQUENCY, na.rm = T)), 
                by = c("SURVEY", "YEAR", "HAULJOIN", "SPECIES_CODE")]
  
  ## Merge the "s_ijk" column from `s_ijk` to `size` using "SPECIES_CODE"
  ## and "HAULJOIN" as a composite key. 
  size <- merge(x = size,
                y = s_ijk[, c("SPECIES_CODE", "HAULJOIN", "s_ijk")],
                by = c("SPECIES_CODE", "HAULJOIN"))
  
  ## Merge the "CPUE_NOKM2" column from `cpue` to `size` using "SPECIES_CODE"
  ## and "HAULJOIN" as a composite key. 
  size <- merge(x = size,
                y = cpue[, c("HAULJOIN", "SPECIES_CODE", "CPUE_NOKM2")],
                by = c("HAULJOIN", "SPECIES_CODE"))
  
  ## The CPUE at size is (S_ijklm)
  size$S_ijklm <- size$FREQUENCY / size$s_ijk * size$CPUE_NOKM2
  
  if (fill_NA_method == "AIGOA") {
    ## For AI and GOA survey  region, the size composition for hauls with 
    ## positive counts but missing size data is imputted by averaging the 
    ## size composition from the hauls in that same stratum and year. 
    
    ## Query hauls with positive counts but have no records in `size`
    missing_hauljoins <- data.frame()
    
    for (ispp in sort(x = unique(x = size$SPECIES_CODE)) ) 
    { ## Loop over species -- start
      temp_hauljoins <- unique(x = size$HAULJOIN[size$SPECIES_CODE == ispp])
      
      missing_hauljoins <- rbind(missing_hauljoins,
                                 subset(x = cpue,
                                        subset = CPUE_NOKM2 > 0 & 
                                          SPECIES_CODE == ispp &
                                          !HAULJOIN %in% temp_hauljoins))
    } ## Loop over species -- end
    
    ## Calculate mean S_ijklm of individuals of species-k with sex-m and 
    ## length-l among the hauls within stratum-i. Technically, this would be
    ## something like S_hat_iklm. 
    size$p_ijklm <- with(size, FREQUENCY / s_ijk)
    
    ## First we sum all the proportions within a stratum...
    imputted_size <- size[,
                          .(p_ijklm = sum(p_ijklm)),
                          by = c("SURVEY", "YEAR", "STRATUM", 
                                 "SPECIES_CODE", "SEX", "LENGTH")]
    
    ## Then we calculate the total number of unique hauls within a stratum...
    hauls_w_length_by_stratum <-
      size[,
           .(HAULJOIN_TOTAL = (function(x) 
             length(x = unique(x = x$HAULJOIN)))(.SD)),
           by = c("SURVEY", "YEAR", "STRATUM", "SPECIES_CODE")]
    
    ## Then we calculate the average proportion. Merge the "HAULJOIN_TOTAL"
    ## column from `hauls_w_length_by_stratum` to imputted_size using
    ## "SURVEY", "YEAR", "STRATUM", "SPECIES_CODE" as a composite key.
    imputted_size <- merge(x = imputted_size,
                           y = hauls_w_length_by_stratum,
                           by = c("SURVEY", "YEAR", "STRATUM", "SPECIES_CODE"))
    imputted_size$p_ijklm <- with(imputted_size, p_ijklm / HAULJOIN_TOTAL)
    
    ## Merge this mean S_hat_iklm to `missing_hauljoins` using column 
    ## "SURVEY", "YEAR", "STRATUM", "SPECIES_CODE" as a composite key.
    imputted_size <- merge(x = missing_hauljoins,
                           y = imputted_size,
                           by = c("SURVEY", "YEAR", "STRATUM", "SPECIES_CODE"))
    
    ## Recalculate S_ijklm using the imputted size composition. 
    imputted_size$S_ijklm <- with(imputted_size, p_ijklm * CPUE_NOKM2)
    
    ## Append the now imputted `missing_hauljoins` to size.
    size <- rbind(size[, c("HAULJOIN", "STRATUM", "SPECIES_CODE", "LENGTH", 
                           "SEX", "SURVEY", "YEAR", "CPUE_NOKM2", "S_ijklm")], 
                  imputted_size[, c("HAULJOIN", "STRATUM", "SPECIES_CODE", 
                                    "LENGTH", "SEX", "SURVEY", "YEAR", 
                                    "CPUE_NOKM2", "S_ijklm")])
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Stopping point if spatial_level == "HAUL"
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (spatial_level == "haul") {
    names(x = size)[names(x = size) == "S_ijklm"] <- "S_ijklm_NOKM2"  
    names(x = size)[names(x = size) == "LENGTH"] <- "LENGTH_MM"
    
    return(subset(x = size,
                  select = c(HAULJOIN, SURVEY, YEAR, SPECIES_CODE, 
                             SEX, LENGTH_MM, S_ijklm_NOKM2) ))
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Continuing on if `spatial_level` == "STRATUM" with Wakabayashi et al. 
  ##   1985 Equation 17: The number of individuals (population) of species-k 
  ##   of sex-m and length-l in stratum-i is noted as P_iklm.
  ##   
  ## S_ijklm is summed over hauls-j, creating S_iklm and this is divided
  ##    by S_ik, which is S_ijklm summed over hauls-j, sex-m, and length-l.
  ## S_iklm / S_ik is the proportion of the stratum population attributed 
  ##    to sex-m and length-l for species-k in stratum-i. When multiplied by 
  ##    P_ik, we get P_iklm.
  ##
  ## P_ik (abundance_stratum$pop) and S_ik are merged into the S_iklm
  ##    df via the year, stratum, and species_code values. Wakabayashi et al.
  ##    1985 assumes one year of data so there is no year index in their 
  ##    paper but this function has the capability of calculating size comps
  ##    for multiple years of data.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Aggregate S_ijklm across stratum and species_code
  S_ik <- 
    size[, 
         .("S_ik" = sum(S_ijklm, na.rm = TRUE)), 
         by = c("SURVEY", "YEAR", "STRATUM", "SPECIES_CODE")]
  
  ## Aggregate S_ijklm across stratum, species_code, length bin, and sex
  S_iklm <- 
    size[,
         .("S_iklm" = sum(S_ijklm, na.rm = TRUE)),
         by = c("SURVEY", "YEAR", "STRATUM", "SPECIES_CODE", "LENGTH", "SEX")]
  
  S_iklm <- merge(x = S_iklm,
                  y = S_ik,
                  by = c("SURVEY", "YEAR", 'STRATUM', "SPECIES_CODE"))
  
  ## Merge "POPULATION_COUNT" column from `abundance_stratum` to `S_iklm`
  ## using "SURVEY", "YEAR", 'STRATUM', "SPECIES_CODE" as a composite key. 
  S_iklm <- merge(x = S_iklm,
                  y = abundance_stratum[, c("SURVEY", "YEAR", 'STRATUM',
                                            "SPECIES_CODE",
                                            "POPULATION_COUNT")],
                  by = c("SURVEY", "YEAR", 'STRATUM', "SPECIES_CODE"),
                  all = TRUE)
  
  ## There are some strata with no length data. In these cases, the length
  ## is coded as -9 and sex = 3. S_ik and S_ik are set to 1 to ease calculations
  S_iklm$LENGTH[is.na(x = S_iklm$LENGTH)] <- -9
  S_iklm$SEX[is.na(x = S_iklm$SEX)] <- 3
  S_iklm$S_iklm[is.na(x = S_iklm$S_iklm)] <- 1
  S_iklm$S_ik[is.na(x = S_iklm$S_ik)] <- 1
  
  if (fill_NA_method == "BS") {
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##   In the Bering Sea design based composition calculations, there are 
    ##   some strata in some years where lengths data were not collected. 
    ##   To account for these individuals with no length in the stratum, these
    ##   records are designated with a dummy length class of -9 and SEX = 3.
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    S_iklm$LENGTH[is.na(x = S_iklm$LENGTH)] <- -9 ## dummy variable
    S_iklm$SEX[is.na(x = S_iklm$SEX)] <- 3
  }
  
  ## Stratum-level population calculation
  S_iklm$NUMBER <- 
    round(x = S_iklm$POPULATION_COUNT * S_iklm$S_iklm / S_iklm$S_ik)
  
  ## Remove zero-records from the size composition
  S_iklm <- subset(x = S_iklm, 
                   subset = NUMBER > 0,
                   select = -c(S_iklm, S_ik, POPULATION_COUNT))
  
  ## Rename "NUMBER" column back to "POPULATION_COUNT" and add units to LENGTH
  data.table::setnames(x = S_iklm, 
                       old = c("NUMBER", "LENGTH"),
                       new = c("POPULATION_COUNT", "LENGTH_MM"))
  
  ## Add SURVEY_DEFINITION_ID to output
  S_iklm <- merge(x = S_iklm,
                  y = gapdata$survey,
                  by = c("SURVEY", "YEAR"))
  
  S_iklm <- 
    subset(x = data.table::data.table(
      S_iklm,
      key = c("SURVEY_DEFINITION_ID", "SURVEY", "YEAR", "STRATUM",
              "SPECIES_CODE", "LENGTH_MM", "SEX")),
      select = c(SURVEY_DEFINITION_ID, SURVEY, YEAR, STRATUM,
                 SPECIES_CODE, LENGTH_MM, SEX, POPULATION_COUNT)
    )
  
  return(S_iklm)
}
