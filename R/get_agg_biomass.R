#' Calculate index of total biomass across aggregated strata
#'
#' @param region         character string. One of c("EBS_STANDARD", "EBS_PLUSNW", 
#'                       "NBS", "GOA", "AI")
#' @param biomass_strata a dataframe of stratum biomass, result object from 
#'                       `get_biomass_stratum()`
#'
#' @return dataframe of biomass and population abundance estimates across 
#'         subareas and across the region, along with variances (CIs to be 
#'         added when we can figure out how to do them...) 
#' @export
#' 

get_agg_biomass <- function(biomass_strata = NULL,
                            region = c("EBS_STANDARD", "EBS_PLUSNW", "NBS",
                                       "GOA", "AI")[2]) {
  
  ## Checks
  if (region == "GOA") {
    if (any(unique(biomass_strata$YEAR) > 2021))
      warning("The GOA total biomass across INPFC area and across depth zones
              only includes years 1987-2021. Starting from 2023, total biomass
              across NMFS areas will only be reported.")
  }
  
  if (region == "EBS_PLUSNW") {
    if ( any(unique(biomass_strata$YEAR) < 1987) ){
      
      stop("The (EBS + NW) output only includes years 1987-present. 
      Years 1982-1986 are NOT included for the (EBS + NW) output because 
      essentially no stations within strata 82 & 90 (subarea 8 & 9) 
      were sampled during those years.")
    }
  } 
  
  ## Filter subareas and strata based on region
  which_strata <- list(
    "EBS_STANDARD" = list("999" = c(10,20, 31,32,41,42,43, 50,61,62),
                          ## Depth Subareas
                          "100" = c(10, 20),
                          "200" = c(31,32,41,42,43),
                          "300" = c(50,61,62),
                          ## Subareas
                          "1" = 10, "2" = 20, "3" = c(31, 32),
                          "4" = c(41, 42, 43), "5" = 50, "6" = c(61, 62)),
    
    "EBS_PLUSNW" = list("999" = c(10,20, 31,32,41,42,43, 50,61,62, 82, 90),
                    ## Depth Subareas
                    "100" = c(10, 20),
                    "200" = c(31,32,41,42,43, 82),
                    "300" = c(50,61,62, 90),
                    ## Subareas
                    "1" = 10, "2" = 20, "3" = c(31, 32),
                    "4" = c(41, 42, 43), "5" = 50, "6" = c(61, 62),
                    "8" = 82, "9" = 90),
    
    "NBS" = list("999" = c(70, 71, 81)),
    
    "GOA" = list("999" = c(10:13,  20:22,  30:33,  35,  40:41,  50,
                           110:112, 120:122, 130:134, 140:143, 150:151,
                           210, 220:221, 230:232, 240:241, 250:251,
                           310, 320, 330, 340:341, 350, 351, 
                           410, 420, 430, 440, 450, 
                           510, 520, 530, 540, 550),
                 "0 - 100 m" = c(10:13,  20:22,  30:33,  35,  40:41,  50),
                 "101 - 200 m" = c(110:112, 120:122, 130:134, 140:143, 150:151),
                 "201 - 300 m" = c(210, 220:221, 230:232, 240:241, 250:251),
                 "301 - 500 m" = c(310, 320, 330, 340:341, 350:351),
                 "501 - 700 m" = c(410, 420, 430, 440, 450),
                 "701 - 1000 m" = c(510, 520, 530, 540, 550),
                 "Shumagin" = c(10:13, 110:112, 210, 310, 410, 510),
                 "Chirikof" = c(20:22, 120:122, 220:221, 320, 420, 520),
                 "Kodiak" = c(30:33, 35, 130:134, 230:232, 330, 430, 530),
                 "Yakutat" = c(40:41, 140:143, 240:241, 340:341, 440, 540),
                 "Southeastern" = c(50, 150:151, 250:251, 350:351, 450, 550)),
    
    "AI" = list("999" = c(211:214, 221:224, 
                          311:314, 321:324, 411:414, 421:424, 
                          511:513, 521:523, 594, 611:614, 621:624, 
                          711:712, 721:722, 793:794),
                "WAI 1 - 100 m" = c(211, 221),
                "WAI 101 - 200 m" = c(212, 222),
                "WAI 201 - 300 m" = c(213, 223),
                "WAI 301 - 500 m" = c(214, 224),
                "WAI" = c(211:214, 221:224),
                
                "CAI 1 - 100 m" = c(311, 321, 411, 421),
                "CAI 101 - 200 m" = c(312, 322, 412, 422),
                "CAI 201 - 300 m" = c(313, 323, 413, 423),
                "CAI 301 - 500 m" = c(314, 324, 414, 424),
                "CAI" = c(311:314, 321:324, 411:414, 421:424),
                
                "EAI 1 - 100 m" =   c(511, 521, 611, 621),
                "EAI 101 - 200 m" = c(512, 522, 612, 622),
                "EAI 201 - 300 m" = c(513, 523, 613, 623),
                "EAI 301 - 500 m" = c(594,      614, 624),
                "EAI" = c(511:513, 521:523, 594, 611:614, 621:624),
                
                "SBS 1 - 100 m" = c(711, 721),
                "SBS 101 - 200 m" = c(712, 722),
                "SBS 201 - 300 m" = c(793),
                "SBS 301 - 500 m" = c(794),
                "SBS" = c(711:712, 721:722, 793, 794)))[[region]]
  
  ## Filter all strata within the region
  stratum_biomass <- 
    subset(x = biomass_strata,
           subset = biomass_strata$STRATUM %in% which_strata[["999"]] )
  
  ## For each subarea, sum across the stratum biomasses and variances to get
  ## the total biomass and variance. 
  subarea_biomass <-
    lapply(X = seq_along(which_strata),
           FUN = function(subarea) {
             
             ## Filter strata within subarea
             subarea_biomass <- subset(x = stratum_biomass,
                                       subset = biomass_strata$STRATUM %in%
                                         which_strata[[subarea]])
             
             if (nrow(subarea_biomass) == 0) return(data.frame())
             if (nrow(subarea_biomass) > 0) {
               return(cbind(data.frame(STRATUM = names(which_strata)[subarea]),
                            stats::aggregate(cbind(biomass_mt, 
                                                   biomass_var,
                                                   haul_count) ~
                                               group + YEAR,
                                             data = subarea_biomass,
                                             FUN = sum)))
             }
           })
  
  ## rbind subarea biomasses and return
  return(do.call(what = rbind, args = subarea_biomass))
}
