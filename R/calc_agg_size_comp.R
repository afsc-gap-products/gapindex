#' Calculate size composition across aggregated subareas
#'
#' @param region      character string. One of c("EBS_STANDARD", "EBS_PLUSNW", 
#'                       "NBS", "GOA", "AI")
#' @param size_comps  a dataframe of stratum biomass, result object from either
#'                       `AFSC.GAP.DBE::calc_size_stratum_AIGOA()` or 
#'                       `AFSC.GAP.DBE::calc_size_stratum_BS()`
#'
#' @return dataframe of size composition estimates across 
#'         subareas and across region.
#' @export
#'

calc_agg_size_comp <- function(size_comps,
                               region = c("EBS_STANDARD", "EBS_PLUSNW", "NBS",
                                          "GOA", "AI")[1]) {
  
  ## Error checks
  if (is.null(size_comps))
    stop("Please supply a size composition dataframe created using 
         `AFSC.GAP.DBE::calc_size_stratum_AIGOA()` or 
         `AFSC.GAP.DBE::calc_size_stratum_BS()`")
  
  if (!region %in% c("EBS_STANDARD", "EBS_PLUSNW", "NBS", "GOA", "AI"))
    stop("Argument `region` must be one of these options: 
         EBS_STANDARD, EBS_PLUSNW, NBS, GOA, AI. " )
  
  if (region == "GOA") {
    if (any(unique(size_comps$YEAR) > 2023))
      warning("The GOA total biomass across INPFC area and across depth zones
              only includes years 1987-2023. Starting from 2025, total biomass
              across NMFS areas will only be reported.")
  }
  
  if (region == "EBS_PLUSNW") {
    if ( any(unique(size_comps$YEAR) < 1987) ){
      
      stop("The (EBS + NW) output only includes years 1987-present. 
      Years 1982-1986 are NOT included for the (EBS + NW) output because 
      essentially no stations within strata 82 & 90 (subarea 8 & 9) 
      were sampled during those years.")
    }
  } 
  
  ## Filter subareas and strata based on region
  which_strata <- list(
    "EBS_STANDARD" = list(
      "Region" = list("EBS_Standard_Region" = c(10, 20, 
                                                31, 32,
                                                41, 42, 43, 
                                                50, 61, 62)),
      "Depth" = list("100" = c(10, 20),
                     "200" = c(31, 32, 41, 42, 43),
                     "300" = c(50, 61, 62)),
      "Subarea" = list("1" = 10, "2" = 20, "3" = c(31, 32),
                       "4" = c(41, 42, 43), "5" = 50, "6" = c(61, 62))),  
    "EBS_PLUSNW" = list(
      "Region" = list("EBS_PLUSNW_Region" = c(10, 20, 
                                              31, 32, 
                                              41, 42, 43, 
                                              50, 61, 62, 
                                              82, 90)),
      "Depth" = list("100" = c(10, 20),
                     "200" = c(31, 32, 41, 42, 43, 82),
                     "300" = c(50, 61, 62, 90)),
      "Subarea" = list("1" = 10, "2" = 20, "3" = c(31, 32),
                       "4" = c(41, 42, 43), "5" = 50, "6" = c(61, 62),
                       "8" = 82, "9" = 90)),
    
    "NBS" = list(
      "Region" = list("NBS_Region" = c(70, 71, 81))),
    
    "GOA" = list(
      "Region" = list(
        "GOA_Region" = c(10:13,  20:22,  30:33,  35,  40:41,  50,
                         110:112, 120:122, 130:134, 140:143, 150:151,
                         210, 220:221, 230:232, 240:241, 250:251,
                         310, 320, 330, 340:341, 350, 351, 
                         410, 420, 430, 440, 450, 
                         510, 520, 530, 540, 550)),
      
      "Depth" = list(
        "1 - 100 m" = c(10:13,  20:22,  30:33,  35,  40:41,  50),
        "101 - 200 m" = c(110:112, 120:122, 130:134, 140:143, 150:151),
        "201 - 300 m" = c(210, 220:221, 230:232, 240:241, 250:251),
        "301 - 500 m" = c(310, 320, 330, 340:341, 350:351),
        "501 - 700 m" = c(410, 420, 430, 440, 450),
        "701 - 1000 m" = c(510, 520, 530, 540, 550)),
      
      "INPFC" = list(
        "Shumagin" = c(10:13, 110:112, 210, 310, 410, 510),
        "Chirikof" = c(20:22, 120:122, 220:221, 320, 420, 520),
        "Kodiak" = c(30:33, 35, 130:134, 230:232, 330, 430, 530),
        "Yakutat" = c(40:41, 140:143, 240:241, 340:341, 440, 540),
        "Southeastern" = c(50, 150:151, 250:251, 350:351, 450, 550)),
      
      "INPFC by Depth" = list(
        "Shumagin 1 - 100 m" = 10:13,
        "Shumagin 101 - 200 m" = 110:112,
        "Shumagin 201 - 300 m" = 210,
        "Shumagin 301 - 500 m" = 310,
        "Shumagin 501 - 700 m" = 410,
        "Shumagin 701 - 1000 m" = 510,
        
        "Chirikof 1 - 100 m" = 20:22, 
        "Chirikof 101 - 200 m" = 120:122,
        "Chirikof 201 - 300 m" = 220:221, 
        "Chirikof 301 - 500 m" = 320,
        "Chirikof 501 - 700 m" = 420,
        "Chirikof 701 - 1000 m" = 520,
        
        "Kodiak 1 - 100 m" = c(30:33, 35), 
        "Kodiak 101 - 200 m" = 130:134, 
        "Kodiak 201 - 300 m" = 230:232, 
        "Kodiak 301 - 500 m" = 330, 
        "Kodiak 501 - 700 m" = 430,
        "Kodiak 701 - 1000 m" = 530,
        
        "Yakutat 1 - 100 m" = 40:41, 
        "Yakutat 101 - 200 m" = 140:143, 
        "Yakutat 201 - 300 m" = 240:241, 
        "Yakutat 301 - 500 m" = 340:341, 
        "Yakutat 501 - 700 m" = 440, 
        "Yakutat 701 - 1000 m" = 540,
        
        "Southeastern 1 - 100 m" = 50, 
        "Southeastern 101 - 200 m" = 150:151, 
        "Southeastern 201 - 300 m" = 250:251, 
        "Southeastern 301 - 500 m" = 350:351, 
        "Southeastern 501 - 700 m" = 450,
        "Southeastern 701 - 1000 m" = 550)),
    
    "AI" = list(
      Region = list(
        "AI_Region" = c(211:214, 221:224, 
                        311:314, 321:324, 411:414, 421:424, 
                        511:513, 521:523, 594, 611:614, 621:624, 
                        711:712, 721:722, 793:794)),
      "District" = list(
        "WAI" = c(211:214, 221:224),
        "CAI" = c(311:314, 321:324, 411:414, 421:424),
        "EAI" = c(511:513, 521:523, 594, 611:614, 621:624),
        "SBS" = c(711:712, 721:722, 793, 794)),
      
      "District by Depth" = list(
        "WAI 1 - 100 m" = c(211, 221),
        "WAI 101 - 200 m" = c(212, 222),
        "WAI 201 - 300 m" = c(213, 223),
        "WAI 301 - 500 m" = c(214, 224),
        
        "CAI 1 - 100 m" = c(311, 321, 411, 421),
        "CAI 101 - 200 m" = c(312, 322, 412, 422),
        "CAI 201 - 300 m" = c(313, 323, 413, 423),
        "CAI 301 - 500 m" = c(314, 324, 414, 424),
        
        "EAI 1 - 100 m" =   c(511, 521, 611, 621),
        "EAI 101 - 200 m" = c(512, 522, 612, 622),
        "EAI 201 - 300 m" = c(513, 523, 613, 623),
        "EAI 301 - 500 m" = c(594,      614, 624),
        
        "SBS 1 - 100 m" = c(711, 721),
        "SBS 101 - 200 m" = c(712, 722),
        "SBS 201 - 300 m" = c(793),
        "SBS 301 - 500 m" = c(794),
        
        "AI 1 - 100 m" =   c(211, 221, 311, 321, 411, 421, 511, 521, 611, 621),
        "AI 101 - 200 m" = c(212, 222, 312, 322, 412, 422, 512, 522, 612, 622),
        "AI 201 - 300 m" = c(213, 223, 313, 323, 413, 423, 513, 523, 613, 623),
        "AI 301 - 500 m" = c(214, 224, 314, 324, 414, 424, 594,      614, 624))
    ))[[region]]
  
  ## Filter all strata within the region
  stratum_size_comp <- 
    subset(x = size_comps,
           subset = size_comps$STRATUM %in% which_strata$Region[[1]] )
  
  ## For each subarea, sum across the stratum biomasses and variances to get
  ## the total biomass and variance. 
  subarea_size_comp <-
    c(subarea_size_comp, 
      lapply(X = seq_along(which_strata[[itype]]),
             lapply(X = seq_along(which_strata),
                    FUN = function(subarea) {
                      
                      ## Filter strata within subarea
                      subarea_size_comp <- subset(x = stratum_size_comp,
                                                  subset = size_comps$STRATUM %in%
                                                    which_strata[[itype]][[subarea]])
                      
                      if (nrow(subarea_size_comp) == 0) return(data.frame())
                      if (nrow(subarea_size_comp) > 0) {
                        return(cbind(data.frame(STRATUM = names(which_strata)[subarea]),
                                     stats::aggregate(cbind(MALES, 
                                                            FEMALES,
                                                            UNSEXED, 
                                                            TOTAL) ~
                                                        YEAR + SPECIES_CODE + LENGTH,
                                                      data = subarea_size_comp,
                                                      FUN = sum)))
                      }
                    })))
  
  ## rbind subarea sizecomps, reorder, and return
  subarea_size_comp <- do.call(what = rbind, args = subarea_size_comp)
  subarea_size_comp <- 
    subarea_size_comp[with(subarea_size_comp, 
                           order(SPECIES_CODE, STRATUM, YEAR, LENGTH)), ]
  
  return(subarea_size_comp)
  
}