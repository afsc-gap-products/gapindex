#' Provide a stratum-level summary of catch data
#'
#' This function calculates the biomass and population estimators described in
#' \itemize{
#'   \item{
#'     Wakabayashi, K., R. G. Bakkala, and M. S. Alton. 1985.
#'     Methods of the U.S.-Japan demersal trawl surveys, p. 7-29.
#'     In R. G. Bakkala and K. Wakabayashi (editors), Results of cooperative U.S.-Japan groundfish investigations in the Bering Sea during May-August 1979.
#'     Int. North Pac. Fish. Comm. Bull. 44.
#'   }
#' }
#' Totals for all strata are produced with stratum = "TOTAL".
#' @examples
#' summary2016 <- sumStratum(getRacebase(2016))
#'
#' @param racebase List of dataframes created by \code{\link{getRacebase}}
#'
#' @return The output is a dataframe containing summary values for each stratum. Summary values are included in stratum = "TOTAL", so filter
#' by stratum before attempting summary statistics. CPUE values are given in units (kg or count) per hectare.
#' @export

sumStratum <- function(racebase=NULL) {
   options(stringsAsFactors=F)
   haulSum <- sumHaul(racebase)
   RBstratum <- racebase[['stratum']]
   RBlength <- racebase[['raw_length']] %>%
      dplyr::group_by(CRUISEJOIN,HAULJOIN, SPECIES_CODE) %>%
      dplyr::summarize(
         lengthCount = sum(FREQUENCY)
      )

   stratumTest <- dplyr::setdiff(dplyr::select(haulSum,STRATUM),dplyr::select(RBstratum,STRATUM))
   if (nrow(stratumTest)!=0) {
      message("Error: Stratum table does not contain stratum found in the Haul table for selected cruise. Function sumStratum aborted.")
   }

   stratumRatio <- RBstratum %>%
      dplyr::semi_join(haulSum, by="STRATUM") %>%
      dplyr::mutate(
         areaRatio=STRATUM_AREA/sum(STRATUM_AREA),
         areaHA=STRATUM_AREA * 100
         )


   stratumSum <- haulSum %>%
      dplyr::select(-VOUCHER,-SUBSAMPLE_CODE) %>%
      dplyr::left_join(RBlength, by = c("CRUISEJOIN","HAULJOIN","SPECIES_CODE")) %>%
      tidyr::replace_na(list(lengthCount=0)) %>%
      dplyr::group_by(YEAR,STRATUM,SPECIES_CODE) %>%
      dplyr::summarize(
         wMeanCPUE = mean(wCPUE),
         wVarCPUE = (ifelse(n() <= 1, 0, var(wCPUE)/n())),
         nMeanCPUE = mean(nCPUE),
         nVarCPUE = (ifelse(n() <= 1, 0, var(nCPUE)/n())),
         nHauls = n(),
         nHaulWts = sum(WEIGHT>0),
         nHaulCts = sum(NUMBER_FISH>0),
         nHaulLen = sum(lengthCount>0)
         ) %>%
      dplyr::inner_join(dplyr::select(stratumRatio,STRATUM,areaHA,areaRatio), by="STRATUM") %>%
      dplyr::mutate(
         bioEstimate = areaHA*wMeanCPUE,
         bioVar = areaHA^2*wVarCPUE,
         popEstimate = areaHA*nMeanCPUE,
         popVar = areaHA^2*nVarCPUE,
         Fi = areaHA*(areaHA-nHauls)/nHauls
      ) %>%
      dplyr::mutate(
         bioCIlower = sumfish:::toZero(bioEstimate - qt(.025,nHauls-1, lower.tail=F)*sqrt(bioVar)),
         bioCIupper = bioEstimate + qt(.025,nHauls-1, lower.tail=F)*sqrt(bioVar),
         popCIlower = sumfish:::toZero(popEstimate - qt(.025,nHauls-1, lower.tail=F)*sqrt(popVar)),
         popCIupper = popEstimate + qt(.025,nHauls-1, lower.tail=F)*sqrt(popVar),
         wDfNumerator = Fi*wVarCPUE,
         wDfDenominator = ((Fi^2)*(wVarCPUE^2))/nHauls-1,
         nDfNumerator = Fi*nVarCPUE,
         nDfDenominator = ((Fi^2)*(nVarCPUE^2))/nHauls-1
      )

   totalSum <- stratumSum %>%
      dplyr::group_by(YEAR,SPECIES_CODE) %>%
      dplyr::summarize(
         wMeanCPUE = sum(wMeanCPUE*areaRatio),
         wVarCPUE= sum(wVarCPUE*areaRatio^2),
         nMeanCPUE = sum(nMeanCPUE*areaRatio),
         nVarCPUE= sum(nVarCPUE*areaRatio^2),
         nHauls = sum(nHauls),
         bioEstimate = sum(bioEstimate),
         bioVar = sum(bioVar),
         popEstimate = sum(popEstimate),
         popVar = sum(popVar),
         wNe =((sum(wDfNumerator))^2)/(sum(wDfDenominator)),
         nNe =((sum(nDfNumerator))^2)/(sum(nDfDenominator)),
         nHauls = sum(nHauls),
         nHaulWts = sum(nHaulWts),
         nHaulCts = sum(nHaulCts),
         nHaulLen = sum(nHaulLen)
         ) %>%
      dplyr::mutate(
         bioCIlower = sumfish:::toZero(bioEstimate - qt(.025,wNe,lower.tail=F)*sqrt(bioVar)),
         bioCIupper = bioEstimate + qt(.025,wNe,lower.tail=F)*sqrt(bioVar),
         popCIlower = sumfish:::toZero(popEstimate - qt(.025,nNe,lower.tail=F)*sqrt(popVar)),
         popCIupper = popEstimate + qt(.025,nNe,lower.tail=F)*sqrt(popVar)
      )

   summary <- dplyr::select(stratumSum, -areaHA, -areaRatio, -Fi, -wDfNumerator, -wDfDenominator,
                               -nDfNumerator, -nDfDenominator) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_at(vars(STRATUM), as.character) %>%
      dplyr::bind_rows(
         dplyr::mutate(dplyr::select(totalSum, -wNe, -nNe),STRATUM="TOTAL")
      ) %>%
      dplyr::inner_join(
         dplyr::select(racebase[['species']],SPECIES_CODE,SPECIES_NAME,COMMON_NAME),
         by = "SPECIES_CODE"
      ) %>%
      dplyr::select(
         year = YEAR,
         speciesCode = SPECIES_CODE,
         species = SPECIES_NAME,
         commonName = COMMON_NAME,
         stratum = STRATUM,
         wMeanCPUE,
         wVarCPUE,
         bioEstimate,
         bioVar,
         bioCIlower,
         bioCIupper,
         nMeanCPUE,
         nVarCPUE,
         popEstimate,
         popVar,
         popCIlower,
         popCIupper,
         nHauls,
         nHaulWts,
         nHaulCts,
         nHaulLen
         )

   return(summary)

}
