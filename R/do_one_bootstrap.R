#' Bootstrap a CPUE table from gapindex
#'
#' @param boot (numeric) bootstrap number.
#' @param gapcpue object created from gapindex::calc_cpue(). This can have multiple years of data, but should be one species and one region only. 
#'
#' @returns a resampled CPUE table with one row per sample.
#' @details
#' This function returns one single resample of a CPUE table. Usually, you would be running this function several times (1 for each bootstrap you want to do of the data). 
#' 
#' @export
#'
#' @examples
#' testdata <- gapindex::get_data(
#'   year_set = c(1996, 1999),
#'   survey_set = c("GOA", "AI", "EBS", "NBS", "BSS")[1],
#'   spp_codes = c(21740, 30060, 10110)[1],
#'   haul_type = 3,
#'   abundance_haul = c("Y", "N")[1],
#'   pull_lengths = FALSE
#' )
#' testcpue <- gapindex::calc_cpue(gapdata = testdata)
#'
#' x1 <- do_one_bootstrap(boot = 1, gapcpue = testcpue)
#' x2 <- do_one_bootstrap(boot = 2, gapcpue = testcpue)
#'
#' library(ggplot2)
#' ggplot(testcpue, aes(x = factor(YEAR), y = CPUE_KGKM2)) +
#'   geom_jitter(width = 0.05, height = 0.00001) +
#'   geom_jitter(data = x1, width = 0.05, color = "blue", alpha = 0.2) +
#'   geom_jitter(data = x2, width = 0.05, color = "red", alpha = 0.2)
do_one_bootstrap <- function(boot, gapcpue) {
  if (length(unique(gapcpue$SPECIES_CODE)) > 1) {
    stop("More than one species detected in gapdata object. Please filter your gapdata to a single species and try again. More than one year is ok.")
  }

  if (length(unique(gapcpue$DESIGN_YEAR)) > 1) {
    stop("More than one survey design detected in gapdata object. This may cause issues with your resampling. Suggest limiting to one survey design.")
  }

  if (length(unique(gapcpue$SURVEY)) > 1) {
    stop("More than one region detected in gapdata object. Filter your gapdata object to a single region and run again.")
  }

  # ensures each sample will be random but repeatable
  set.seed(boot)

  # split data by year
  yearly_data <- split(gapcpue, gapcpue[["YEAR"]])

  boot_list <- lapply(yearly_data, function(df_year) {
    # sample sizes within year
    samplesizes <- table(df_year[["STRATUM"]])
    # sort rows by stratum
    df_year <- df_year[order(df_year[["STRATUM"]], decreasing = FALSE), ]

    # run one bootstrap
    x <- sampling::strata(
      df_year,
      stratanames = "STRATUM",
      size = samplesizes,
      method = "srswr"
    )

    # return sampled rows
    out <- sampling::getdata(data = df_year, m = x)
    out <- out[, !(names(out) %in% c("ID_unit", "Prob", "Stratum"))]
    out$boot <- boot # track bootstrap #

    return(out)
  })

  # combine years
  result <- do.call(rbind, boot_list)

  rownames(result) <- NULL
  
  return(result)
}
