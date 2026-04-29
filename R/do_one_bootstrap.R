do_one_bootstrap <- function(boot, gapdata) {
  i <- boot
  set.seed(i)
  
  cpue_base <- gapindex::calc_cpue(gapdata = gapdata)

  # split data by year
  yearly_data <- split(cpue_base, cpue_base[["YEAR"]])

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
    out <- out[, !(names(out) %in% c("ID_unit","Prob", "Stratum"))]
    return(out)
  })

  # combine years
  result <- do.call(rbind, boot_list)

  rownames(result) <- NULL
  result
}