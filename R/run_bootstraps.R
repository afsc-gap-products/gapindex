run_bootstraps <- function(nboot = 5, gapdata, gapcpue) {
  res <- lapply(seq_len(nboot), do_one_bootstrap,
                gapdata = gapdata,
                gapcpue = gapcpue)
  
  list(
    cpue = do.call(rbind, lapply(res, `[[`, "cpue")),
    biomass_stratum = do.call(rbind, lapply(res, `[[`, "biomass_stratum")),
    biomass_subarea = do.call(rbind, lapply(res, `[[`, "biomass_subarea")),
    biomass_total = do.call(rbind, lapply(res, `[[`, "biomass_total"))
  )
}
