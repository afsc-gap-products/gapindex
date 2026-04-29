bootstrap_stratum_biomass <- function(gapdata, nboots, ncpus = 4){
  # For testing
  gapdata <- gapindex::get_data(
    year_set = c(1996, 1999),
    survey_set = c("GOA", "AI", "EBS", "NBS", "BSS")[1],
    spp_codes = c(21740, 30060, 10110)[1],
    haul_type = 3,
    abundance_haul = c("Y", "N")[1],
    pull_lengths = FALSE
  )
  
  
  # Parallelize bootstraps across your computer's CPUs
  snowfall::sfInit(parallel = TRUE, cpus = ncpus) # Change CPUs as needed
  snowfall::sfExport("goa_data")
  
  xx <- snowfall::sfLapply(x = 1:nboots, fun = snow_sims) 
  
  # NOT FINISHED!!!!
}