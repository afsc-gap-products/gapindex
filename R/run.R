packages <- c("tidyverse","RODBC", "janitor")
source("R/package_check.R")


# If local copy of racebase tables does not exist, retrieve one -----------

if(!file.exists("data/local_racebase")){
  x <- askYesNo(msg = "No local racebase files detected in this directory. Would you like to download local copies now?")
  if(x){
  dir.create("data/local_racebase", recursive = TRUE)
  source("R/00_download_data_from_oracle.R")
  }
} 

# Prep data ---------------------------------------------------------------
source("R/01_cleanup_data.R")


# Get CPUE tables ---------------------------------------------------------
source("R/02_get_cpue.R")

x <- get_cpue(survey_area = "GOA", speciescode = 30060)
head(x)

# Get biomass estimates by stratum ----------------------------------------

source("R/03_get_biomass_stratum.R")

y <- get_biomass_stratum(speciescode = 30060, 
                         survey_area = "GOA")
head(y)

# Get total biomass estimates for the whole survey area -------------------

source("R/04_get_biomass_total.R")

z <- get_biomass_total(speciescode = 30060,
                       survey_area = "GOA")
