# Step 1: Get RACEBASE data -----------------------------------------------
# Where to put the cleaned/wrangled ---------------------------------------
library(tidyverse)

# Load Oracle data (takes a while) ----------------------------------------
# This local folder contains csv files of all the RACEBASE tables.
a <- list.files(
  path = here::here("data","local_racebase"),
  pattern = "\\.csv"
)

for (i in 1:length(a)) {
  b <- read.csv(file = paste0(here::here(
    "data","local_racebase", a[i]
  )))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = a[i]), value = b)
}


# Get stratum areas from GOA_STRATA table----------------------------------
goa_ai_strata <- read.csv(here::here(
  "data",
  "goa_strata.csv"
))

goa_strata <- goa_ai_strata %>%
  janitor::clean_names() %>%
  filter(survey == "GOA")

ai_strata <- goa_ai_strata %>%
  janitor::clean_names() %>%
  filter(survey == "AI")

# Add area swept to each haul ---------------------------------------------
head(haul)
nrow(haul)
haul <- haul %>%
  mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width)) # bc distance in km and width in m
# This should match the EFFORT column in AI/CPUE

# Get cruise info from RACE.DATA ------------------------------------------

cruisedat <- cruise %>%
  dplyr::select(survey_name, region,
                cruisejoin,
                agency_name) 
cruisedat$start_date <- as.Date(cruise$start_date)
cruisedat$year <- lubridate::year(cruisedat$start_date)