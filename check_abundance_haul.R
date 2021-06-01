# Does abundance_haul correctly classify all the tows?
# testing with AI only for now.

library(tidyverse)

# Load Oracle data (takes a while) ----------------------------------------
# This is in my general Oracle access project. csv files of all the RACEBASE tables.
a <- list.files(
  path = here::here("..", "Oracle connection", "data", "oracle", "racebase"),
  pattern = "\\.csv"
)

for (i in 1:length(a)) {
  b <- read.csv(file = paste0(here::here(
    "..", "Oracle connection",
    "data", "oracle", "racebase", a[i]
  )))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = a[i]), value = b)
}


# Look at haul data -------------------------------------------------------
head(haul)
nrow(haul)

# Pasted from running scripts.docx, which gives min tow duration per year
mindurtable <- tibble::tribble(
  ~year, ~min_dur_minutes,
  1980L,  6L,
  1983L,  4L,
  1986L,  9L,
  1991L,  9L,
  1994L, 10L,
  1997L, 10L,
  2000L, 10L,
  2002L, 10L,
  2004L,  9L,
  2006L, 10L,
  2010L,  3L,
  2012L,  3L,
  2014L,  1L,
  2016L,  4L,
  2018L,  7L
)

haul2 <- haul %>%
  mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width)) %>% # bc distance in km and width in m
# This should match the EFFORT column in AI-->CPUE
  mutate(year = as.numeric(str_extract(cruise, "^\\d{4}"))) %>%
  left_join(mindurtable)




