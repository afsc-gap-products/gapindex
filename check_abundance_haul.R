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
mindurtable <- read.csv(here::here("data","mintowdurations.csv"))


correct_gear_types <- c(160:172,710:717)

haul2 <- haul %>%
  mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width)) %>% # bc distance in km and width in m
# This should match the EFFORT column in AI-->CPUE
  mutate(year = as.numeric(str_extract(cruise, "^\\d{4}"))) %>%
  left_join(mindurtable) %>%
  mutate(abundance_haul_mcs = case_when(performance >=0 & 
                                          haul_type==3 & 
                                          (duration*60)>=as.numeric(min_dur_minutes) & 
                                          gear %in% correct_gear_types & 
                                          !is.null(stratum) & 
                                          stratum != 0 
                                          ~ "Y", 
                                        TRUE ~ "N"
                                        )) %>% 
  filter(region == "AI")


haul2
all(haul2$abundance_haul == haul2$abundance_haul_mcs)
test <- haul2 %>% filter(abundance_haul != abundance_haul_mcs)

# Write mismatched rows to table
write.csv(test,file = "mismatched_AI.csv",row.names = FALSE)
