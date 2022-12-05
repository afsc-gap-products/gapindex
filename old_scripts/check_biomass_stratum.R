# Check biomass_stratum from table against values in RACEBASE (from Wayne)
source("R/01_cleanup_data.R")
source("R/02_get_cpue.R")
source("R/03_get_biomass_stratum.R")

# Wayne's tables from Oracle - source AFSC//Other Users//GOA//BIOMASS_STRATUM
wayne <- read.csv("data/biomass_stratum_2021.csv")

pop_dbe <- get_biomass_stratum(speciescode = 30060, survey_area = "GOA") %>%
  dplyr::arrange(year, stratum) %>%
  as.data.frame()
pop_palsson <- wayne %>%
  filter(SPECIES_CODE == 30060) %>%
  dplyr::arrange(YEAR, STRATUM) %>%
  janitor::clean_names()
  
# pop_palsson2 <- pop_palsson %>%
#   filter(STRATUM_BIOMASS > 0)
# boxplot(data = pop_palsson2, log(STRATUM_BIOMASS) ~ STRATUM)

nrow(pop_dbe)
nrow(pop_palsson)

colnames(pop_dbe)
colnames(pop_palsson)

head(pop_dbe)
head(pop_palsson)

plot(pop_dbe$mean_wgt_cpue, pop_palsson$mean_wgt_cpue)
plot(pop_dbe$biomass_var, pop_palsson$biomass_var)

# Compare dataframes
diffdf::diffdf(pop_palsson, pop_dbe, tolerance = 10)

# Look at individual diffs
pop_dbe[16, ]
pop_palsson[16,]
# Tese are the 1984 indices, which makes sense cuz that's when the errors in abundance_haul happen.

pop_dbe[63, ]
pop_palsson[63,]
# And these are the 1987 indices, where there is also an error in abundance_haul

pop_dbe[5, ]
pop_palsson[5,]
# These are also from an error in abundance_haul. My CPUEs are correct (should be NA because missing net mensuration data)

pop_dbe[74, ]
pop_palsson[74,]
#This is also because of the NA value in stratum 111 in 1984

pop_dbe[242, ]
pop_palsson[242,]

x <- cpue_w %>%
  rename(catch_kg = weight)
write.csv(x = x,file = "wayne_cpue.csv")
write.csv(x = x,file = "dbi_cpue.csv")

x2_wayne <- x %>%
  group_by(year, stratum) %>%
  dplyr::summarize(
    haul_count = length(unique(hauljoin)), # number of total abundance hauls
    mean_wgt_cpue = mean(wgtcpue, na.rm = TRUE),
    var_wgt_cpue = ifelse(haul_count <= 1, NA, var(wgtcpue) / haul_count),
    mean_num_cpue = mean(numcpue, na.rm = TRUE),
    var_num_cpue = ifelse(haul_count <= 1, NA, var(numcpue) / haul_count),
    catch_count = length(which(catch_kg > 0)) # number of hauls with nonzero catch
  ) %>%
  dplyr::ungroup() %>%
  select(
    year, stratum, 
    haul_count, catch_count,
    mean_wgt_cpue, var_wgt_cpue,
    mean_num_cpue, var_num_cpue
  ) %>%
  add_column(.after = "stratum", species_code = speciescode)

x2 <- x2 %>% filter(year==1984)

head(x2)
head(x2_wayne)
diffdf::diffdf(x2,x2_wayne)
# stratum 111
# cpue_oracle <- read.csv("data/cpue_table.csv")
# xx <- get_cpue(speciescode = 30060,survey_area = "GOA") %>%
#   filter(stratum==111 & year == 1984) %>% arrange(desc(haul.x))
# xx_pal <- cpue_oracle  %>%
#   janitor::clean_names() %>%
#   filter(year==1984 & stratum == 111 & species_code == 30060) %>%
#   arrange(desc(haul))
#
#
# nrow(xx)
# nrow(xx_pal)
#
# diffdf::diffdf(xx_pal,xx)
