#Check biomass_stratum from table against values in RACEBASE (from Wayne)
source("R/01_cleanup_data.R")
source("R/02_get_cpue.R")
source("R/03_get_biomass_stratum.R")

# Wayne's tables from Oracle - source AFSC//Other Users//GOA//BIOMASS_STRATUM
wayne <- read.csv("data/biomass_stratum_2021.csv")

pop_dbe <- get_biomass_stratum(speciescode = 30060,survey_area = "GOA")
pop_palsson <- wayne %>% filter(SPECIES_CODE==30060) %>% arrange(YEAR,STRATUM)

pop_palsson2 <- pop_palsson %>% filter(STRATUM_BIOMASS>0)
boxplot(data = pop_palsson2, log(STRATUM_BIOMASS)~STRATUM)

nrow(pop_dbe)
nrow(pop_palsson)

colnames(pop_dbe)
colnames(pop_palsson)

head(pop_dbe)
head(pop_palsson)

plot(pop_dbe$mean_wgt_cpue,pop_palsson$MEAN_WGT_CPUE)

# Compare dataframes
diffdf::diffdf(janitor::clean_names(pop_palsson),pop_dbe)

# First difference
pop_dbe[16,]

#stratum 111
xx <- get_cpue(speciescode = 30060,survey_area = "GOA") %>% 
  filter(stratum==111 & year == 1984)
xx_pal <- read.csv("data/cpue_table.csv") %>% filter(YEAR==1984)
