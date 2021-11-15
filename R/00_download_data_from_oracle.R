# 00_download_data_from_oracle

# Setup folders for local files -------------------------------------------
if(!file.exists("data/local_racebase")) dir.create("data/local_racebase", recursive = TRUE)
if(!file.exists("data/local_race_data")) dir.create("data/local_race_data", recursive= TRUE)

# Load packages -----------------------------------------------------------
PKG <- c("RODBC")
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

# Secret database login info:
source("R/setup_channel.R")

# The setup_channel.R script sets up a channel using your Oracle username and pw:
# channel <- odbcConnect(dsn = "AFSC",
#                        uid = "", # change
#                        pwd = "", #change
#                        believeNRows = FALSE)
# 
# odbcGetInfo(channel)

##################DOWNLOAD TABLES##################################
# RACEBASE ----------------------------------------------------------------

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.CATCH")
write.csv(x=a, "./data/local_racebase/catch.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.HAUL")
write.csv(x=a, "./data/local_racebase/haul.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.LENGTH")
write.csv(x=a, "./data/local_racebase/length.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIMEN")
write.csv(x=a, "./data/local_racebase/specimen.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.STRATUM")
write.csv(x=a, "./data/local_racebase/stratum.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.STATIONS")
write.csv(x=a, "./data/local_racebase/stations.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIES")
write.csv(x=a, "./data/local_racebase/species.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIES_CLASSIFICATION")
write.csv(x=a, "./data/local_racebase/species_classification.csv", row.names = FALSE)


# RACE_DATA ---------------------------------------------------------------

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.HAULS")
write.csv(x=a, "./data/local_race_data/hauls.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.RACE_SPECIES_CODES")
write.csv(x=a, "./data/local_race_data/race_species_codes.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.VESSELS")
write.csv(x=a, "./data/local_race_data/vessels.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.TAXONOMIC_RANKS")
write.csv(x=a, "./data/local_race_data/taxonomic_ranks.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.SPECIES_TAXONOMIC")
write.csv(x=a, "./data/local_race_data/species_taxonomic.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.V_CRUISES")
write.csv(x=a, "./data/local_race_data/cruises.csv", row.names = FALSE)



# ADFG --------------------------------------------------------------------

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.LENGTH_ADFG")
write.csv(x=a, "./data/length_ADFG.csv", row.names = FALSE)

a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIMEN_ADFG")
write.csv(x=a, "./data/specimen_ADFG.csv", row.names = FALSE)



# GOA ---------------------------------------------------------------------

a<-RODBC::sqlQuery(channel, "SELECT * FROM GOA.GOA_STRATA")
write.csv(x=a, "./data/goa_strata.csv", row.names = FALSE)
