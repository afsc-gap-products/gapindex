library(getPass)

# Define RODBC connection to ORACLE
get.connected <- function(schema='AFSC', username, password){(echo=FALSE)
  if(!hasArg(username)) {
    username <- getPass(msg = "Enter your ORACLE Username: ")
  }
  if(!hasArg(password)) {
    password <- getPass(msg = "Enter your ORACLE Password: ")
  }
  channel <- RODBC::odbcConnect(paste(schema),paste(username),paste(password), believeNRows=FALSE)
}

# Execute the connection
channel <- get.connected()


odbcGetInfo(channel)
