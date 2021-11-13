# Channel info for Oracle
setUser <- function(){
  
  v_user <- readline(prompt = "Oracle Username: ")
  assign("oracle_user", v_user, envir = .GlobalEnv)
  
  v_pw <- readline(prompt = "Oracle Password: ")
  assign("oracle_pw", v_pw, envir = .GlobalEnv)
  
}

setUser()

channel <- odbcConnect(dsn = "AFSC",
                      uid = oracle_user, # change
                      pwd = oracle_pw, #change
                      believeNRows = FALSE)

odbcGetInfo(channel)
