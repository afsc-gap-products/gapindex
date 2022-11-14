# Function by Jason Conner. 
setUser <- function(){
  
  v_user <- readline(prompt = "Oracle Username: ")
  assign("oracle_user", v_user, envir = .GlobalEnv)
  
  v_pw <- readline(prompt = "Oracle Password: ")
  assign("oracle_pw", v_pw, envir = .GlobalEnv)
  
}

getSQL <- function(query = "SELECT SYSDATE FROM DUAL"){
  # Checks for oracle user info for getSQL
  if (exists("oracle_user") == F | exists("oracle_pw") == F) {
    setUser()
  }
  
  
  
  # Function to set AFSC Oracle connection
  getConnect <- function(){
    repeat {
      success <- T
      withRestarts({
        goodConnect <- ROracle::dbConnect(drv=dbDriver('Oracle'),
                                          username=oracle_user,
                                          password=oracle_pw,
                                          dbname='afsc', bulk_read = 10000000L)
      }, tryAgain = function() {
        success <<- F
      }
      )
      if (success) break
    }
    
    return(goodConnect)
  }
  
  AFSC <- withCallingHandlers({
    tries <- 4L
    getConnect()
  }, error = function(w){
    tries <<- tries - 1L
    if (tries > 0) {
      message("Oracle connection failed. Re-enter User and Password:")
      message(paste0(tries," attempts remaining!"))
      sumfish:::setUser()
      invokeRestart("tryAgain")
    } else {
      message("Could not connect to AFSC Database. Please ensure that you have the correct user and password, and that your Oracle client is configured correctly.")
      stop()
    }
  }
  )
  
  queryResult <- dbGetQuery(AFSC, query)
  dbDisconnect(AFSC)
  
  return(queryResult)
}