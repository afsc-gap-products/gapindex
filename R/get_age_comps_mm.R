#' Get age comps for a species
#' @description All of this code is converted from a file found in G:/GOA/R/agecomp. Any notes straight from Megsie will be annotated, otherwise you can assume they are the original documentation.
#' Location: G:/GOA/R/agecomp/
#  Purpose:  Estimate age compositions for a subset of a species defined by length
#            This function was constructed to estimate age compositions for Pacific
#            cod less than 27 cm or greater than or equal to 27 cm.  Probably will
#            not be used for any other species or size breaks, but it is generalized
#            for anything in the Aleutian or GOA surveys.
#' @param survey "GOA" (the default; not yet generalized)
#' @param t.username oracle username, default is NULL, which leads to a query to user
#' @param t.password oracle password, default is NULL, which leads to a query to user
#' @param t.species integer value: the RACE species code for the species of interest. Default: 21720 (default: Pacific cod)
#' @param t.cut 270 millimeters(default)
#'            this is the length that defines the bin
#'           this length is ALWAYS included in the larger bin
#'            e.g. select small bin as < t.cut, large bin as >= t.cut
#'            NOTE: the cut length is in MILLIMETERS, not cm.  This is to agree
#'                  with the units of measure in the data base.  This length
#'                  gets converted to cm in the guts of this function.
#' @param t.which.bin #    t.which.bin = "smaller" or "larger"
#'                  default: "larger"
#' @details  the arguments t.species, t.cut, and t.which.bin are going to be
#'          coerced into character class and used in the paste function for
#'         calls to Oracle through RODBC commands.  Actually, in the case
#'          of t.which.bin, the argument will be changed to the character
#'          string "<" or ">=" and then pasted in place in a Sequel command.
#'
#' @return a dataframe containing the following elements: 
#'		$SURVEY = GOA or AI
#'		$SURVEY_YEAR = a numeral representing a temporal abstraction
#'		$SPECIES_CODE = a RACEy sort of thing, no units
#'		$SEX = yes, no, maybe later after this headache goes away
#'		$AGE = units of measure: years
#'		$AGEPOP = numbers of fish of the this age
#'	$MEAN_LENGTH = mean length of fish of this age in cm
#'		$STANDARD_DEVIATION = I really don't know yet what this is (Munro, speaking)
#' @export 
#'
#' @examples
get_age_comps_mm <- function(survey = "GOA", t.username = NULL, t.password = NULL, t.species = 21720, t.cut = 270, t.which.bin = "larger"){
  #    
  #
  #-------------------------
  #
  #  History:
  #    Originally constructed by Michael Martin; Michael's function is now called
  #    'agecomp.cod.gt27.original.by.MMartin'.  Michael gave that function to Teresa
  #    A'mar.  In 2014, Teresa tweaked it to work for her.  Teresa gave her function
  #    to Wayne Palsson, who passed it along to Peter Munro, who is turning it into
  #    the function you are looking at now.  The moment is 2014.  All the R code
  #    that follows is Michael's code wedded to Teresa's code, then modified by
  #    Munro, building on the object Teresa built.
  #
  #-------------------------
  #
  #
  #  Setting up a channel to Oracle.
  #
  if(is.null(t.username)){
    cat("\n")
    cat("\n")
    cat("Type your sql username\n")
    cat("\n")
    cat("\n")
    t.username <- readline()
  }
  if(is.null(t.password)){
    cat("\n")
    cat("\n")
    cat("Type your sql password\n")
    cat("\n")
    cat("\n")
    t.password <- readline()
  }
  #browser()
  #
  #
  # Set up a channel to the database, called goa.db
  #
  goa.db <- RODBC::odbcConnect("AFSC", t.username, t.password, believeNRows = FALSE)
  #browser()
  #
  #
  #--------------------------------------
  #
  #
  #  Go into Oracle and grab specimen data, specifically the age data,
  #  for all surveys
  #
  #  Construct the Sequel command that we want to make
  #
  #  Set the input arguments to proper class and value for pasting into a command:
  #
  t.species <- as.character(t.species)
  t.cut <- as.character(t.cut)
  if(t.which.bin == "smaller"){
    t.which.bin <- "<"
  }
  if(t.which.bin == "larger"){
    t.which.bin <- ">="
  }
  #  Paste the command together
  #
  t.sequel.command <- paste("select floor(cruise/100) year, cruisejoin, species_code, specimenid, length length_mm, sex, age from racebase.specimen where cruisejoin in (select cruisejoin from goa.biennial_surveys where survey = '", survey, "') and age is not null and species_code = ", t.species, " and length ", t.which.bin, " ", t.cut, sep = "")
  #browser()
  #
  #  Do the actual query to Sequel
  #
  specimen <- RODBC::sqlQuery(goa.db, t.sequel.command)
  #browser()
  names(specimen) <- casefold(names(specimen))
  #
  #
  #  Assign the retrieved specimen data to an object that exists outside the frame
  #  of the function, (as well as within the function's frame), I don't know why.
  #
  assign("specimen", specimen, pos = 1, immediate = T)
  #
  #
  #--------------------------------------
  #
  #
  #  Go into Oracle and grab size composition data
  #     These are not what I (Munro) think of as size composition data.
  #     In fact, these are not data at all.  Not observations.
  #     These are estimates.
  #     These appear to be population estimates for each size bin, in
  #     each survey, over the entire survey area without regard to
  #     sampling stratum.
  #
  sizecomp.total <- RODBC::sqlQuery(goa.db,paste("select * from ", survey, ".sizecomp_total where species_code = ", t.species, sep = ""))
  names(sizecomp.total) <- casefold(names(sizecomp.total))
  #
  #
  #  Assign the retrieved number at size estimates to an object that exists outside the frame
  #  of the function, (as well as within the function's frame), I don't know why.
  #
  assign("sizecomp.total", sizecomp.total, pos = 1, immediate = T)
  #browser()
  #
  #
  # convert from mm to cm
  #
  specimen$length <- specimen$length / 10
  sizecomp.total$length <- sizecomp.total$length / 10
  #
  #
  #--------------------------------------
  #
  #
  #  Set up a sex table for . . . I'm not sure why yet. (Teresa added this piece)
  #
  sex.table <- data.frame(sex.code = c(1,2,3,4), sex.name = c("males","females","unsexed","total"))
  #
  #
  sizecomp <- sizecomp.total
  #browser()
  #
  #
  #--------------------------------------
  #
  #
  #  Go through a series of nested loops to get down to each combination of
  #     species (in this case, only cod), year, and sex
  #
  #  Before looping, set up a data frame to build on at each complete loop
  #
  all.out <- data.frame(matrix(ncol = 8))
  names(all.out) <- c("SURVEY", "SURVEY_YEAR","SPECIES_CODE", "SEX", "AGE", "AGEPOP","MEAN_LENGTH", "STANDARD_DEVIATION")
  #
  #
  #  Now begin the looping itself:
  #
  # Outermost loop: species by species, (but, in this case, just cod).
  #
  for(sp in sort(unique(specimen$species_code))) {
    cat(paste("species", sp, "\n"))
    sp.specimen <- specimen[specimen$species_code == sp,  ]
    sp.sizecomp <- sizecomp[sizecomp$species_code == sp,  ]
    #
    #
    # Second loop from outside: year by year
    #
    for(yr in sort(unique(sp.specimen$year))) {
      cat(paste("Year", yr, "\n"))
      yr.specimen <- sp.specimen[sp.specimen$year == yr,  ]
      yr.sizecomp <- sp.sizecomp[sp.sizecomp$year == yr,  ]
      year.sexes.1 <- apply(yr.sizecomp[, c("males", "females", "unsexed", "total")], 2, sum)
      year.sexes.2 <- names(year.sexes.1[year.sexes.1 > 0])
      year.sexes.3 <- sex.table$sex.code[match(casefold(sex.table$sex.name), year.sexes.2)]
      #browser()
      #
      #
      # Third loop from outside: sex by sex
      #
      for(sx in sort(year.sexes.3)) {
        cat(paste("Sex", sx, "\n"))
        sex.name <- casefold(sex.table$sex.name[sx])
        sx.sizecomp <- yr.sizecomp[, c("length", sex.name)]
        sx.sizecomp <- sx.sizecomp[sx.sizecomp[, 2] > 0,  ]
        # If there is no sizecomp data, we are wasting our time
        if(length(sx.sizecomp) == 0) {
          cat(paste("No sizecomp data for sex", sx, "\n"))
          next
        }
        # Get the specimen data, if sex = 4, then use all the specimen data
        sx.specimen <- yr.specimen[yr.specimen$sex == sx,  ]
        if(length(sx.specimen$sex) == 0 && sx != 4) {
          cat(paste("No specimen data for sex", sx, "\n"))
          next
        }
        if(sx == 4)
        {
          sx.specimen <- yr.specimen
        }
        # Make a list of all possible lengths
        lenlist <- seq(from = min(sx.sizecomp$length), to = max(sx.sizecomp$length), by = 10)
        # Get number of ages by length and age
        age.numbers <- tapply(sx.specimen$age, list(sx.specimen$length, sx.specimen$age), length)
        age.numbers[is.na(age.numbers)] <- 0
        # Turn these into fractions
        age.fractions <- age.numbers/apply(age.numbers, 1, sum)
        # Find lengths from age data where there is no sizecomp data
        no.lengths <- unique(sort(sx.specimen$length))[is.na(match(unique(sort(sx.specimen$length)), sx.sizecomp$length))]
        # Find lengths from sizecomp data where there is no age data.
        no.ages <- sx.sizecomp$length[is.na(match(sx.sizecomp$length, unique(sx.specimen$length)))]
        if(length(no.ages) == 0)
          no.ages <- sx.sizecomp$length
        no.age.sizecomp <- sx.sizecomp[match(no.ages, sx.sizecomp$length),  ]
        # Nothing else we can do when there are age data with no sizecomp data, so get rid of these records
        age.fractions <- age.fractions[is.na(match(as.numeric(dimnames(age.fractions)[[1]]), no.lengths)),  ]
        # Estimate numbers by age and length
        pop.age.estimate <- age.fractions * sx.sizecomp[match(as.numeric(dimnames(age.fractions)[[1]]), sx.sizecomp$length, nomatch = 0, incomparables = no.lengths), 2]
        # If we have sizecomp with no age, roll all these into an age of -9
        if(length(no.age.sizecomp$length) > 0) {
          all.lengths <- sort(c(as.numeric(dimnames(pop.age.estimate)[[1]]), no.age.sizecomp$length))
          all.ages <- c(dimnames(pop.age.estimate)[[2]], "-9")
          new.matrix <- matrix(nrow = length(all.lengths), ncol = length(all.ages), dimnames = list(all.lengths, all.ages))
          new.matrix[dimnames(pop.age.estimate)[[1]], dimnames(pop.age.estimate)[[2]]] <- pop.age.estimate
          new.matrix[match(no.age.sizecomp$length, dimnames(new.matrix)[[1]]), "-9"] <- no.age.sizecomp[, 2]
          new.matrix[is.na(new.matrix)] <- 0
          pop.age.estimate <- new.matrix
          #browser()
        }
        # Now sum up the numbers at age for all lengths
        age.estimate <- apply(pop.age.estimate, 2, sum)
        # Calculate mean lengths and standard deviation for results
        mean.lengths <- apply(pop.age.estimate * as.numeric(dimnames(pop.age.estimate)[[1]]), 2, sum)/age.estimate
        stds <- sqrt((apply(as.numeric(dimnames(pop.age.estimate)[[1]])^2 * pop.age.estimate, 2, sum) - age.estimate * mean.lengths^2)/age.estimate)
        # Get the data ready for export and send it to the Oracle table.
        out.data <- data.frame(survey, as.numeric(yr), sp, sx, as.numeric(names(age.estimate)), age.estimate, mean.lengths, stds)
        names(out.data) <- c("SURVEY", "SURVEY_YEAR", "SPECIES_CODE", "SEX", "AGE", "AGEPOP", "MEAN_LENGTH", "STANDARD_DEVIATION")
        out.data <- out.data[out.data$AGEPOP > 0,  ]
        print(out.data)
        all.out <- rbind(all.out, out.data)
        # write.oracle.data(data = out.data, table = "agecomp_cod_ge27", user.name = user.name, password = password)
      }
    }
  }
  close(goa.db)
  all.out <- all.out[all.out$AGE != -9, ]
  all.out <- all.out[!is.na(all.out$SURVEY),]
  all.out
}



# Compare to age comps in RACEBASE ----------------------------------------
source("C:/Users/margaret.siple/Work/oraclecreds.R")

cod <- get_age_comps(survey = "GOA",
                     t.username = siple.oracle,
                     t.password = siple.oracle.pass,
                     t.species = 21720,
                     t.cut = 270,
                     t.which.bin = "larger")
library(dplyr)
cod_racebase <- read.csv("data/agecomp_total_racebase.csv") 
cod_racebase2  <- cod_racebase %>% 
  filter(SPECIES_CODE == 21720) %>% 
  mutate(MEAN_LENGTH = MEAN_LENGTH/10, STANDARD_DEVIATION = STANDARD_DEVIATION/10) %>% # convert to cm for comparison
  #tidyr::complete(SURVEY,SURVEY_YEAR,SEX,AGE) %>%
  select(-SPECIES_CODE)
  

# For some reason this code doesn't always produce age 1 estimates (maybe related to the min size). Fill with NAs so the dataframes are more comparable.
cod2 <- cod %>% 
  #tidyr::complete(SURVEY,SURVEY_YEAR,SEX,AGE) %>% 
  select(-SPECIES_CODE)

compare.df <- full_join(cod_racebase2,cod2,by=c("SURVEY","SURVEY_YEAR","SEX","AGE"),suffix = c(".racebase",".mmcode"))

#diffdf::diffdf(cod2, cod_racebase2)


