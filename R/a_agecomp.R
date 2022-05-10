

a_agecomp <- function(survey = "GOA", t.username = NULL, t.password = NULL, t.species = 10110, t.year = 2003) {
  # Function: A.agecomp()
  # Location: G:\GOA\R\agecomp\.RData
  # Location of text file: \\Nmfs.local\akc-race\Users\Peter.Munro\My Documents\A.R.text\
  #
  # Purpose: Estimate population at age from age data and population-at-size estimates
  #          that reside in RACE databases
  #
  # History: Originally written by Michael Martin.  Michael's code is preserved in an R object,
  #          in the same location, called: agecomp.all.species.original.by.MMartin()
  #
  #    The code that follows is a modification of Michael's original R function.
  #
  #  Arguments:
  #    survey = "GOA" (the default; not yet generalized)
  #    t.username = oracle username, default is NULL, which
  #                 leads to a query to user
  #    t.password = oracle password, default is NULL, which
  #                 leads to a query to user
  #    t.species = NULL
  #                this is an integer value: the RACE species code for the
  #                species of interest.
  #                arrowtooth flounder is 10110
  #    t.year = NULL
  #             This argument allows us to pick just a single year of data.  We put it
  #             in to help track down an arrowtooth flounder problem in 2003 data.
  #
  #
  #
  # ==============================
  #  Required packages:
  #    RODBC
  #
  require(RODBC)
  #browser()
  # ==============================
  #
  #
  #  Setting up a channel to Oracle.
  #
  if (is.null(t.username)) {
    cat("\n")
    cat("\n")
    cat("Type your sql username, NO quotation marks\n")
    cat("\n")
    cat("\n")
    t.username <- readline()
  }
  if (is.null(t.password)) {
    cat("\n")
    cat("\n")
    cat("Type your sql password, NO quotation marks\n")
    cat("\n")
    cat("\n")
    t.password <- readline()
  }
  # browser()
  #
  #
  # Set up a channel to the database, called goa.db
  #
  goa.db <- RODBC::odbcConnect("AFSC", t.username, t.password, believeNRows = FALSE)
  # browser()
  #
  # ======================================================
  #
  #
  #
  #
  # SPECIMEN DATA:
  # ======================================================
  #
  #  Go into Oracle and grab specimen data, specifically the age data,
  #  for all the years of the specified survey
  #
  #  First: create the species code and the year pieces to be used in paste()
  #
  #     species
  #
  if (is.null(t.species)) {
    t.species.bit <- ""
  }
  if (!is.null(t.species)) {
    t.species.bit <- paste(" and species_code = ", as.character(t.species), sep = "")
  }
  #
  #     year
  #
  if (is.null(t.year)) {
    t.year.bit <- ""
  }
  if (!is.null(t.year)) {
    t.year.bit <- paste(" and floor(cruise/100) = ", as.character(t.year), sep = "")
  }
  #
  #  Second: make sure the survey argument is properly formatted:
  #
  survey <- toupper(survey)
  #
  #
  #  Third: paste together the actual Sequel command that we want to make
  #
  t.sequel.command <- paste("select floor(cruise/100) year, cruisejoin, species_code, specimenid, length length_mm, sex, age from racebase.specimen where cruisejoin in (select cruisejoin from goa.biennial_surveys where survey = '", survey, "') and age is not null", t.species.bit, t.year.bit, sep = "")
  #
  # below is the paste command that was in Michael's original call:
  #
  # t.original.command <- paste("select floor(cruise/100) year, cruisejoin, species_code, specimenid, #length, sex, age from racebase.specimen where cruisejoin in (select cruisejoin from #goa.biennial_surveys where survey = '", survey, "') and age is not null", sep = "")
  #
  # browser()
  #
  #  Do the actual query to Sequel
  #
  specimen <- RODBC::sqlQuery(goa.db, t.sequel.command)
  names(specimen) <- casefold(names(specimen))
  # browser()
  #
  #
  #  Here ends the getting of the specimen data from RACE databases
  #
  # ======================================================
  #
  #
  #
  #
  # POPULATION-AT-SIZE ESTIMATES:
  # ======================================================
  #
  #  Go into Oracle and grab population at size estimates, summed over
  #  each stratum in the survey.  (These are estimates that have already
  #  been computed elsewhere and elsewise and now exist in Oracle.)
  #
  #
  sizecomp.total <- RODBC::sqlQuery(goa.db, query = paste("select * from ", survey, ".sizecomp_total", sep = ""))
  names(sizecomp.total) <- tolower(names(sizecomp.total))
  # 	assign("sizecomp.total", sizecomp.total, env = .GlobalEnv)
  t.df <- sizecomp.total
  #
  #
  # PROBLEM: Later on, integer values get too big and exceed the limit.  To prevent
  # that, we coerce the problem elements from integer to numeric.  This is a crude fix,
  # not really the correct way to keep control of the class of objects and elements
  # of objects as they pass from Oracle to R (or back):
  # ========================================================================================
  #
  t.df$males <- as.numeric(t.df$males)
  t.df$females <- as.numeric(t.df$females)
  t.df$unsexed <- as.numeric(t.df$unsexed)
  t.df$total <- as.numeric(t.df$total)
  #
  # ========================================================================================
  #
  #  If we need to winnow down to a species and/or a year:
  #
  #     species
  #
  if (!is.null(t.species)) {
    t.df <- t.df[t.df$species_code == t.species, ]
  }
  #
  #     year
  #
  if (!is.null(t.year)) {
    t.df <- t.df[t.df$year == t.year, ]
  }
  # browser()
  t.df <- t.df[order(t.df$year, t.df$species_code, t.df$length), ]
  sizecomp.total <- t.df
  #
  #
  #  Here ends the getting of the pop-at-size estimates from RACE databases
  #
  #
  # ======================================================
  #
  #
  #
  #
  # MISSING FROM MICHAEL'S ORIGINAL (was in his R code for the cod split)
  # ===========================================================================
  #
  # convert from mm to cm
  #
 
  if(nrow(specimen)>0){specimen$length <- specimen$length / 10}
  if(nrow(specimen)==0){print(paste('specimen table has 0 rows for',t.species))}
  
  sizecomp.total$length <- sizecomp.total$length / 10
  #
  #
  #--------------------------------------
  #
  #
  #  Set up a sex table to control labeling and indexing downstream
  #    (Teresa added this part)
  #
  sex.table <- data.frame(sex.code = c(1, 2, 3, 4), 
                          sex.name = c("males", "females", "unsexed", "total"))
  #
  #
  # Get an easier-to-type name for the population_at_size dataframe:
  #
  sizecomp <- sizecomp.total
  # browser()
  #
  #  Here ends the grafted-in code that seemed to be missing
  # ============================================================================
  #
  #
  #
  #
  #
  # SETTING UP FOR OUTPUT
  # ======================================================
  #
  all.out <- data.frame(matrix(ncol = 8))
  names(all.out) <- c("SURVEY", "SURVEY_YEAR", "SPECIES_CODE", "SEX", "AGE", "AGEPOP", "MEAN_LENGTH", "STANDARD_DEVIATION")
  #
  #
  #
  #
  #
  #
  # A sequence of loops for working through species, year, and sex
  # ======================================================================================
  #
  # Loop for species:
  #
  if(nrow(specimen>0)){ #allow for species not having age comps (some don't)
  for (sp in sort(unique(specimen$species_code))) {
    cat(paste("species", sp, "\n"))
    sp.specimen <- specimen[specimen$species_code == sp, ]
    sp.sizecomp <- sizecomp[sizecomp$species_code == sp, ]
    #
    #
    # Loop for year:
    #
    for (yr in sort(unique(sp.specimen$year))) {
      cat(paste("Year", yr, "\n"))
      yr.specimen <- sp.specimen[sp.specimen$year == yr, ]
      yr.sizecomp <- sp.sizecomp[sp.sizecomp$year == yr, ]
      #
      # In the apply() below, we encountered an "Integer overflow" error because the function
      # was summing over such huge numbers.  We have coerced the numbers at size to be in
      # elements that have a numeric class (previously had an integer class).
      year.sexes.1 <- apply(yr.sizecomp[, c("males", "females", "unsexed")], 2, sum)
      year.sexes.2 <- names(year.sexes.1[year.sexes.1 > 0])
      year.sexes.3 <- sex.table$sex.code[match(casefold(sex.table$sex.name), year.sexes.2)]
      #
      #
      # Loop for sex:
      #
      for (sx in sort(year.sexes.3)) {
        cat(paste("Sex", sx, "\n"))
        sex.name <- casefold(sex.table$sex.name[sx])
        sx.sizecomp <- yr.sizecomp[, c("length", sex.name)]
        sx.sizecomp <- sx.sizecomp[sx.sizecomp[, 2] > 0, ]
        # If there is no sizecomp data, we are wasting our time
        if (length(sx.sizecomp) == 0) {
          cat(paste("No sizecomp data for sex", sx, "\n"))
          next
        }
        # Get the specimen data, if sex = 3, then use all the specimen data
        sx.specimen <- yr.specimen[yr.specimen$sex == sx, ]
        if (length(sx.specimen$sex) == 0) {
          cat(paste("No specimen data for sex", sx, "\n"))
          next
        }
        if (sx == 3) {
          sx.specimen <- yr.specimen
        }
        # Make a list of all possible lengths
        lenlist <- seq(from = min(sx.sizecomp$length), to = max(sx.sizecomp$length), by = 10)
        # Get number of ages by length and age
        age.numbers <- tapply(sx.specimen$age, list(sx.specimen$length, sx.specimen$age), length)
        age.numbers[is.na(age.numbers)] <- 0
        # Turn these into fractions
        age.fractions <- age.numbers / apply(age.numbers, 1, sum)
        # Find lengths from age data where there is no sizecomp data
        no.lengths <- unique(sort(sx.specimen$length))[is.na(match(unique(sort(sx.specimen$length)), sx.sizecomp$length))]
        # Find lengths from sizecomp data where there is no age data.
        no.ages <- sx.sizecomp$length[is.na(match(sx.sizecomp$length, unique(sx.specimen$length)))]
        if (length(no.ages) == 0) {
          no.ages <- sx.sizecomp$length
        }
        no.age.sizecomp <- sx.sizecomp[match(no.ages, sx.sizecomp$length), ]
        # Nothing else we can do when there are age data with no sizecomp data, so get rid of these records
        age.fractions <- age.fractions[is.na(match(as.numeric(dimnames(age.fractions)[[1]]), no.lengths)), ]
        # Estimate numbers by age and length
        pop.age.estimate <- age.fractions * sx.sizecomp[match(as.numeric(dimnames(age.fractions)[[1]]), sx.sizecomp$length, nomatch = 0, incomparables = no.lengths), 2]
        # If we have sizecomp with no age, roll all these into an age of -9
        if (length(no.age.sizecomp$length) > 0) {
          all.lengths <- sort(c(as.numeric(dimnames(pop.age.estimate)[[1]]), no.age.sizecomp$length))
          all.ages <- c(dimnames(pop.age.estimate)[[2]], "-9")
          new.matrix <- matrix(nrow = length(all.lengths), ncol = length(all.ages), dimnames = list(all.lengths, all.ages))
          new.matrix[dimnames(pop.age.estimate)[[1]], dimnames(pop.age.estimate)[[2]]] <- pop.age.estimate
          new.matrix[match(no.age.sizecomp$length, dimnames(new.matrix)[[1]]), "-9"] <- no.age.sizecomp[, 2]
          new.matrix[is.na(new.matrix)] <- 0
          pop.age.estimate <- new.matrix
        }
        # Now sum up the numbers at age for all lengths
        age.estimate <- apply(pop.age.estimate, 2, sum)
        # Calculate mean lengths and standard deviation for results
        mean.lengths <- apply(pop.age.estimate * as.numeric(dimnames(pop.age.estimate)[[1]]), 2, sum) / age.estimate
        stds <- sqrt((apply(as.numeric(dimnames(pop.age.estimate)[[1]])^2 * pop.age.estimate, 2, sum) - age.estimate * mean.lengths^2) / age.estimate)
        # Get the data ready for export and send it to the Oracle table.
        out.data <- data.frame(survey, as.numeric(yr), sp, sx, as.numeric(names(age.estimate)), age.estimate, mean.lengths, stds)
        names(out.data) <- names(all.out)
        out.data <- out.data[out.data$AGEPOP > 0, ]
        all.out <- rbind(all.out, out.data)
        # browser()
      }
    }
  }
    } #/if statement about nrow(specimen)
  all.out <- all.out[!is.na(all.out$SURVEY), ]
  # MCS: comment this out for now. The below script writes the table to RACEBASE... let's not do that at the moment!!
  # browser()
  # varTypes <- c(SURVEY = "VARCHAR2(3BYTE)", SURVEY_YEAR = "NUMBER", SPECIES_CODE = "NUMBER", SEX = "NUMBER", AGE = "NUMBER", AGEPOP = "NUMBER", MEAN_LENGTH = "NUMBER", STANDARD_DEVIATION = "NUMBER")
  # sqlQuery(channel = channel, query = "drop table AGECOMP_TOTAL_2")
  # sqlQuery(channel = channel, "commit")
  # sqlSave(channel = channel, dat = all.out, tablename = "AGECOMP_TOTAL_2", rownames = F, safer = FALSE, varTypes = varTypes)
  # sqlQuery(channel = channel, query = "create table agecomp_total as select * from agecomp_total_2")
  # sqlQuery(channel = channel, query = "drop table agecomp_total_2")
  # sqlQuery(channel = channel, query = "grant select on agecomp_total to public")
  # close(goa.db)
  return(all.out)
}


#Compare a_agecomp to racebase table for ATF -----------------------------------------------------

# Oracle credentials
source("C:/Users/margaret.siple/Work/oraclecreds.R")
atf_aa <- a_agecomp(
  survey = "GOA",
  t.username = siple.oracle,
  t.password = siple.oracle.pass,
  t.species = 10110, t.year = 2003 #atf
)

atf_racebase <- read.csv("data/agecomp_total_racebase.csv")
atf_racebase2 <- atf_racebase %>%
  filter(SPECIES_CODE == 10110 & SURVEY_YEAR==2003) %>%
  mutate(MEAN_LENGTH = MEAN_LENGTH / 10, STANDARD_DEVIATION = STANDARD_DEVIATION / 10) %>% # convert to cm for comparison
  mutate_at(.vars = c("AGE","SEX","SURVEY_YEAR"),as.numeric)

nrow(atf_aa)
nrow(atf_racebase2)
atf_aa$MEAN_LENGTH == atf_racebase2$MEAN_LENGTH


compare.df <- full_join(atf_racebase2, atf_aa, 
                        by = c("SURVEY", "SURVEY_YEAR", "SEX", "AGE"), 
                        suffix = c(".racebase", ".mmcode")) %>%
  select(SURVEY, SURVEY_YEAR,SEX,AGE,
         AGEPOP.mmcode,AGEPOP.racebase,
         MEAN_LENGTH.mmcode, MEAN_LENGTH.racebase,
         STANDARD_DEVIATION.mmcode, STANDARD_DEVIATION.racebase)

x <- diffdf::diffdf(atf_racebase2,atf_aa)
print(x, as_string = TRUE)


# Compare a_agecomp to racebase table for other species/yrs ---------------
racebase_allcomps <- read.csv("data/agecomp_total_racebase.csv")
spps_in_racebase <- unique(racebase_allcomps$SPECIES_CODE)
spps_in_safe <- read.csv("data/siple_safe_species.csv") %>%
  filter(GOA == 1 & !is.na(species_code)) %>%
  arrange(species_code)

compare_tabs <- function(species_code, year_compare, region = "GOA") {
  agecomp_mmartin <- a_agecomp(
    survey = region,
    t.username = siple.oracle,
    t.password = siple.oracle.pass,
    t.species = species_code, t.year = year_compare # atf
  )

  agecomp_racebase <- racebase_allcomps %>%
    filter(SPECIES_CODE == species_code & SURVEY_YEAR == year_compare) %>%
    mutate(MEAN_LENGTH = MEAN_LENGTH / 10, STANDARD_DEVIATION = STANDARD_DEVIATION / 10) %>% # convert to cm for comparison
    mutate_at(.vars = c("AGE", "SEX", "SURVEY_YEAR"), as.numeric)

  
  if (nrow(agecomp_mmartin) == 0 | nrow(agecomp_racebase) == 0) {
    cat("zero rows in one or both age tables")
    matchtest <- 3
  }else {
      x <- diffdf::diffdf(agecomp_racebase, agecomp_mmartin)
      y <- print(x, as_string = TRUE)[1]
      matchtest <- ifelse(y == "No issues were found!", 1, 0)
    }
  return(matchtest)
}

#this one should always work:
compare_tabs(species_code = 10110,year_compare = 2003,region = "GOA")

#this one is for testing:
compare_tabs(species_code = 10130,year_compare = 2017,region = "GOA")


# Make testing dataframe
spps_to_compare <- as.numeric(spps_in_racebase)
yrs_to_compare <- as.numeric(unique(racebase_allcomps$SURVEY_YEAR))
full_comparison <- expand.grid(spps_to_compare, yrs_to_compare)
colnames(full_comparison) <- c('spcode','yr')
full_comparison$does_it_match <- NA
full_comparison$sp_in_mmartin_total <- 0
full_comparison$sp_in_racebase_total <- 0


# Loop through years and species and compare the age comps ----------------

for(i in 1:nrow(full_comparison)){
  a <- a_agecomp(survey = region,
                 t.username = siple.oracle,
                 t.password = siple.oracle.pass,
                 t.species = full_comparison$spcode[i], 
                 t.year = full_comparison$yr[i])
  b <- racebase_allcomps %>%
    filter(SPECIES_CODE == full_comparison$spcode[i] & 
             SURVEY_YEAR == full_comparison$yr[i]) %>%
    mutate(MEAN_LENGTH = MEAN_LENGTH / 10, 
           STANDARD_DEVIATION = STANDARD_DEVIATION / 10) %>% # convert to cm
    mutate_at(.vars = c("AGE", "SEX", "SURVEY_YEAR"), as.numeric)
  if(nrow(a)>0){
    full_comparison$sp_in_mmartin_total[i] <- 1
  }
  if(nrow(b)>0){
    full_comparison$sp_in_racebase_total[i] <- 1
  }
  if(nrow(a)>0 & nrow(b)>0){
    full_comparison$does_it_match[i] <- compare_tabs(
      species_code = full_comparison$spcode[i],
      year_compare = full_comparison$yr[i],region = "GOA")
    }
  }

write.csv(full_comparison, file = "outputs/a_agecomp_GOA_AGECOMP_comparison.csv",row.names = FALSE)


# Which species and years have tables for both but are mismatched? --------
full_comparison %>% 
  filter(sp_in_mmartin_total==1 & sp_in_racebase_total==1 & does_it_match==0)

full_comparison %>%
  filter(spcode == 30051)

# Look at specific cases
check_spp <- 30051
  check_year <- 2013
  
checkspps_mm <- a_agecomp(
  survey = "GOA",
  t.username = siple.oracle,
  t.password = siple.oracle.pass,
  t.species = check_spp, t.year = check_year
)
head(checkspps_mm)

checkspps_racebase <- racebase_allcomps %>%
  filter(SPECIES_CODE == check_spp & 
           SURVEY_YEAR == check_year) %>%
  mutate(MEAN_LENGTH = MEAN_LENGTH / 10, 
         STANDARD_DEVIATION = STANDARD_DEVIATION / 10) %>% # convert to cm
  mutate_at(.vars = c("AGE", "SEX", "SURVEY_YEAR"), as.numeric)
head(checkspps_racebase)

diffdf::diffdf(checkspps_racebase,checkspps_mm)
