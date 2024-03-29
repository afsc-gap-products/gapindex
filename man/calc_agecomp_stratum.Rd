% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_agecomp_stratum.R
\name{calc_agecomp_stratum}
\alias{calc_agecomp_stratum}
\title{Calculate stratum-level age composition and mean/std.dev length at age}
\usage{
calc_agecomp_stratum(racebase_tables = NULL, alk = NULL, size_comp = NULL)
}
\arguments{
\item{racebase_tables}{data object created from \code{gapindex::get_data()}}

\item{alk}{age-length key (dataframe) created from \code{gapindex::calc_alk()}}

\item{size_comp}{a dataframe of size compositions created from
\code{gapindex::calc_sizecomp_stratum()}.}
}
\value{
A named list with two elements. "age_comp" is a dataframe of numbers
at age by survey, year, stratum, species, and sex. A table of column
name descriptions is coming soon."length_at_age" is a support table
for the gapindex::calc_agecomp_region() function and should not be
used.
}
\description{
Calculate stratum-level age composition and mean/std.dev length at age
}
