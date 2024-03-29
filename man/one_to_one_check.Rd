% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/one_to_one_check.R
\name{one_to_one_check}
\alias{one_to_one_check}
\title{Checks a one to one correspondence between two fields of a dataframe and highlights records where it is not found.}
\usage{
one_to_one_check(df, field_x, field_y)
}
\arguments{
\item{df}{mandatory. a dataframe that contains at least two fields that must have a one to one relationship}

\item{field_x}{mandatory. a character string containing the name of a field that wants to be compared against another field}

\item{field_y}{mandatory. a character string containing the name of a field that wants to be compared against another field}
}
\description{
This function highlights records where a one to one relationship is expected between two fields but is not found. 
Usually relevant to compare fields that should have one-to-one relationship such as scientificName and scientifciNameID
}
\examples{
\dontrun{
output <- one_to_one_check(df = Occurrence, field_x = "scientificNameID", field_y = "scientificName")
output <- one_to_one_check(df = eMoF, field_x = "measurementTypeID", field_y = "measurementType")
output <- one_to_one_check(df = eMoF, field_x = "measurementValueID", field_y = "measurementValue")
output <- one_to_one_check(df = eMoF, field_x = "measurementUnitID", field_y = "measurementUnit")
output <- one_to_one_check(df = Event, field_x = "institutionID", field_y = "institutionCode")
}
}
