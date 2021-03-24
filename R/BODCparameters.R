#' Copy of the BODC P01 and Q01 collections, including a parameter for EUNIS.
#'
#' A dataframe containing the BODC P01 and Q01 collections, including a parameter for EUNIS. 
#' These vocabulary terms can be used as measurementTypeIDs. 
#' 
#'
#' @format A data frame with updating number of rows and 7 columns:
#' \describe{
#'   \item{identifier}{ID of the vocab term}
#'   \item{definition}{definition of the vocab term}
#'   \item{preflabel}{preferred label of the vocab term}
#'   \item{altLabel}{alternative label of the vocab term}
#'   \item{deprecated}{states if the term is deprecated}
#'   \item{uri}{uri of the vocab term}
#'   \item{standardUnitID}{BODC P06 vocabulary term URI associated to the vocab term}
#'   \item{standardunit}{BODC P06 vocabulary term preferred label associated to the vocab term}
#' }
#' @source \url{http://vocab.nerc.ac.uk/collection/}
"BODCparameters"