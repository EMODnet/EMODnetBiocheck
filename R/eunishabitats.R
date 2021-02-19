#' Copy of the EUNIS habitats vocabulary terms.
#'
#' A dataframe containing the EUNIS habitats vocabulary terms. 
#' These vocabulary terms can be used as measurementValueIDs. Currently not in use? 
#' 
#'
#' @format A data frame with updating number of rows and 6 columns:
#' \describe{
#'   \item{identifier}{ID of the vocab term}
#'   \item{preflabel}{preferred label of the vocab term}
#'   \item{altlabel}{alternative label of the vocab term}
#'   \item{definition}{definition of the vocab term}
#'   \item{deprecated}{deprecated state of the vocab term}
#'   \item{uri}{uri of the vocab term}
#' }
#' @source \url{https://eunis.eea.europa.eu/habitats}
"eunishabitats"