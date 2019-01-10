#' fncols
#'
#' checks if a collumn exists in the dataframe and adds one if not.
#' @param data mandatory parameter, name dataframe
#' @param cname mandatory parameter, collumn name
#' @export


fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  if(length(add)!=0) data[add] <- as.character(NA)
  data
}