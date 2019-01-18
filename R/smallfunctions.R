#' fncols
#'
#' checks if a collumn exists in the dataframe and adds one if not.
#' @param data mandatory parameter, name dataframe
#' @param cname mandatory parameter, collumn name
#' @import RCurl finch dplyr ggplot2
#' @import knitr leaflet xml2 obistools rmarkdown
#' @importFrom data.table dcast
#' @importFrom stringr str_sub
#' @export


fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  if(length(add)!=0) data[add] <- as.character(NA)
  data
}





substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#' get the rightmost characters from a string
#' @export

richtfrom <- function(x, y, n = 0) {
  substrRight(x, nchar(x)-stringr::str_locate(x, y)[1] - n)
}


#' remove empty collumns and make all collumns into characters
#' @export

cleandataframe <- function (x, vector = TRUE) {
  
  x[x =='NA' | x =='' | x ==' '] <- NA
  x <- x[,colSums(is.na(x))<nrow(x)]
  
  if (vector == TRUE ){
    x <- data.frame(lapply(x, as.character), stringsAsFactors=FALSE)
  }
  return (x)
}

#' add "/" to value if missing
#' @export

cleanemof <- function (x) {
  x <- x %>% fncols(c("occurrenceID", "measurementTypeID","measurementValueID", "measurementValue", "measurementUnitID", "eventID", "measurementUnit")) %>%
    mutate (measurementTypeID =  if_else(str_sub(measurementTypeID, -1, -1)=='/',measurementTypeID, paste(measurementTypeID, "/",  sep = '')) ,
            measurementValueID =  if_else(str_sub(measurementValueID, -1, -1)=='/',measurementValueID, paste(measurementValueID, "/",  sep = '')),
            measurementUnitID =  if_else(str_sub(measurementUnitID, -1, -1)=='/',measurementUnitID, paste(measurementUnitID, "/",  sep = '')))
  
  return(x)
}


#' convert IPT names to vector
#'
#' Function used by loopcheckIPTdataset to convert the imput to a vector.
#' @export
#' @param x mandatory parameter, input



NamesToVector = function (x) {
  if (is.null(names(x)) == TRUE) {
    for (j in x) {
      if ((grepl("resource?", j)==TRUE) | (grepl("archive?", j)==TRUE)) {
        if (exists("lstnames")==TRUE) {
          lstnames <- c(lstnames, substr(j, regexpr('=', j)+1, nchar(j)))
        } else {
          lstnames <- c(substr(j, regexpr('=', j)+1, nchar(j)))
        }
      } else if (grepl(".zip", j)==TRUE) {
        if (exists("lstnames")==TRUE) {
          lstnames <- c(lstnames, substr(j, if ((grepl("/", j)==FALSE) | (grepl("/", j)==FALSE)) {1}
                                         else { if (grepl("/", j)==TRUE) { rev(gregexpr("\\/", j)[[1]])[1] +1   }
                                           else { nchar(j) -12  }
                                         }, regexpr('.zip', j)-1))
        } else {
          lstnames <- c(substr(j, if ((grepl("/", j)==FALSE) | (grepl("/", j)==FALSE)) {1}
                               else { if (grepl("/", j)==TRUE) { rev(gregexpr("\\/", j)[[1]])[1] +1   }
                                 else { nchar(j) -12  }
                               }, regexpr('.zip', j)-1))
        } } }
    x <- structure(x, names=lstnames)
    rm(lstnames)
    return(x)
  }
}


