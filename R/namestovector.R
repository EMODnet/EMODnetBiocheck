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
