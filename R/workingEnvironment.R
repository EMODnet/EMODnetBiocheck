
#rm(list=setdiff(ls(), c("lstIPT", "i", "fncols", "BODC")))

library("RCurl")
library("finch")
library("dplyr")
library("data.table")
library("ggplot2")
library("knitr")
library("leaflet")
library("xml2")
require("obistools")
library("rmarkdown")



BODC = list()
BODC$units  <- read.csv("BODCdata/units.csv", stringsAsFactors = FALSE )
BODC$values <-read.csv("BODCdata/values.csv", stringsAsFactors = FALSE )
BODC$parameters <-read.csv("BODCdata/parameters.csv", stringsAsFactors = FALSE )




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



fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  if(length(add)!=0) data[add] <- as.character(NA)
  data
}


BODC$nomofvalues <- c('http://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01/', 'http://vocab.nerc.ac.uk/collection/P01/current/SSAMPC01/')

BODC$biometrics <- c ('http://vocab.nerc.ac.uk/collection/P01/current/LSTAGE01/', 'http://vocab.nerc.ac.uk/collection/P01/current/OBSINDLX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/LGPIXEL1/', 'http://vocab.nerc.ac.uk/collection/P01/current/AGEBENTX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/CELLVOLM/', 'http://vocab.nerc.ac.uk/collection/P01/current/OBSMAXLX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/OBSMINLX/', 'http://vocab.nerc.ac.uk/collection/P01/current/ENTSEX01/' ) #create a list of the measurementtypeIDs to be taken into account when checking the occurrence table for duplicates

BODC$effort <- c('AREABEDS', 'Q01', 'VOLWBSMP', 'LENTRACK' , 'AZDRZZ01' ,'VOLFFMXX')

BODC$instrument <- c('Q0100002')
