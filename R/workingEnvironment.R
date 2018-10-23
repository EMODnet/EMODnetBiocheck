####  empty the working environment

rm(list=setdiff(ls(), c("lstIPT", "i", "fncols", "BODC")))

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

#### install and require dependancys


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






#create a list of the measurementtypeIDs to be taken into account when checking the occurrence table for duplicates




#-----------------------------------------------------------------------#
####                    BODC VOCAB tables                            ####
#-----------------------------------------------------------------------#

  
BODC=list()

BODC$biometrics <- c ('http://vocab.nerc.ac.uk/collection/P01/current/LSTAGE01/', 'http://vocab.nerc.ac.uk/collection/P01/current/OBSINDLX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/LGPIXEL1/', 'http://vocab.nerc.ac.uk/collection/P01/current/AGEBENTX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/CELLVOLM/', 'http://vocab.nerc.ac.uk/collection/P01/current/OBSMAXLX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/OBSMINLX/' )



BODC$effort <- c('AREABEDS', 'Q01', 'VOLWBSMP', 'LENTRACK' , 'AZDRZZ01' ,'VOLFFMXX')

BODC$instrument <- c('Q0100002')




##### Paramters

x <- read_xml("http://vocab.nerc.ac.uk/collection/P01/current/")
terms <- xml_find_all(x, ".//skos:Concept")
l <- lapply(terms, function(term) {
  element <- as_list(term)
  return(list(
    identifier = unlist(element$identifier),
    definition = unlist(element$definition),
    preflabel = unlist(element$prefLabel),
    altLabel = unlist(element$altLabel),
    uri = xml_attr(term, "about"))
  )
})
P01s <- bind_rows(l)

x <- read_xml("http://vocab.nerc.ac.uk/collection/Q01/current/")
terms <- xml_find_all(x, ".//skos:Concept")
l <- lapply(terms, function(term) {
  element <- as_list(term)
  altLabel <- if (length(element$altLabel) > 0) unlist(element$altLabel) else NA
  return(list(
    identifier = unlist(element$identifier),
    definition = unlist(element$definition),
    preflabel = unlist(element$prefLabel),
    altLabel = altLabel,
    uri = xml_attr(term, "about"))
  )
})
Q01s <- bind_rows(l)
BODC$parameters <- rbind(Q01s,P01s )


##### Units

x <- read_xml("http://vocab.nerc.ac.uk/collection/P06/current/")
terms <- xml_find_all(x, ".//skos:Concept")
l <- lapply(terms, function(term) {
  element <- as_list(term)
  return(list(
    identifier = unlist(element$identifier),
    preflabel = unlist(element$prefLabel),
    altLabel = unlist(element$altLabel),
    uri = xml_attr(term, "about"))
  )
})
BODC$units <- bind_rows(l)



#### Values

x <- read_xml("http://vocab.nerc.ac.uk/collection/L22/current/")
terms <- xml_find_all(x, ".//skos:Concept")
l <- lapply(terms, function(term) {
  element <- as_list(term)
  definition <- if (length(element$definition) > 0) unlist(element$definition) else NA
  return(list(
    preflabel = unlist(element$prefLabel),
    definition = definition,
    uri = xml_attr(term, "about"))
  )
})
L22s <- bind_rows(l)

x <- read_xml("http://vocab.nerc.ac.uk/collection/L05/current/")
terms <- xml_find_all(x, ".//skos:Concept")
l <- lapply(terms, function(term) {
  element <- as_list(term)
  definition <- if (length(element$definition) > 0) unlist(element$definition) else NA
  return(list(
    preflabel = unlist(element$prefLabel),
    definition = definition,
    uri = xml_attr(term, "about"))
  )
})
L05s <- bind_rows(l)


x <- read_xml("http://vocab.nerc.ac.uk/collection/S10/current/")
terms <- xml_find_all(x, ".//skos:Concept")
l <- lapply(terms, function(term) {
  element <- as_list(term)
  definition <- if (length(element$definition) > 0) unlist(element$definition) else NA
  return(list(
    preflabel = unlist(element$prefLabel),
    definition = definition,
    uri = xml_attr(term, "about"))
  )
})
S10s <- bind_rows(l)

x <- read_xml("http://vocab.nerc.ac.uk/collection/S11/current/")
terms <- xml_find_all(x, ".//skos:Concept")
l <- lapply(terms, function(term) {
  element <- as_list(term)
  definition <- if (length(element$definition) > 0) unlist(element$definition) else NA
  return(list(
    preflabel = unlist(element$prefLabel),
    definition = definition,
    uri = xml_attr(term, "about"))
  )
})
S11s <- bind_rows(l)

x <- read_xml("http://vocab.nerc.ac.uk/collection/M20/current/")
terms <- xml_find_all(x, ".//skos:Concept")
l <- lapply(terms, function(term) {
  element <- as_list(term)
  definition <- if (length(element$definition) > 0) unlist(element$definition) else NA
  return(list(
    preflabel = unlist(element$prefLabel),
    definition = definition,
    uri = xml_attr(term, "about"))
  )
})
M20s <- bind_rows(l)

x <- read_xml("http://dd.eionet.europa.eu/vocabulary/biodiversity/eunishabitats/rdf")
terms <- xml_find_all(x, ".//skos:Concept")
l <- lapply(terms, function(term) {
  element <- as_list(term)
  definition <- if (length(element$definition) > 0) unlist(element$definition) else NA
  return(list(
    preflabel = unlist(element$prefLabel),
    definition = definition,
    uri = xml_attr(term, "about"))
  )
})
EUNIS <- bind_rows(l) %>% mutate (uri = paste(uri,"/", sep ="")) 


BODC$values <- rbind(L22s, L05s, S11s, S10s, M20s, EUNIS)

rm(L22s, L05s, S11s, S10s, M20s, x,l, terms, EUNIS, P01s, Q01s)

