#' get skos xml data
#'
#' This function allows to get the information form different concepts stored in skoss xml. It takes a single concept, multiple concepts or a shema. The output is a list of 2 data frames; one with the lables and definitions. The other with the relations with other concepts.
#' @param vocids mandatory parameter, the XML link(s)  you want to read the read
#' @import dplyr xml2
#' @export
#' @examples
#' getskossxmldata("http://vocab.nerc.ac.uk/collection/P06/current/UMCU/")
#' getskossxmldata(c("http://vocab.nerc.ac.uk/collection/P06/current/UMCU/","http://vocab.nerc.ac.uk/collection/P06/current/ULAA/")
#' getskossxmldata("http://vocab.nerc.ac.uk/collection/S10/current/")
#' getskossxmldata("http://dd.eionet.europa.eu/vocabulary/biodiversity/eunishabitats/rdf")


getskossxmldata <- function (vocids) {
  for (i in vocids) {
    skossxml  <- readskossxml(i)
    terminfo <- skossxmlinfo(skossxml)
    termrelation <- skossxmlrelations(skossxml)
    
    if (exists("terminfos")) {terminfos <- bind_rows(terminfos, terminfo)} else {  terminfos <- terminfo} 
    if (exists("termrelations")) {termrelations <- bind_rows(termrelations, termrelation)} else {  termrelations <- termrelation} 
  }
  
  skossxmldata <-list()
  skossxmldata$termrelations <- termrelations
  skossxmldata$terminfos <- terminfos
  
  return(skossxmldata)
  
}


readskossxml <- function(X) {
  x <- xml2::read_xml(X)
  skossxml <- xml2::xml_find_all(x, ".//skos:Concept")
  
  return(skossxml)
}


skossxmlinfo <- function (skossxml) {
  
  l <- lapply(skossxml, function(term) {
    element <- xml2::as_list(term)
    return(list(
      identifier = as.character(if (length(unlist(element$identifier))>0) {unlist(element$identifier)} else {"NA"}),
      prefLabel  = as.character(if (length(unlist(element$prefLabel ))>0) {unlist(element$prefLabel )} else {"NA"}),
      altLabel  = as.character(if (length(unlist(element$altLabel ))>0) {unlist(element$altLabel )} else {"NA"}),
      definition = as.character(if (length(unlist(element$definition))>0) {unlist(element$definition)} else {"NA"}),
      deprecated = as.character(if (length(unlist(element$deprecated))>0) {unlist(element$deprecated)} else {"NA"}),
      uri = xml2::xml_attr(term, "about")))})
  
  terminfo <- bind_rows(l)
  return (terminfo)
}


skossxmlrelations <- function (skossxml) {
  
  for (x in 1:length(skossxml)) {
    for (j in skossconcepts) {
      options(stringsAsFactors = FALSE)
      templ <- xml2::xml_find_all(skossxml[x],paste0(".//",j))
      tempid <- xml2::xml_attr(skossxml[x], "about")
      
      if(length(templ)>0){
        temp <- dplyr::bind_rows(lapply(xml2::xml_attrs(templ), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
        temp <- cbind(term = tempid ,temp,relation =EMODnetBiocheck::richtfrom(j,":",0))
        
        if (exists("termr")) {termr <- bind_rows(termr,temp ) } else {termr <- temp }
        rm(temp)
      }
    }
    if (exists("termr")){if(exists("termr2")) {termr2 <- bind_rows(termr2,termr) } else {termr2 <- termr}}
    if (exists("termr")){rm(termr)}
    
  }
  
  if (exists("termr2")){
    termrelations <- termr2 } else {termrelations <- data.frame(term = tempid, resource = NA, relation = NA) }
  return(termrelations)
}
