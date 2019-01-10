


library("dplyr")
library("xml2")

library("finch")

link <- "http://ipt.iobis.org/training/archive?r=biofun2009_solution"
out <- dwca_read(link, read = TRUE)
eMoF <-out$data[["extendedmeasurementorfact.txt"]]  


typeids <- unique(eMoF$measurementTypeID) 


matched <- bind_rows(lapply(typeids, getbodc))



getbodc = function (X) {
x <- read_xml(X)
concept <- xml_find_all(x, ".//skos:Concept")
if (grepl("P01", X, fixed = TRUE)){
  l <- lapply(concept, function(term) {
    element <- as_list(term)
    return(list(
      identifier = unlist(element$identifier),
      definition = unlist(element$definition),
      preflabel = unlist(element$prefLabel),
      deprecated = unlist(element$deprecated),
      altLabel = unlist(element$altLabel),
      uri = xml_attr(term, "about"))
      
    ) })} else if (grepl("Q01", X, fixed = TRUE)){
  l <- lapply(concept, function(term) {
    element <- as_list(term)
    altLabel <- if (length(element$altLabel) > 0) unlist(element$altLabel) else NA
    return(list(
      identifier = unlist(element$identifier),
      definition = unlist(element$definition),
      deprecated = unlist(element$deprecated),
      preflabel = unlist(element$prefLabel),
      altLabel = altLabel,
      uri = xml_attr(term, "about"))
    ) })} 

P01 <- bind_rows(l)

related <- xml_find_all(concept, ".//skos:related")
related <- bind_rows(lapply(xml_attrs(related), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
if (nrow(related)>0){
  related <- related %>% mutate(uri = P01$uri, link = "related")  }
broader <- xml_find_all(concept, ".//skos:broader")
broader <- bind_rows(lapply(xml_attrs(broader), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
if (nrow(broader)>0){
  broader <- broader %>% mutate(uri = P01$uri, link = "broader")  }
sameAs <- xml_find_all(concept, ".//owl:sameAs")
sameAs <- bind_rows(lapply(xml_attrs(sameAs), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
if (nrow(sameAs)>0){
  sameAs <- sameAs %>% mutate(uri = P01$uri, link = "sameAs")  }

links <-bind_rows(related,broader,sameAs)

if (nrow(links)>0){
P01 <- P01 %>% inner_join(links, by ="uri") } else {P01}

}










