library(dplyr)
library(xml2)

##### Parameters
x <- read_xml("http://vocab.nerc.ac.uk/collection/P01/current/")
termsp01 <- xml_find_all(x, ".//skos:Concept")
l <- lapply(termsp01, function(term) {
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

# Manually adding the eunis habitat collection as a vocab term <- requested by MBA and approved by OBIS-VLIZ
eunis_col <- data.frame("eunishabitats","EUNIS habitats","Classification of habitat types according to the EUNIS Biodiversity database", "http://dd.eionet.europa.eu/vocabulary/biodiversity/eunishabitats/")
names(eunis_col)<-c("identifier", "preflabel","definition", "uri")

parameters <- rbind(Q01s,P01s) %>% 
              bind_rows(eunis_col)



P01sunits <-NULL

for (i in 1:nrow(P01s)){
  P01 <- bind_rows(lapply(xml_attrs(termsp01[i]), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  related <- xml_find_all(termsp01[i], ".//skos:related")
  related <- bind_rows(lapply(xml_attrs(related), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  related <- related %>% filter (grepl("P06", resource, fixed = TRUE))
  if (nrow(related)>0){
    links <- related %>% mutate(p01s = P01$about)
    P01sunits <- rbind(P01sunits, links)
  }
  rm(P01,related, links)
}



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
units <- bind_rows(l)

##### Link parameters and units

parameters <- parameters %>% left_join(P01sunits, by=c("uri" = "p01s")) %>%
  left_join(units %>% select (uri, standardunit = altLabel), 
            by=c("resource" = "uri")) %>% rename (standardUnitID = resource)


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


x <- read_xml("http://vocab.nerc.ac.uk/collection/C17/current/")
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
C17s <- bind_rows(l)


x <- read_xml("http://vocab.nerc.ac.uk/collection/F02/current/")
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
F02s <- bind_rows(l)



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

values <- rbind(L22s, L05s, F02s, C17s, S11s, S10s, M20s, EUNIS)




# Update the data files # Make sure that the tables look  
BODCparameters <- parameters
BODCunits <- units
BODCvalues <- values

# Make sure that the tables look correct before overwriting the BODC following files
usethis::use_data(BODCunits, overwrite = TRUE)
usethis::use_data(BODCvalues, overwrite = TRUE)
usethis::use_data(BODCparameters, overwrite = TRUE)


# Deprecated
# dir.create(file.path(getwd(), "BODCdata"))

# To-do: locate this file under the BODCdata (or the data folder actually)
# write.csv(parameters, 'parameters.csv', row.names = FALSE, na = "" ) 
# write.csv(units, 'units.csv', row.names = FALSE, na = "" ) 
# write.csv(values, 'values.csv', row.names = FALSE, na = "" ) 
