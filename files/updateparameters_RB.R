library(httr) # calling API
library(jsonlite) # transforming from JSON to data tables
library(dplyr) # data manipulation
library(stringr)
library(jsonld)
library(tidyr) # unnesting df and lists

##############################################
# P02 == P06
# P02 ~= S11 -> altlabel unnest issue
# S11 = S09
# P02 = C17
#############################################


# Call API from specific BODC collection
res = GET("https://vocab.nerc.ac.uk/collection/P01/current/?_profile=nvs&_mediatype=application/ld+json",
           query = list(status = "all",
                        relationshipType = "related"))

# Transform data into a R readable list
data <- fromJSON(rawToChar(res$content))[[1]] # Sometimes doesn't work Error: lexical error: invalid char in json text.
                                              # <!DOCTYPE HTML PUBLIC "-//IETF/

# Explore and extract wanted data (vocab terms basic data)
names(data)
P01 <- data %>% select (identifier, definition, altLabel, deprecated, `@id`, prefLabel, related) %>%
              filter (!is.na(identifier)) %>%
              unnest (cols = definition) %>%
              unnest (cols = prefLabel) %>%
              select (-`@language`) %>%
              filter (prefLabel != "en") %>%
              mutate_all(na_if, "") %>%
              transmute (identifier,
                         definition = `@value`,
                         preflabel = as.character(prefLabel),
                         altlabel = altLabel,
                         deprecated,
                         uri = `@id`, 
                         related) %>%
              unnest(cols = related) %>%
              filter (grepl("P06", related))

P01_all <- P01 %>% mutate (altlabel = as.character(altlabel))

### Turn method into function and repeat for other parameters vocab collections
# Function to extract data from BODC excluding units

bodcdata <- function (collection){
  
          res = GET(paste0 ("https://vocab.nerc.ac.uk/collection/", collection, "/current/?_profile=nvs&_mediatype=application/ld+json"),
                    query = list(status = "all",
                                 relationshipType = "related"))
          
          data <- fromJSON(rawToChar(res$content))[[1]]
          
          names(data)
          collection <- data %>% select (identifier, definition, altLabel, deprecated, `@id`, prefLabel) %>%
            filter (!is.na(identifier)) %>%
            unnest (cols = definition) %>%
            unnest (cols = prefLabel) %>%
            select (-`@language`) %>%
            filter (prefLabel != "en") %>%
            mutate_all(na_if, "") %>%
            transmute (identifier,
                       definition = `@value`,
                       preflabel = as.character(prefLabel),
                       altlabel = altLabel,
                       deprecated,
                       uri = `@id`)  
          return(collection)
}



# P35

P35s <- bodcdata("P35")

res = GET("https://vocab.nerc.ac.uk/collection/P35/current/?_profile=nvs&_mediatype=application/ld+json",
          query = list(status = "all",
                       relationshipType = "related"))

data <- fromJSON(rawToChar(res$content))[[1]]

P35 <- data %>% select (identifier, definition, altLabel, deprecated, `@id`, prefLabel, related) %>%
  filter (!is.na(identifier)) %>%
  unnest (cols = definition) %>%
  unnest (cols = prefLabel) %>%
  select (-`@language`) %>%
  filter (prefLabel != "en") %>%
  mutate_all(na_if, "") %>%
  transmute (identifier,
             definition = `@value`,
             preflabel = as.character(prefLabel),
             altlabel = NA_character_, # Until I find a way to unnest that list
             deprecated,
             uri = `@id`, 
             related) %>%
  unnest(cols = related) %>%
  filter (grepl("P06", related))         


# P02
P02ss <- P02 %>% mutate_all(na_if, "") %>% 
  unnest(cols = definition) %>% 
  unnest(cols = prefLabel) %>%
  filter (definition != "en",
          prefLabel != "en") %>%
  mutate(definition = as.character(definition),
         prefLabel = as.character(prefLabel)) %>%
  mutate_all(na_if, "NULL") %>%
  mutate_all(na_if, "NA")


res = GET("https://vocab.nerc.ac.uk/collection/P02/current/?_profile=nvs&_mediatype=application/ld+json",
          query = list(status = "all"))

data <- fromJSON(rawToChar(res$content))[[1]]

P02 <- data %>% select (identifier, definition, altLabel, deprecated, `@id`, prefLabel) 

P02_all <- P02 %>%
            filter (!is.na(identifier),
                    definition != "") %>% 
            unnest (cols = definition) %>%
            bind_rows(P02 %>% filter (definition == "")) %>%
            unnest (cols = prefLabel) %>%
            filter (prefLabel != "en",
                    definition != "en") %>%
            mutate_all(na_if, "") %>%
            transmute (identifier,
                       definition = case_when (definition == "NA" ~ NA_character_,
                                               TRUE ~ as.character(definition)),
                       preflabel = as.character(prefLabel),
                       altlabel = NA_character_, # Until I find a way to unnest that list
                       deprecated,
                       uri = `@id`)

# Q01

Q01 <- bodcdata(collection = "Q01")


# Manually adding the eunis habitat collection as a vocab term <- requested by MBA and approved by OBIS-VLIZ
eunis_col <- data.frame("eunishabitats","EUNIS habitats","Classification of habitat types according to the EUNIS Biodiversity database", "false", "http://dd.eionet.europa.eu/vocabulary/biodiversity/eunishabitats/")
names(eunis_col)<-c("identifier", "preflabel","definition", "deprecated", "uri")

parameters <- bind_rows(Q01,P01_all, P02_all, P35) %>% 
  bind_rows(eunis_col) %>%
  filter(!is.na(identifier)) 


# Units P06
P06ss <- P06 %>% mutate_all(na_if, "") %>% 
  unnest(cols = definition) %>% 
  unnest(cols = prefLabel) %>%
  filter (definition != "en",
          prefLabel != "en") %>%
  mutate(definition = as.character(definition),
         prefLabel = as.character(prefLabel)) %>%
  mutate_all(na_if, "NULL") %>%
  mutate_all(na_if, "NA")



res = GET("https://vocab.nerc.ac.uk/collection/P06/current/?_profile=nvs&_mediatype=application/ld+json",
          query = list(status = "all"))

data <- fromJSON(rawToChar(res$content))[[1]]

names(data)
P06 <- data %>% select (identifier, definition, altLabel, deprecated, `@id`, prefLabel) 

P06_all <- P06 %>%
  filter (!is.na(identifier),
          definition != "") %>%
  unnest (cols = definition) %>%
  bind_rows(P06 %>% filter (definition == "")) %>%
  unnest (cols = prefLabel) %>%
  filter (prefLabel != "en",
          definition != "en") %>%
  mutate_all(na_if, "") %>%
  transmute (identifier,
             definition = case_when (definition == "NA" ~ NA_character_,
                                     TRUE ~ as.character(definition)),
             preflabel = as.character(prefLabel),
             altlabel = NA_character_, # Until I find a way to unnest that list,
             deprecated,
             uri = `@id`)

##### Link parameters and units

parameters <- parameters %>% left_join(P06_all %>% select (uri, 
                                                            standardunit = preflabel), # ideally altlabel instead of preflabel for standardunit
                                        by=c("related" = "uri")) %>% 
                              rename (standardUnitID = related) %>%
                              distinct()


## VALUES

# L22
L22 <- bodcdata("L22") %>% unnest(cols = altlabel) %>%
                           filter(!grepl("=", altlabel)) %>%
                           mutate (altlabel = as.character(altlabel))



 
# L05
L05 <- bodcdata("L05") %>% unnest(cols = altlabel)  %>%
                           filter(!grepl("=", altlabel)) %>%
                           mutate (altlabel = as.character(altlabel)) %>%
                           mutate_all(na_if, "NULL") %>%
                           mutate_all(na_if, "NA")

# S10
S10 <- bodcdata("S10") %>% unnest(cols = altlabel)  %>%
                          filter(!grepl("=", altlabel)) %>%
                          mutate (altlabel = as.character(altlabel)) %>%
                          mutate_all(na_if, "NULL") %>%
                          mutate_all(na_if, "NA")



# S11
S11ss <- S11 %>% mutate_all(na_if, "") %>% 
                           unnest(cols = definition) %>% 
                           unnest(cols = prefLabel) %>%
                           filter (definition != "en",
                                   prefLabel != "en") %>%
                            mutate(definition = as.character(definition),
                                   prefLabel = as.character(prefLabel)) %>%
                            mutate_all(na_if, "NULL") %>%
                            mutate_all(na_if, "NA")


res = GET("https://vocab.nerc.ac.uk/collection/S11/current/?_profile=nvs&_mediatype=application/ld+json",
          query = list(status = "all",
                       relationshipType = "related"))

data <- fromJSON(rawToChar(res$content))[[1]]

names(data)
S11 <- data %>% select (identifier, definition, altLabel, deprecated, `@id`, prefLabel) 

S11_all <- S11 %>%
  filter (!is.na(identifier),
          definition != "") %>%
  unnest (cols = definition) %>%
  bind_rows(S11 %>% filter (definition == "")) %>%
  unnest (cols = prefLabel) %>%
  filter (prefLabel != "en",
          definition != "en") %>%
  mutate_all(na_if, "") %>%
  transmute (identifier,
             definition = case_when (definition == "NA" ~ NA_character_,
                                     TRUE ~ as.character(definition)),
             preflabel = as.character(prefLabel),
             altlabel = altLabel,
             deprecated,
             uri = `@id`)

# S09

S09ss <- S09 %>% mutate_all(na_if, "") %>% 
  unnest(cols = definition) %>% 
  unnest(cols = prefLabel) %>%
  filter (definition != "en",
          prefLabel != "en") %>%
  mutate(definition = as.character(definition),
         prefLabel = as.character(prefLabel)) %>%
  mutate_all(na_if, "NULL") %>%
  mutate_all(na_if, "NA")


res = GET("https://vocab.nerc.ac.uk/collection/S09/current/?_profile=nvs&_mediatype=application/ld+json",
          query = list(status = "all",
                       relationshipType = "related"))

data <- fromJSON(rawToChar(res$content))[[1]]

S09 <- data %>% select (identifier, definition, altLabel, deprecated, `@id`, prefLabel) 

S09_all <- S09 %>%
  filter (!is.na(identifier),
          definition != "") %>%
  unnest (cols = definition) %>%
  bind_rows(S09 %>% filter (definition == "")) %>%
  unnest (cols = prefLabel) %>%
  filter (prefLabel != "en",
          definition != "en") %>%
  mutate_all(na_if, "") %>%
  transmute (identifier,
             definition = case_when (definition == "NA" ~ NA_character_,
                                     TRUE ~ as.character(definition)),
             preflabel = as.character(prefLabel),
             altlabel = altLabel,
             deprecated,
             uri = `@id`)

# C17
C17ss <- C17 %>% mutate_all(na_if, "") %>% 
  unnest(cols = definition) %>% 
  unnest(cols = prefLabel) %>%
  filter (definition != "en",
          prefLabel != "en") %>%
  mutate(definition = as.character(definition),
         prefLabel = as.character(prefLabel),
         altLabel = as.character(altLabel)) %>%
  mutate_all(na_if, "NULL") %>%
  mutate_all(na_if, "NA")


res = GET("https://vocab.nerc.ac.uk/collection/C17/current/?_profile=nvs&_mediatype=application/ld+json",
          query = list(status = "all",
                       relationshipType = "related"))

data <- fromJSON(rawToChar(res$content))[[1]]

C17 <- data %>% select (identifier, definition, altLabel, deprecated, `@id`, prefLabel) 

C17_all <- C17 %>%
  filter (!is.na(identifier),
          definition != "") %>%
  unnest (cols = definition) %>%
  bind_rows(C17 %>% filter (definition == "")) %>%
  unnest (cols = prefLabel) %>%
  filter (prefLabel != "en",
          definition != "en") %>%
  mutate_all(na_if, "") %>%
  transmute (identifier,
             definition = case_when (definition == "NA" ~ NA_character_,
                                     TRUE ~ as.character(definition)),
             preflabel = as.character(prefLabel),
             altlabel = NA_character_,
             deprecated,
             uri = `@id`)


#############################################
# F02
F02 <- bodcdata("F02")
F02 <- collection %>% mutate (altlabel = NA_character_) %>% # Until I find a way to unnest that list,
 mutate_all(na_if, "NA")
# M20
bodcdata("M20")
M20 <- collection %>% mutate (altlabel = NA_character_) %>% # Until I find a way to unnest that list,
mutate_all(na_if, "NA")
# M21
bodcdata("M21")
M21 <- collection %>% mutate (altlabel = NA_character_) %>% # Until I find a way to unnest that list,
mutate_all(na_if, "NA")
# M24
bodcdata("M24")
M24 <- collection %>% mutate (altlabel = NA_character_) %>%# Until I find a way to unnest that list,
mutate_all(na_if, "NA")
# L06
bodcdata("L06")
L06 <- collection %>% mutate (altlabel = NA_character_) %>% # Until I find a way to unnest that list,
mutate_all(na_if, "NA")
# eunis
res = GET("https://dd.eionet.europa.eu/vocabulary/biodiversity/eunishabitats/json",
          query = list(status = "all"))

data <- fromJSON(rawToChar(res$content))[[2]]

EUNIS <- data %>% unnest(cols = prefLabel) %>%
                  transmute (identifier = `@id`, 
                                             definition = Definition, 
                                             altLabel = NA_character_, 
                                             deprecated = ifelse(Status == "Valid",
                                                                 "false",
                                                                 "true"), 
                                             uri = as.character(`skos:exactMatch`),
                                             prefLabel = `@value`) 




values <- bind_rows(L22, L05, F02, C17, S11, S10, S09, M20, M21, M24, L06, EUNIS)



# Update the data files 
BODCparameters <- parameters
BODCunits <- units
BODCvalues <- values

# Make sure that the tables look correct before overwriting the BODC following files
usethis::use_data(BODCunits, overwrite = TRUE)
usethis::use_data(BODCvalues, overwrite = TRUE)
usethis::use_data(BODCparameters, overwrite = TRUE)