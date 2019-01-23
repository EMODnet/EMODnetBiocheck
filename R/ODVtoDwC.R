#' ODV to DwC
#'
#' converts ODV files exported from ODV to DwC (occurrence and eMoF tables)
#' @param file mandatory parameter, the location of the .txt file you want to convert
#' @import obistools dplyr
#' @export
#' @examples
#' reversetaxmatch(aphiaIDs)


ODVtoDwC <- function (file) {

options(stringsAsFactors = FALSE) ## should be built in somewhere!

#-----------------------------------------------------------------------#
####                    helper functions                             ####
#-----------------------------------------------------------------------#


# to be stored as a table in the R package


basisOfRecord <- "materialSample"


#-----------------------------------------------------------------------#
####                    read dataset                                 ####
#-----------------------------------------------------------------------#


con <- file(description=file, open="r")

##determine number of commentlines before headerline

commentlines <- 0
tmp <- scan(file=con, what=character(), nlines=1, quiet= TRUE, sep = "\n")
while(grepl("^//", tmp[1])==TRUE) {
  if (grepl("^//<DataVariable>", tmp[1])==TRUE) {
    if(exists("DataVariables")) {DataVariables <- c(tmp[1], DataVariables) } else {
      DataVariables <- tmp[1] } }
  commentlines<-commentlines +1
  tmp <- scan(file=con, what=character(), nlines=1, quiet= TRUE, sep = "\n")
}

close(con)


#get labels and P01 - P06 codes
for (i in DataVariables) {
  if (exists("lables")) {lables <- c(lables, midstring(i, first="label", last ="value_type", n=3, m =-3)) } else
  {lables <- midstring(i, first="label", last ="value_type", n=3, m =-3)}
  
  if (exists("P01s")) {P01s <- c(P01s, midstring(i, first="SDN:P01::", last ="SDN:P01::", n=1, m=16)) } else
  {P01s <- midstring(i, first="SDN:P01::", last ="SDN:P01::", n=1, m=16)}
    
  if (exists("P06s")) {P06s <- c(P06s, midstring(i, first="SDN:P06::", last ="SDN:P06::", n=1, m=12)) } else
  {P06s <- midstring(i, first="SDN:P06::", last ="SDN:P06::", n=1, m=12)}
}

### to add other un-R-ly codes
lables <- gsub(" |%|[[]|[]]|#", ".", lables) 

parameters <- data.frame(lables, P01s, P06s, stringsAsFactors = FALSE)



##read datafile in dataframe variable skipping commentlines
df <-read.delim(file, skip = commentlines, colClasses = "character", na.strings ='' )



#-----------------------------------------------------------------------#
####                    map header with DWC terms                    ####
#-----------------------------------------------------------------------#
cdiconvert <- list(
   institutionCode = "Originator",
   institutionCode2 = "Data.Holding.centre",
   institutionCode2 = "EDMO_code",
   datasetName = "EDMED.references",
   datasetName2 =  "Data.set.name",
   eventDate = "yyyy.mm.ddThh.mm.ss.sss",
   eventID3 = "LOCAL_CDI_ID",
   locationID =  "Station.name"  ,
   locality = "Station",
   decimalLongitude = "Longitude..degrees_east.",
   decimalLatitude = "Latitude..degrees_north.",
   references  =  "CDI.record.id" 
  )

odvtermsconvert <-  parameters %>% inner_join(p01todwc, by="P01s") %>% select (dwc, lables)

dwcterms <- c(odvtermsconvert$dwc , names(cdiconvert))
dfterms <- c(odvtermsconvert$lables , unlist(cdiconvert))

convertnames <- setNames(as.list(dfterms),dwcterms)


df2<- obistools::map_fields(df, convertnames)  


df2 <- df2 %>% fncols(unique(c(names(convertnames), p01todwc$dwc))) %>% 
                                mutate (institutionCode = if_else(is.na(institutionCode), institutionCode2, institutionCode),
                                        datasetName = if_else(is.na(datasetName), datasetName2, datasetName),
                                        basisOfRecord = basisOfRecord,
                                        eventID = if_else (is.na(eventID) & !is.na(eventID2),eventID2,
                                                        if_else (!is.na(parentEventID), parentEventID,
                                                                 if_else(!is.na(eventID3), eventID3,
                                                        eventID))),
                                        parentEventID = if_else(!is.na(parentEventID) & parentEventID!=eventID, parentEventID, "NA"),
                                        occurrenceStatus = if_else(is.na(occurrenceStatus) | as.integer(occurrenceStatus) == "1", "present",
                                                        if_else(as.integer(occurrenceStatus) == "0", "absent", occurrenceStatus)),
                                        occurrenceID = if_else (!is.na(datasetName) ,  paste(institutionCode, datasetName, row_number(), sep = "_"), paste(institutionCode, Cruise, row_number(), sep = "_"))
                                ) %>% select(-institutionCode2,-datasetName2,-eventID2)

####to does: 

#special P01s to process later 
# EventDate:  start and stop STRT8601 ENDX8601 # if they exists, then overwrite eventDate 
# Longitude: start and stop STRTXLON ENDXXLON STRTXLAT ENDXXLAT # if they exists, then generate footprintWKT


#------------------------------------------------------------------------------------#
####                    create event core                                         ####
#------------------------------------------------------------------------------------#

# should we create this?


#------------------------------------------------------------------------------------#
####                    create occurrence table                                   ####
#------------------------------------------------------------------------------------#

suppressWarnings(Occurrence <- df2 %>% select (one_of(obistools::occurrence_fields())))

Occurrence <- EMODnetBiocheck::cleandataframe(Occurrence)
Occurrence$id = Occurrence$occurrenceID

#------------------------------------------------------------------------------------#
####                    create emof table                                         ####
#------------------------------------------------------------------------------------#

####to does: 

# terms to examine later maybe combine with instrument.info for cleaner code: 
# Bot..Depth..m,   Cruise 

parametersforemof <- parameters %>% filter (!P01s %in% p01todwc$P01s) 


df3 <- df2 %>% select(occurrenceID, Instrument.Info, parametersforemof$lables)

emof <- data.table::melt (df3, id.vars="occurrenceID", factorsAsStrings = FALSE) %>% rename (measurementType=variable, measurementValue=value ) %>%
  left_join(parametersforemof, by =c("measurementType" = "lables")) %>% 
    mutate(
      measurementTypeID = paste0("http://vocab.nerc.ac.uk/collection/P01/current/",P01s, "/"),
      measurementUnitID = if_else(!is.na(P06s),paste0("http://vocab.nerc.ac.uk/collection/P06/current/",P06s, "/"),"http://vocab.nerc.ac.uk/collection/P06/current/XXXX/"), 
      measurementTypeID = if_else(measurementType == "Instrument.Info", "http://vocab.nerc.ac.uk/collection/Q01/current/Q0100002/", measurementTypeID),
      measurementValueID = if_else(measurementType == "Instrument.Info", paste0("http://vocab.nerc.ac.uk/collection/L22/current/",midstring(measurementValue, first="L22::", last ="L22::", n=5, m=12), "/"), "NA")
      ) %>% 
  select (-P01s, -P06s ) %>% 
  left_join(EMODnetBiocheck::BODCunits %>% select (uri,  measurementUnit=altLabel), by =c("measurementUnitID" = "uri")) %>%
  left_join(EMODnetBiocheck::BODCvalues %>% select (uri,  preflabel), by =c("measurementValueID" = "uri")) %>% 
     mutate( measurementValue= if_else(!is.na(preflabel),preflabel, measurementValue ),
             id = occurrenceID) %>% select (-preflabel)  
  


output <- list()
output$Occurrence <- Occurrence %>% mutate (decimalLatitude = as.numeric(decimalLatitude) , 
                                            decimalLongitude = as.numeric(decimalLongitude))
output$eMoF <- emof

return (output)
}


