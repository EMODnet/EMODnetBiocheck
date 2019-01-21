#' asses fitnes 
#'
#' assess the fitness of an imported IPT file 
#' @param output optional the output from importiptdata 
#' @param Event optional a DwC event table 
#' @param Occurrence optional a DwC occurrence table
#' @param eMoF optional a DwC eMoF table 
#' @export
#' @examples
#' fitness <- assessfitness(output)



assessfitness <- function(Event = NULL, Occurrence = NULL, eMoF = NULL, output = list()) {
 
  
  if (!is.null(output$Event)) { if (is.data.frame(output$Event)) {
    Event <- output$Event
  }}
  if (!is.null(output$Occurrence)) { if (is.data.frame(output$Occurrence)) {
    Occurrence <- output$Occurrence
  }}
  if (!is.null(output$eMoF)) { if (is.data.frame(output$eMoF)) {
    eMoF <- output$eMoF
  }}
  
  if (is.null(Event)) {rm(Event)}
  if (is.null(eMoF)) {rm(eMoF)}
  

# Class C
##### missing ev_check_id
  
  
if (exists("Event")) {
   ev_check_id <- check_eventids(Event)  # Checks that all parenteventIDs linked to an eventID in the dataset
   if ( nrow(ev_check_id) == 0) {
    
    ev_flat0 <- flatten_event(Event) 
    ev_flat <- ev_flat0 %>% filter (!eventID %in% Event$parentEventID)
    
    ev_CheckFields <- check_fields(ev_flat, level = "warning") %>% filter (field %in% event_fields()) %>% mutate(table="event")
    oc_CheckFields <- check_fields(Occurrence, level = "warning") %>% filter (!field %in% event_fields()) %>% mutate(table="occurrence")
    
  } else
  {
    ev_flat <- Event %>% filter (!eventID %in% Event$parentEventID)
    
    ev_CheckFields <- check_fields(ev_flat, level = "warning") %>% filter (field %in% event_fields()) %>% mutate(table="event")
    oc_CheckFields <- check_fields(Occurrence, level = "warning") %>% filter (!field %in% event_fields()) %>% mutate(table="occurrence")
    ev_flat0 <- ev_flat
    
  } 
    if ( nrow(ev_CheckFields) > 0) {
      ev_CheckFields <- Event  %>% select(eventID) %>% mutate (rownew = row_number()) %>% inner_join(ev_flat %>% select(eventID) %>% 
        mutate (row = row_number()), by = "eventID") %>%
        right_join(ev_CheckFields, by="row") %>% select (-eventID, -row) %>% rename (row=rownew) 
      }
    } else  {
      
## code for occurrence Core
      oc_CheckFields <- check_fields(Occurrence, level = "warning") %>% mutate(table="occurrence")
    }

  
  # taxa --------------------------------------
  
  if (!is.null(Occurrence$scientificNameID)){ 
    if ( length(unique(Occurrence$scientificNameID[nchar(Occurrence$scientificNameID)>35]))!=0  ){
    
    reversedmatch <- reversetaxmatch( as.integer(gsub("urn:lsid:marinespecies.org:taxname:", "", unique(Occurrence$scientificNameID[nchar(Occurrence$scientificNameID)>35]))))
    genusorlower <- Occurrence %>% group_by (scientificName, scientificNameID) %>% summarise(count = n()) %>% 
      left_join(reversedmatch, by = c("scientificNameID" = "lsid")) %>% group_by(rank)  %>% summarise(counts = sum(count))  %>% 
      mutate(freq = counts/ sum(counts)) %>% filter(rank %in% c(
        "Aggr.",  "Coll. sp." , "Forma" ,        "Genus" ,
        "Natio",         "Section" ,         "Series",         "Species",
        "Subforma",         "Subgenus" ,        "Subsection",         "Subspecies",
        "Subvariety",         "Variety" 
      )) %>% summarise(sum(freq))
    }
  } else {genusorlower = 0}
  
  


  
if (  exists("eMoF")){
    
### Checks for B  
   if (sum(grepl(BODCinstrument, unique(eMoF$measurementTypeID)))== 0){
      mof_noInstrument <- data.frame(level = c('warning'),field = c('measurementType'), row = NA, 
                                     message = c('No sampling instrument present'))
    }
    
    if (sum(grepl(paste(BODCeffort, collapse="|"), unique(eMoF$measurementTypeID)))== 0){
      mof_noSamplingdescriptor <- data.frame(level = c('warning'),field = c('measurementType'), row = NA, 
                                             message = c('No sampling descriptors present: see http://vocab.nerc.ac.uk/collection/Q01/current/'))
    }

    
  if (exists("Event") == FALSE) {
     if(!"eventID" %in% colnames(Occurrence)) {
        if(length(unique(Occurrence$eventID)) == nrow(Occurrence %>% fncols(c("eventID", "eventDate","decimalLatitude","decimalLongitude","minimumDepthInMeters", "maximumDepthInMeters")) %>% 
                                                      select(eventID, eventDate,decimalLatitude,decimalLongitude,minimumDepthInMeters,maximumDepthInMeters) %>% distinct())
           ) {goodeventID = TRUE } else {goodeventID = FALSE }
     } else {goodeventID = FALSE }

    if (goodeventID == FALSE) {
      goodeventID  <- data.frame(level = c('warning'),field = c('eventID'), row = NA, 
                               message = c('no unique eventID for each seperate event')) 
            }
  }
  

### Checks for A
  
    if (sum(grepl(paste(BODCquantity, collapse="|"), unique(eMoF$measurementTypeID)))== 0){
      mof_noQuantity <- data.frame(level = c('warning'),field = c('measurementType'), row = NA, 
                                   message = c('no quantity present'))
    }
}
  


checks <-  bind_rows (if(exists("ev_CheckFields"))ev_CheckFields, 
                      if(exists("oc_CheckFields"))oc_CheckFields , 
                      if(exists("mof_noInstrument"))mof_noInstrument, 
                      if(exists("mof_noSamplingdescriptor"))mof_noSamplingdescriptor, 
                      if(exists("mof_noQuantity"))mof_noQuantity,
                      if(exists("goodeventID"))goodeventID) 




output <- list()

fitvalue <- if ( any(grepl("is missing", checks$message)) | genusorlower < 0.8) {"F"} else {
  if (exists("eMoF") == FALSE ) {"C"} else {
    if (any(grepl("No sampling", checks$message)) | any(grepl("no unique eventID", checks$message))) {"C"} else {
    if (any(grepl("no quantity present", checks$message)) | any(grepl("no unique eventID", checks$message))) {"B"} else {
    "A"}    
      }
  }}

output$fitvalue <- fitvalue

if(exists("oc_CheckFields")) {
  output$oc_CheckFields <- oc_CheckFields
}
if(exists("ev_CheckFields")) {
  output$ev_CheckFields <- ev_CheckFields
}
if(exists("ev_flat")) {
  output$ev_flat <- ev_flat
}

return (output)

}





