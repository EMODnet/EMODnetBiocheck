#' Check the quality of the dataset
#'
#' This function does all QC checks of the package
#' @param Event optional parameter, the name of the event core file
#' @param Occurrence mandatory parameter, the name of the occurrence file
#' @param eMoF optional parameter, the name of the mof or emof file
#' @param IPTreport optional parameter, in case you want to append the result to an existing listfile
#' @param tree optional parameter, takes value yes if you want the QC report to include the OBIS tree hierachy
#' 
#' @import skosxml
#' @import stringr
#' @import obistools
#' @importFrom tidyr pivot_wider
#' @importFrom stringdist stringsim
#' 
#' @export
#' @examples
#' \dontrun{
#' IPTreport <-checkdataset(Event = event, Occurrence = occurrence, eMoF = emof, IPTreport = IPTreport, tree = FALSE)
#' }


checkdataset = function(Event = NULL, Occurrence = NULL, eMoF = NULL, DNA = NULL, IPTreport = list(), tree = FALSE){

  ###### Extracting tables from IPTreport list, which is the output of importiptdata()
  ######-------------------------------------------------------------------------------
  
  
  # if (exists("IPTreport") == FALSE ) {IPTreport <- list()} # To use checkdataset only with tables and no importiptdata()
  if (!is.null(IPTreport$error)) { 
    stop(IPTreport$error) 
  }
  if (!is.null(IPTreport$Event)) { 
    if (is.data.frame(IPTreport$Event)) {
      Event <- IPTreport$Event
    }}
  if (!is.null(IPTreport$Occurrence)) { 
    if (is.data.frame(IPTreport$Occurrence)) {
      Occurrence <- IPTreport$Occurrence
    }}
  if (!is.null(IPTreport$eMoF)) { 
    if (is.data.frame(IPTreport$eMoF)) {
      eMoF <- IPTreport$eMoF
    }}
  if (!is.null(IPTreport$DNA)) { 
    if (is.data.frame(IPTreport$DNA)) {
      DNA <- IPTreport$DNA
    }}
  
  # Remove R objects if they are null
  if (is.null(Event)) {rm(Event)}
  if (is.null(eMoF)) {rm(eMoF)}
  if (is.null(Occurrence)) {rm(Occurrence)}
  if (is.null(DNA)) {rm(DNA)}


  #---------------------------------------------------------------------------#
  ###                   Fix formatting                                        ####
  #---------------------------------------------------------------------------#      
  
  
  # event_required_fields <- c("eventID", "eventDate", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", 
  #                            "datasetName", "institutionCode", "maximumDepthInMeters", "minimumDepthInMeters")
  # 
  # occurrence_required_fields <- c("eventID", "occurrenceID", "scientificNameID", "scientificName", "basisOfRecord", "occurrenceStatus")
  # 
  # emof_required_fields <- c("occurrenceID", "measurementType", "measurementTypeID","measurementValueID", "measurementValue", "measurementUnitID", "eventID", "measurementUnit")
  
  #### Occurrence fix for Occurrence Core
  ####-----------------------------------
 
  if (  exists("Occurrence") == TRUE ) {
    
    if (  !exists("Event") ) {
    
    
    if(!"occurrenceID" %in% names(Occurrence) & "id" %in% names(Occurrence)) {
      warning("There is no occurrenceID field in the Occurrence core; occurrenceID created from id field")
      Occurrence <- Occurrence %>% mutate(occurrenceID = id)
    }
    
  Occurrence <- Occurrence %>% cleandataframe()
  
  #Occurrence <- fncols(Occurrence, occurrence_required_fields)

    }}
  
  # Occurrence <- Occurrence %>% mutate_all(na_if, 'NA') %>%
  #                              mutate_all(na_if, '') %>%
  #                              mutate_all(na_if, ' ')
  # Occurrence <- Occurrence[,colSums(is.na(Occurrence))<nrow(Occurrence)]
  
  #     Occurrence <- fncols(Occurrence, c("eventDate"))
  
  
  #### Event fix
  ####--------------
  
  if (  exists("Event") == TRUE ) {
    
    if(!"eventID" %in% names(Event) & "id" %in% names(Event)) {
      warning("There is no eventID field in the Event table; eventID created from id field")
      Event <- Event %>% mutate(eventID = id)
    }
    
    if(length(Event) < 2 ) {
      rm(Event)
      } else {
        
        Event <- Event %>% cleandataframe()
        
        # Event <- Event %>% mutate_all(na_if, 'NA') %>%
        #                    mutate_all(na_if, '') %>%
        #                    mutate_all(na_if, ' ')
        # Event <- Event[,colSums(is.na(Event))<nrow(Event)]
        Event <- fncols(Event, c("parentEventID"))
        #        Event[Event =='NA' | Event =='' | Event ==' '] <- NA
        
        #       Event <- fncols(Event, c("eventDate"))
      }
    
    #### Occurrence fix for Event core
    ####-------------------------------
    if (  exists("Occurrence") == TRUE ) {
      
      if(!"eventID" %in% names(Occurrence) & "id" %in% names(Occurrence)) {
        warning("There is no eventID field in the Occurrence extension; eventID created from id field")
        Occurrence <- Occurrence %>% mutate(eventID = id)
      }
      
      Occurrence <- Occurrence %>% cleandataframe()
      
    }
    
    }
  
  
  #### MoF fix
  ####---------------
  
  if ( exists("eMoF") == TRUE  )    { 
    if(length(eMoF) < 2 ) {rm(eMoF)} else {
      
      eMoF <- eMoF %>% cleandataframe()
      
     
      eMoF <- fncols(eMoF, c("occurrenceID", "measurementType", "measurementTypeID","measurementValueID", "measurementValue", "measurementUnitID", "eventID", "measurementUnit"))

      
      eMoF <- eMoF %>% mutate (measurementTypeID = if_else(str_sub(measurementTypeID, -1, -1)!='/' & "vocab.nerc.ac.uk" %in% measurementTypeID,
                                                           paste0(measurementTypeID, 
                                                                  "/"),
                                                           measurementTypeID),
                               measurementValueID = if_else(str_sub(measurementValueID, -1, -1)!='/' & "vocab.nerc.ac.uk" %in% measurementValueID,
                                                            paste0(measurementValueID, 
                                                                   "/"),
                                                            measurementValueID),
                               measurementUnitID = if_else(str_sub(measurementUnitID, -1, -1)!='/' & "vocab.nerc.ac.uk" %in% measurementUnitID,
                                                           paste0(measurementUnitID, 
                                                                  "/"),
                                                           measurementUnitID),
                               measurementTypeID = str_replace(measurementTypeID, "https://", "http://"),
                               measurementValueID = str_replace(measurementValueID, "https://", "http://"),
                               measurementUnitID = str_replace(measurementUnitID, "https://", "http://")
                               
      )
      
      
      if ( exists("Event") == TRUE){
        if(!is.null(eMoF$eventID) == FALSE){
          if(is.null(eMoF$id) == FALSE){
          eMoF$eventID <- as.character(eMoF$id) #eventID column is required in the measurements table.
        }}
      } else {
        if(!is.null(eMoF$occurrenceID) == FALSE){
          if(is.null(eMoF$id) == FALSE){
        eMoF$occurrenceID <- as.character(eMoF$id) #occurrenceID column is required in the measurements table.
      }}
    }}}
  
  
  #----------------------------------------------------------------------------#
  ####                    Generate summary                                  ####
  #----------------------------------------------------------------------------#
  
  
  
  ###### Creating the datasummary table: Overview of event and occurrence records
  ######-------------------------------------------------------------------------
  
  if (  exists("Occurrence") == TRUE ) {
      
      
  occurrencetemp <- suppressWarnings(if(("occurrenceStatus" %in% names(Occurrence)) == FALSE) {
                                        mutate  (Occurrence, occurrenceStatus = "present")
                                    } else {Occurrence} %>% fncols ("basisOfRecord") %>%
                                                            select (one_of(c("occurrenceStatus","basisOfRecord", "eventID"))))
  
  
  if (exists("Event")) {

    
    if("absent" %in% occurrencetemp$occurrenceStatus | NA %in% occurrencetemp$occurrenceStatus  )   {     
      IPTreport$datasummary <- Event %>% fncols(c("type", "eventType")) %>% # Check for both "type" and "eventType", and create a common column "event_type"
                                         mutate(event_type = coalesce(eventType, type)) %>%
                                         select (eventID, event_type) %>% 
                                         left_join(occurrencetemp, by = "eventID") %>% 
                                         group_by(eventID, event_type, occurrenceStatus, basisOfRecord ) %>% 
                                         summarise( occount = sum(!is.na(basisOfRecord))) %>%
                                         pivot_wider(names_from = occurrenceStatus, 
                                                     values_from = occount, 
                                                     values_fn = sum,
                                                     values_fill = 0) %>% 
                                         fncols(c("absent", "NA", "present")) %>%
                                         mutate(absent = as.integer(absent), `NA` = as.integer(`NA`) ) %>% 
                                         group_by(event_type, basisOfRecord) %>% 
                                         summarise(n_events = sum(!is.na(unique(eventID))) , n_absent = sum(absent), n_present = sum(as.integer(present)), n_NA = sum(`NA`)) %>%
                                         select(event_type,n_events, basisOfRecord, n_present, n_absent, n_NA )
     } else {
      IPTreport$datasummary <- Event %>% fncols(c("type", "eventType")) %>% # Check for both "type" and "eventType", and create a common column "event_type"
                                         mutate(event_type = coalesce(eventType, type)) %>%
                                         select (eventID, event_type) %>% 
                                         left_join(occurrencetemp, by = "eventID") %>% 
                                         mutate (occurrenceStatus = if_else(!is.na(occurrenceStatus), paste("n_", occurrenceStatus, sep ="") , occurrenceStatus )) %>%
                                         group_by(event_type, occurrenceStatus, basisOfRecord ) %>% 
                                         summarise(occount = sum(!is.na(basisOfRecord)), n_events = sum(!is.na(unique(eventID)))) %>%  
                                         mutate (occount = if_else(occount =="0", as.integer(NA), occount )) %>% 
                                         pivot_wider(names_from = occurrenceStatus, 
                                                     values_from = occount,
                                                     values_fill = 0) 
    }
    
    IPTreport$datasummary[IPTreport$datasummary =='0' | IPTreport$datasummary  =='NA'] <- NA
    IPTreport$datasummary <- IPTreport$datasummary[,names(IPTreport$datasummary) == "event_type" |colSums(is.na(IPTreport$datasummary))<nrow(IPTreport$datasummary)] 
    
  } else {
    IPTreport$datasummary <- occurrencetemp %>% group_by(basisOfRecord, occurrenceStatus) %>% 
                                                summarize(countOccurrence = n())        
  }}
  
  
  # Datasummary for datasets without an occurrence table
  if (exists("Occurrence") == FALSE ) {
    if (exists("Event")) {
      if (!is.null(Event)){
        
        IPTreport$datasummary <- Event %>% fncols(c("type", "eventType")) %>% # Check for both "type" and "eventType", and create a common column "event_type"
                                           mutate(event_type = coalesce(eventType, type)) %>%
                                           select (eventID, event_type) %>%
                                           group_by(event_type) %>%
                                           summarise(n_events = sum(!is.na(eventID)))
        
      }
    }
  }
  
  
  ###### Creating the mofsummary table: Overview of the measurement or fact records
  ######------------------------------------------------------------------------------
  
  if (exists("eMoF")) {
      
      parastolookup <- (eMoF %>% select (measurementTypeID) %>% 
                                 distinct() %>% filter(!measurementTypeID %in% BODCparameters$uri))$measurementTypeID
      
      # Need to add warning when params not in vocabs collections P01|Q01. DELETE this comment when error created
      
      suppressWarnings(
      if(length(parastolookup[!is.na(parastolookup)&parastolookup!=""])>0){
      parastolookedup <- suppressWarnings(getunitsandparams(vocids = parastolookup, vocabs ="P01|Q01"))
      parameters <- bind_rows(BODCparameters, if(exists("parastolookedup") & length(parastolookedup) > 0 & !is.null(parastolookedup)) parastolookedup)
      } else { parameters <- BODCparameters}
      )
      
      #parameters <- suppressWarnings(getunitsandparams(vocid = unique(eMoF$measurementTypeID), vocabs ="P01|Q01"))
      
      
      valuestolookup <- (eMoF %>% select (measurementValueID) %>% 
                                  distinct() %>% filter(!measurementValueID %in% BODCvalues$uri))$measurementValueID
      
      
      
      # Need to add warning when values not in vocabs collections S10|S11|L22|L05|M20|M21|M22|M23|C35|C17. DELETE this comment when error created
      
      suppressWarnings(
      if(length(valuestolookup[!is.na(valuestolookup)&valuestolookup!=""])>0){
        valuestolookedup <- suppressWarnings(getskossxmldatainfo(vocid = valuestolookup, vocabs ="S04|S10|S11|L22|L05|M20|M21|M22|M23|C35|C17"))
        values <- bind_rows(BODCvalues, valuestolookedup)
      } else { values <- BODCvalues}
      )
      #values <- suppressWarnings(getskossxmldatainfo(vocid = unique(eMoF$measurementValueID), vocabs ="S10|S11|L22|L05|M20|M21|M22|M23|C35|C17"))
      
      
      IPTreport$mofsummary <-  suppressWarnings(eMoF %>% mutate(IDlink = if_else(is.na(occurrenceID) , "eventMoF", "occurrenceMoF" ), measurementValue =  as.numeric(measurementValue)) %>% 
                                                         group_by(IDlink, measurementType,  measurementTypeID, measurementUnit) %>% 
                                                         summarize(count = n(), minValue = min(measurementValue), maxValue = max(measurementValue) ) %>% 
                                                         ungroup() %>%
                                                         left_join(parameters , by = c("measurementTypeID"="uri")) %>%
                                                         transmute (IDlink, measurementType, minValue,  maxValue, measurementUnit,  
                                                                    count, TypeID_standardUnit = standardunit, TypeID_name = preflabel, 
                                                                    TypeID_definition = definition))

      
      # Rounding up values and eliminating decimals of max-min
      IPTreport$mofsummary$minValue <- ifelse(IPTreport$mofsummary$minValue > 0, 
                                              ifelse(IPTreport$mofsummary$minValue > 0.01 , 
                                                     format(round(IPTreport$mofsummary$minValue, 2), digits = 2, nsmall = 2), 
                                                     sprintf("%.7f", IPTreport$mofsummary$minValue)), 0)
      IPTreport$mofsummary$maxValue <- ifelse(IPTreport$mofsummary$maxValue > 0, 
                                              ifelse(IPTreport$mofsummary$maxValue > 0.01 , 
                                                     format(round(IPTreport$mofsummary$maxValue, 2), digits = 2, nsmall = 2), 
                                                     sprintf("%.7f", IPTreport$mofsummary$maxValue)), 0)
      
      

  ###### Creating the mofsummary_values table: Overview of standardised measurement values
  ######----------------------------------------------------------------------------------   
      
      IPTreport$mofsummary_values <- eMoF %>% filter(!is.na(measurementValueID)) %>% 
                                              mutate(IDlink = if_else(is.na(occurrenceID) , "eventMoF", "occurrenceMoF" )) %>% 
                                              group_by(IDlink, measurementType, measurementValue, measurementValueID) %>% 
                                              summarize(count = n()) %>% 
                                              ungroup() %>%
                                              left_join(values, by = c("measurementValueID"="uri")) %>%
                                              transmute (IDlink, measurementType, measurementValue, 
                                                         ValueID_name = preflabel, ValueID_definition = definition)
      
  
    
    if (is.data.frame(IPTreport$mofsummary)) {if (nrow(IPTreport$mofsummary)>0){
      IPTreport$mofsummary  <- IPTreport$mofsummary %>% arrange(IDlink , desc(count))
    }}
  }
  
  
  
  #-----------------------------------------------------------------------#
  ####                    Integrity Checks			                   ####
  #-----------------------------------------------------------------------#
  
  
  if ( exists("Event")){
    ev_check_id <- check_eventids(Event)  # Checks that all parenteventIDs linked to an eventID in the dataset
    
    if (exists("Occurrence")) {
      oc.ev_check_id <- check_extension_eventids(Event,Occurrence) # Checks if all eventIDs in your occurrence extentsion link to the event Core
    }                              
    
    if (  exists("eMoF")){
      mof.ev_check_id <- check_extension_eventids(Event,eMoF)  # Checks that all the eventIDs in your eMoF extentsion link to the event Core
      
      if (exists("Occurrence")) {
        
        if ( sum(is.na(eMoF$occurrenceID)) != nrow(eMoF)  ){
          mof.oc_check_id <- eMoF %>% mutate (level = 'error', 
                                              field = 'occurrenceID', 
                                              row = row_number(),
                                              message = 'This occurrenceID has no corresponding occurrenceID in the occurrence Extension') %>%
            filter (!is.na(occurrenceID)) %>%  
            anti_join(Occurrence, by = "occurrenceID")  %>%
            select (level, field, row, message) # Checks that all eMoF occurrenceIDs exist in the Occurrence table
          
          
          mof.oc.ev_check_id <- eMoF %>% mutate (level = 'error', 
                                                 field = 'eventID', 
                                                 row = row_number(),
                                                 message = 'This eventID differs from the eventID provided in the related Occurrence') %>%
            inner_join(Occurrence, by = "occurrenceID") %>% 
            mutate(eventID.y = as.character(eventID.y)) %>%
            anti_join(eMoF %>% mutate(eventID = as.character(eventID)), 
                      by = c(  "eventID.y"=  "eventID", "occurrenceID" = "occurrenceID")) %>%
            select (level, field, row, message) # Checks that all eMoF eventIDs are linked to the same eventID as the related occurrenceID
        }
        
      }
    }
    if (exists("DNA")) {
      dna.ev_check_id <- check_extension_eventids(Event,DNA) # Checks that all eventIDs in your DNA extension link to the event Core
    }
  }
  
  if (exists("Occurrence")) {
    
    if ( exists("Event") == FALSE & exists("eMoF") == TRUE  ){
      mof.oc_check_id <- check_extension_occurrenceids(Occurrence, eMoF)
      
    }
    
    if ( exists("Event") == FALSE & exists("DNA") == TRUE  ){
      dna.oc_check_id <- check_extension_occurrenceids(Occurrence, DNA)
    }
    
    
    occurrenceIDs <- Occurrence$occurrenceID[!is.na(Occurrence$occurrenceID) & !Occurrence$occurrenceID == ""]
    dup_rows <- which(duplicated(occurrenceIDs))
    
    if (length(dup_rows) > 0) {
      dup_occID <- data_frame(level = "error",
                              field = "occurrenceID",
                              row = dup_rows,
                              message = paste0("occurrenceID ", Occurrence$occurrenceID[dup_rows], " is duplicated in the Occurrence table")
      ) # Checks that occurrenceID is unique in the occurrence table
    }
  }
  
  
  
  # Preparing general_issues table: Overview of all issues
  #-------------------------------------------------------- 
  emoferror <- data.frame()
  emoferror <- bind_rows(emoferror, 
                         if(exists("mof.ev_check_id")) mof.ev_check_id, 
                         if(exists("mof.oc_check_id")) mof.oc_check_id,
                         if(exists("mof.oc.ev_check_id")) mof.oc.ev_check_id)
  
  occurrenceerror <- data.frame()
  occurrenceerror <- bind_rows(occurrenceerror,
                               if(exists("oc.ev_check_id")) oc.ev_check_id,
                               if(exists("dup_occID")) dup_occID)
  
  eventerror <- data.frame()
  eventerror <- bind_rows(eventerror, 
                          if(exists("ev_check_id")) ev_check_id)
  
  dnaerror <- data.frame()
  dnaerror <- bind_rows(dnaerror, 
                        if(exists("dna.ev_check_id")) dna.ev_check_id,
                        if(exists("dna.oc_check_id")) dna.oc_check_id)

  
  
  #-----------------------------------------------------------------------#
  ####                    Tree structure                               ####
  #-----------------------------------------------------------------------#
  
  if ( exists("Event") &  exists("eMoF") & exists("Occurrence") & tree == "yes" ) { 
    if ( if(exists("ev_check_id") ){nrow(ev_check_id) == 0} & 
         if(exists("mof.oc_check_id") ){nrow(mof.oc_check_id) ==0} & 
         if(exists("mof.oc.ev_check_id") ){nrow(mof.oc.ev_check_id) ==0} &
         if(exists("mof.ev_check_id") ){nrow(mof.ev_check_id) ==0} &
         if(exists("dna.ev_check_id") ){nrow(dna.ev_check_id) == 0} &
         if(exists("dna.oc_check_id") ){nrow(dna.oc_check_id) == 0} ) {
      tryCatch({IPTreport$tree <- treeStructure(Event, Occurrence, eMoF)}, error = function(x){print("tree gives error")})
      
      
    }}
  
  
  
  
  
  
  #-----------------------------------------------------------------------#
  ####                    measurementsorfacts                          ####
  #-----------------------------------------------------------------------#
  
  if (  exists("eMoF")){
    
    
    #### Quality Checks on eMoF records
    ####--------------------------------
    
    mof_noUnit <- c('N', 'NA', '', ' ', 'None') # add values to the list if they refer to 'no unit'. Ruben removed 'Dimensionless', 'Dmnless' because they are units
    
    
    # Missing BODC terms
    #-------------------
    
    mof_oc_noTypeID <- eMoF %>% filter (!is.na(occurrenceID), is.na(measurementTypeID) ) %>% 
                                select (measurementType, measurementUnit) %>% 
                                mutate(IDlink = 'occurrenceMoF', message = 'measurementTypeID is missing') %>% 
                                group_by (IDlink,measurementType, measurementUnit, message) %>% 
                                summarize(count = n())
    
    mof_noValueID <- eMoF %>% filter ( (is.na(measurementUnit) | 
                                          (measurementUnit %in% mof_noUnit)  | 
                                          measurementUnitID == "http://vocab.nerc.ac.uk/collection/P06/current/XXXX/" &
                                          is.na(measurementUnit)) &
                                       is.na(measurementValueID) & 
                                       (!measurementType %in% c("count") & 
                                          !measurementTypeID %in% BODCnomofvalues)) %>%
                              mutate(IDlink = if_else(!is.na(occurrenceID),"occurrenceMoF", "eventMoF") ) %>%
                              mutate(message = 'measurementValues which may need a measurementValueID or a Unit') %>%  
                              group_by (IDlink, measurementType, measurementValue, message) %>% 
                              summarize(count = n()) %>% 
                              arrange (desc(measurementType))
    
    
    mof_noUnitID <- eMoF %>% filter(!is.na(measurementUnit) & 
                                    !measurementUnit %in% mof_noUnit &
                                    is.na(measurementUnitID)) %>%
                             mutate(IDlink = if_else(!is.na(occurrenceID),"occurrenceMoF", "eventMoF") ) %>%
                             mutate(message = 'measurementUnits which may need a measurementUnitID') %>%  
                             group_by (IDlink, measurementType, measurementUnit, message) %>% 
                             summarize(count = n()) %>% 
                             arrange (desc(measurementType))
                             
    
    # Checking if the BODC terms are deprecated
    #-------------------------------------------
    
    deprec_typeID <- eMoF %>% select (measurementTypeID) %>% 
                              distinct() %>%
                              left_join(BODCparameters %>% select (uri, deprecated),
                                        by = c("measurementTypeID" = "uri")) %>%
                              filter(deprecated == "true") %>%
                              select(measurementTypeID)
    
        if(nrow(deprec_typeID) > 0){
                deprec_typeIDs <- eMoF %>% filter(measurementTypeID %in% deprec_typeID) %>%
                                        mutate(IDlink = if_else(!is.na(occurrenceID),"occurrenceMoF", "eventMoF") ) %>%
                                        mutate(message = 'This measurementTypeID is deprecated') %>%  
                                        group_by (IDlink, measurementTypeID, message) %>% 
                                        summarize(count = n()) %>% 
                                        arrange (desc(measurementTypeID))
        }
    
    deprec_valueID <- eMoF %>% select (measurementValueID) %>% 
                              distinct() %>%
                              left_join(BODCvalues %>% select (uri, deprecated),
                                        by = c("measurementValueID" = "uri")) %>%
                              filter(deprecated == "true") %>%
                              select(measurementValueID)
    
        if(nrow(deprec_valueID) > 0){
                deprec_valueIDs <- eMoF %>% filter(measurementValueID %in% deprec_valueID) %>%
                                        mutate(IDlink = if_else(!is.na(occurrenceID),"occurrenceMoF", "eventMoF") ) %>%
                                        mutate(message = 'This measurementValueID is deprecated') %>%  
                                        group_by (IDlink, measurementValueID, message) %>% 
                                        summarize(count = n()) %>% 
                                        arrange (desc(measurementValueID))
        }
    
    deprec_unitID <- eMoF %>% select (measurementUnitID) %>% 
                               distinct() %>%
                               left_join(BODCunits %>% select (uri, deprecated),
                                         by = c("measurementUnitID" = "uri")) %>%
                               filter(deprecated == "true") %>%
                               select(measurementUnitID)
    
        if(nrow(deprec_unitID) > 0){
                deprec_unitIDs <- eMoF %>% filter(measurementUnitID %in% deprec_unitID) %>%
                                        mutate(IDlink = if_else(!is.na(occurrenceID),"occurrenceMoF", "eventMoF") ) %>%
                                        mutate(message = 'This measurementUnitID is deprecated') %>%  
                                        group_by (IDlink, measurementUnitID, message) %>% 
                                        summarize(count = n()) %>% 
                                        arrange (desc(measurementUnitID))
        }
    
    
    # Checking if the BODC terms resolve to sampling descriptors and effort
    #-----------------------------------------------------------------------    
    
    
    if (sum(grepl(paste0(BODCinstrument, collapse="|"), unique(eMoF$measurementTypeID)))== 0){
      mof_noInstrument <- data_frame(level = "warning",
                                     field = "measurementType", 
                                     row = NA, 
                                     message = "No sampling instrument present")
    }
    
    if (sum(grepl(paste(BODCeffort, collapse="|"), unique(eMoF$measurementTypeID)))== 0){
      mof_noSamplingdescriptor <- data.frame(level = "warning",
                                             field = "measurementType", 
                                             row = NA, 
                                             message = "No sampling descriptors/effort present")
    }
    
    
    # Checking if the measurementIDs (type, value, unit) resolve to BODC terms
    #--------------------------------------------------------------------------    
    
      mof_oc_TypeID_NotResolve <- eMoF %>% filter (!is.na(occurrenceID), !is.na(measurementTypeID) ) %>% 
                                           select (measurementType, measurementTypeID, measurementUnit) %>% 
                                           anti_join(parameters, by = c("measurementTypeID"="uri")) %>%
                                           mutate(IDlink = 'occurrenceMoF', 
                                                  message = 'measurementTypeID does not resolve') %>% 
                                           group_by (IDlink,measurementType, measurementTypeID, measurementUnit, message) %>% 
                                           summarize(count = n())
      
      mof_oc_ValueID_NotResolve <- eMoF %>% filter (!is.na(occurrenceID), !is.na(measurementValueID) ) %>% 
                                            select (measurementValue, measurementValueID) %>% 
                                            anti_join(values, by = c("measurementValueID"="uri")) %>%
                                            mutate(IDlink = 'occurrenceMoF', 
                                                   message = 'measurementValueID does not resolve') %>% 
                                            group_by (IDlink,measurementValue, measurementValueID, message) %>% 
                                            summarize(count = n())
      
      mof_oc_UnitID_NotResolve <- eMoF %>% filter (!is.na(occurrenceID), !is.na(measurementUnitID) ) %>% 
                                            select (measurementUnit, measurementUnitID) %>% 
                                            anti_join(BODCunits, by = c("measurementUnitID"="uri")) %>%
                                            mutate(IDlink = 'occurrenceMoF', 
                                                   message = 'measurementUnitID does not resolve') %>% 
                                            group_by (IDlink,measurementUnit, measurementUnitID, message) %>% 
                                            summarize(count = n())
 
                                             
      # Checking eMoF records integrity --> NULL measurementValueIDs, present record with 0 values in occurrence eMoFs, duplicated eMoF records.
      #-----------------------------------------------------------------------------------------------------------------------------------------
    
    mof_ValueNull <- eMoF %>% mutate (level = 'error', 
                                      field = 'measurementValue', 
                                      row = row_number(),
                                      message = 'MeasurementValue of Null') %>%
                              filter (is.na(measurementValue))  %>% 
                              select (level,field, row ,message)
    
      
    
    if ( sum(is.na(eMoF$occurrenceID)) != nrow(eMoF)  ){
        mof_oc_Value_err <- suppressWarnings(eMoF %>% mutate (level = 'error', 
                                                               field = 'measurementValue', 
                                                               row = row_number(),
                                                               message = 'Biological quantitification of 0 while occurrenceStatus is present') %>%
                                                inner_join (fncols(Occurrence, c("occurrenceStatus")) %>% filter (is.na(occurrenceStatus) | occurrenceStatus == 'present' ) %>% 
                                                              select (occurrenceID), 
                                                            by = 'occurrenceID') %>%
                                                mutate(measurementValue = as.numeric(measurementValue)) %>% 
                                                filter (measurementValue == 0 &
                                                        measurementTypeID %in% BODCquantity) %>%  
                                                select (level,field, row ,message))
      } else {mof_oc_Value_err <-NULL }  
      
        
    if ( sum(is.na(eMoF$occurrenceID)) != nrow(eMoF)  ){
      mof_oc_Value_war <- suppressWarnings(eMoF %>% mutate (level = 'warning', 
                                                         field = 'measurementValue', 
                                                         row = row_number(),
                                                         message = 'Biological value of 0 while occurrenceStatus is present') %>%
                                                 inner_join (fncols(Occurrence, c("occurrenceStatus")) %>% filter (is.na(occurrenceStatus) | occurrenceStatus == 'present' ) %>% 
                                                                                                           select (occurrenceID), 
                                                             by = 'occurrenceID') %>%
                                                 mutate(measurementValue = as.numeric(measurementValue)) %>% 
                                                 filter (measurementValue == 0 &
                                                         !measurementTypeID %in% BODCquantity ) %>%  
                                                 select (level,field, row ,message))
      } else {mof_oc_Value_war <-NULL }
    
    duplicated_measurementType_oc <- eMoF %>% filter (!is.na(occurrenceID),is.na(measurementTypeID)) %>%
                            select (occurrenceID, measurementType) %>% 
                            group_by (occurrenceID, measurementType) %>% 
                            summarize(count = n())  %>% 
                            filter (count >1 )  %>%
                            inner_join((eMoF) %>% mutate (row = row_number()), 
                                       by = c("occurrenceID", "measurementType") ) %>% 
                            ungroup() %>%
                            mutate (field = 'measurementType', 
                                    level = 'error',  
                                    message = 'Duplicate measurementType linked to the same occurrence') %>%
                            select (level,field, row ,message)

    duplicated_measurementTypeID_oc <- eMoF %>% filter (!is.na(occurrenceID), !is.na(measurementTypeID)) %>%
                            select (occurrenceID, measurementTypeID) %>% 
                            group_by (occurrenceID, measurementTypeID) %>% 
                            summarize(count = n())  %>% 
                            filter (count >1 )  %>%
                            inner_join((eMoF) %>% mutate (row = row_number()), 
                                       by = c("occurrenceID", "measurementTypeID") ) %>% 
                            ungroup() %>%
                            mutate (field = 'measurementType', 
                                    level = 'error',  
                                    message = 'Duplicate measurementTypeID linked to the same occurrence') %>%
                            select (level,field, row ,message)
 
    exact_duplicate_emof_check <- duplicated(eMoF) | duplicated(eMoF, fromLast = TRUE)
    exact_duplicate_emof_list <- eMoF[exact_duplicate_emof_check, ] %>%
                            mutate(field = 'eMoF',
                                    level = 'error',
                                    message = 'Duplicate eMoF record',
                                    row=which(exact_duplicate_emof_check)) %>%
                            select(level, field, row, message)
    
    if (  exists("Event") ){
      duplicated_measurementType_ev <- eMoF %>% filter (is.na(occurrenceID),is.na(measurementTypeID)) %>%
                              select (eventID, measurementType) %>% 
                              group_by (eventID, measurementType) %>% 
                              summarize(count = n())  %>% 
                              filter (count >1 )  %>%
                              inner_join((eMoF) %>% mutate (row = row_number()), 
                                         by = c("eventID", "measurementType") )  %>% 
                              ungroup() %>%
                              mutate (field = 'measurementType', 
                                      level = 'error',  
                                      message = 'Duplicate measurementType linked to the same event') %>%
                              select (level,field, row ,message)
      
      duplicated_measurementTypeID_ev <- eMoF %>% filter (is.na(occurrenceID), !is.na(measurementTypeID)) %>%
                              select (eventID, measurementTypeID) %>% 
                              group_by (eventID, measurementTypeID) %>% 
                              summarize(count = n())  %>% 
                              filter (count >1 )  %>%
                              inner_join((eMoF) %>% mutate (row = row_number()), 
                                         by = c("eventID", "measurementTypeID") )  %>% 
                              ungroup() %>%
                              mutate (field = 'measurementType', 
                                      level = 'error',  
                                      message = 'Duplicate measurementTypeID linked to the same event') %>%
                              select (level,field, row ,message)

      # Missing BODC terms for Event related records
      #----------------------------------------------   
      mof_ev_noTypeID <- eMoF %>% filter (!is.na(eventID), is.na(occurrenceID), is.na(measurementTypeID) ) %>% 
                                  select (measurementType, measurementUnit) %>% 
                                  mutate(IDlink = 'eventMoF', 
                                         message = 'measurementTypeID is missing') %>% 
                                  group_by (IDlink,measurementType, measurementUnit, message) %>% 
                                  summarize(count = n())
      
      mof_ev_TypeID_NotResolve <- eMoF %>% filter (!is.na(eventID), is.na(occurrenceID),  !is.na(measurementTypeID) ) %>% 
                                           select (measurementType, measurementTypeID, measurementUnit) %>% 
                                           anti_join(parameters, 
                                                     by = c("measurementTypeID"="uri")) %>%
                                           mutate(IDlink = 'eventMoF', 
                                                  message = 'measurementTypeID does not resolve') %>% 
                                           group_by (IDlink,measurementType, measurementTypeID, measurementUnit, message) %>% 
                                           summarize(count = n())
        
        
       mof_ev_ValueID_NotResolve <- eMoF %>% filter (!is.na(eventID), is.na(occurrenceID), !is.na(measurementValueID) ) %>% 
                                             select (measurementValue, measurementValueID) %>% 
                                             anti_join(values, 
                                                       by = c("measurementValueID"="uri")) %>%
                                             mutate(IDlink = 'eventMoF', 
                                                    message = 'measurementValueID does not resolve') %>% 
                                             group_by (IDlink,measurementValue, measurementValueID, message) %>% 
                                             summarize(count = n())
       
       mof_ev_UnitID_NotResolve <- eMoF %>% filter (!is.na(eventID), is.na(occurrenceID), !is.na(measurementUnitID) ) %>% 
                                             select (measurementUnit, measurementUnitID) %>% 
                                             anti_join(BODCunits, 
                                                       by = c("measurementUnitID"="uri")) %>%
                                             mutate(IDlink = 'eventMoF', 
                                                    message = 'measurementUnitID does not resolve') %>% 
                                             group_by (IDlink,measurementUnit, measurementUnitID, message) %>% 
                                             summarize(count = n())
      }
    
    
    # Creating the mof_issues table: Overview of potential issues with Measurements or Facts records
    #------------------------------------------------------------------------------------------------
        
    IPTreport$dtb$mof_issues <- bind_rows(mof_oc_noTypeID, mof_noValueID, mof_noUnitID,
                                                       if(exists("deprec_typeIDs")) deprec_typeIDs,
                                                       if(exists("deprec_valueIDs")) deprec_valueIDs,
                                                       if(exists("deprec_unitIDs")) deprec_unitIDs,
                                                       if(exists("mof_oc_TypeID_NotResolve")) mof_oc_TypeID_NotResolve, 
                                                       if(exists("mof_oc_ValueID_NotResolve")) mof_oc_ValueID_NotResolve,
                                                       if(exists("mof_oc_UnitID_NotResolve")) mof_oc_UnitID_NotResolve,
                                                       if(exists("mof_ev_noTypeID")) mof_ev_noTypeID, 
                                                       if(exists("mof_ev_TypeID_NotResolve"))  mof_ev_TypeID_NotResolve,
                                                       if(exists("mof_ev_ValueID_NotResolve"))  mof_ev_ValueID_NotResolve,
                                                       if(exists("mof_ev_UnitID_NotResolve"))  mof_ev_UnitID_NotResolve) %>%
                                                 select (one_of(c("IDlink" , "measurementType", "measurementTypeID", "measurementValue", 
                                                                    "measurementValueID",  "measurementUnit", "measurementUnitID", "message", "count")))
    
    if (is.data.frame(IPTreport$dtb$mof_issues)){
      if (nrow(IPTreport$dtb$mof_issues)>0){
        IPTreport$dtb$mof_issues <- IPTreport$dtb$mof_issues[,colSums(is.na(IPTreport$dtb$mof_issues))<nrow(IPTreport$dtb$mof_issues)] %>% 
                                    arrange(IDlink , message, desc(count))
    }}

    
    # Preparing general_issues table: Overview of all issues
    
    emoferror <- rbind(emoferror, mof_ValueNull, mof_oc_Value_war, mof_oc_Value_err, duplicated_measurementType_oc, duplicated_measurementTypeID_oc, exact_duplicate_emof_list,
                       if(exists("duplicated_measurementType_ev")) duplicated_measurementType_ev,
                       if(exists("duplicated_measurementTypeID_ev")) duplicated_measurementTypeID_ev,
                       if(exists("mof_noInstrument")) mof_noInstrument,  
                       if(exists("mof_noSamplingdescriptor")) mof_noSamplingdescriptor
    )
    
  }
  
  #-----------------------------------------------------------------------#
  ####                    QC checks                                    ####
  #-----------------------------------------------------------------------#
  
  #### QC checks - Metadata
  ####----------------------
  
  
  # Check if IPT url is too long
  #------------------------------  
  
  
  if(is.null(IPTreport$ipt_url) == FALSE){
    if(nchar(IPTreport$ipt_url) > 255){
      metadataerror <- data.frame(level = "error",
                                  field = "metadata",
                                  message = "The IPT resource URL seem to be longer than 255 characters",
                                  count = 1)
    }
  }

   
  #### QC checks - Check if all required fields are there  
  ####----------------------------------------------------
  
  other_fields <- c("datasetName", "institutionCode")
  
  if (  exists("Event") ){
    
  # Checks run only when the Event table exists
  #---------------------------------------------    
    
      for (i in other_fields){
        if (i %in% names(Event) == FALSE){
          
          assign(paste0("no_ev_", i), data.frame(level = "error",
                                              field = i,
                                              message = paste0("Required field ", i, " is missing")))
        }}
    
    if ( nrow(ev_check_id) == 0) {
    
      ev_flat0 <- flatten_event(Event) 
      ev_flat <- ev_flat0 %>% filter (!eventID %in% Event$parentEventID)
      
      ev_CheckFields <- check_fields(ev_flat, level = "warning") %>% filter (field %in% event_fields())
      
      if (  exists("Occurrence") ){
        oc_CheckFields <- check_fields(Occurrence, level = "warning") %>% filter (!field %in% event_fields())
        }
      
      } else {
    
            ev_flat <- Event %>% filter (!eventID %in% Event$parentEventID)
            
            ev_CheckFields <- check_fields(ev_flat, level = "warning") %>% filter (field %in% event_fields())
            
            if (  exists("Occurrence") ){
                oc_CheckFields <- check_fields(Occurrence, level = "warning") %>% filter (!field %in% event_fields())
              }
            ev_flat0 <- ev_flat
    
      } 
    
    if ( nrow(ev_CheckFields) > 0) {
      ev_CheckFields <- Event  %>% select(eventID) %>% 
                                   mutate (rownew = row_number()) %>% 
                                   inner_join(ev_flat %>% select(eventID) %>% 
                                                          mutate (row = row_number()), 
                                              by = "eventID") %>%
                                   right_join(ev_CheckFields, by="row") %>% 
                                   select (-eventID, -row) %>% 
                                   rename (row=rownew) 
      
    }} else  {
      
      # Checks run only when the Event table does not exists
      #----------------------------------------------------- 
      
      if (  exists("Occurrence") ) {
      
        for (i in other_fields){
          if (i %in% names(Occurrence) == FALSE){
            
            assign(paste0("no_oc_", i), data.frame(level = "error",
                                                field = i,
                                                message = paste0("Required field ", i, " is missing")))
          }}
      
      oc_CheckFields <- check_fields(Occurrence, level = "warning")
    }}
  
 
    
    # Checks run only when the DNA table exists
    #---------------------------------------------
    
  if (  exists("DNA") ){
    dna_checkFields <- check_required_fields_dna(Occurrence, DNA)
  }
    
      # Checks one to one relationship issues
      #---------------------------------------
  
  
  if(exists("Occurrence")){
    
    
    many_names <- one_to_one_check(fncols(Occurrence, c("scientificName", "scientificNameID")), 
                                   "scientificName", 
                                   "scientificNameID")
    
    if(!is.null(many_names) &
       nrow(many_names) > 0){
      
      IPTreport$exclusivity_fails$taxa_ids_not_exclusive <-  many_names
    }
    
  }
  
  
 
  if(exists("eMoF")){
    
    
    
    many_types <- one_to_one_check(fncols(eMoF, c("measurementType", "measurementTypeID")), 
                                   "measurementType", 
                                   "measurementTypeID")
    
    if(!is.null(many_types) &
       nrow(many_types) > 0){
      
      IPTreport$exclusivity_fails$type_ids_not_exclusive <-  many_types
    }
    
    
    
    
    many_values <- one_to_one_check(fncols(eMoF, c("measurementValue", "measurementValueID")), 
                                    "measurementValue", 
                                    "measurementValueID")
    
    if(!is.null(many_values) &
       nrow(many_values) > 0){
      
      IPTreport$exclusivity_fails$value_ids_not_exclusive <-  many_values
    }
    
    
    
    
    many_units <- one_to_one_check(fncols(eMoF, c("measurementUnit", "measurementUnitID")), 
                                   "measurementUnit", 
                                   "measurementUnitID")
    
    if(!is.null(many_units) &
       nrow(many_units) > 0){
      
      IPTreport$exclusivity_fails$unit_ids_not_exclusive <-  many_units
    }
    
    
  }
  
  
  
  
  # Preparing general_issues table: Overview of all issues
  
  occurrenceerror <- bind_rows(if(exists("occurrenceerror") & nrow(occurrenceerror) > 0) occurrenceerror, 
                               if(exists("oc_CheckFields")) oc_CheckFields,
                               if(exists("no_oc_datasetName")) no_oc_datasetName,
                               if(exists("no_oc_institutionCode")) no_oc_institutionCode)
  
  eventerror <- bind_rows(if(exists("eventerror") & nrow(eventerror) > 0) eventerror, 
                          if(exists("ev_CheckFields")) ev_CheckFields,
                          if(exists("no_ev_datasetName")) no_ev_datasetName,
                          if(exists("no_ev_institutionCode")) no_ev_institutionCode)
  
  dnaerror <- bind_rows(if(exists("dnaerror") & nrow(dnaerror) > 0) dnaerror, 
                        if(exists("dna_checkFields")) dna_checkFields)
  
  
  
  
  #### QC checks - Check coordinates  
  ####---------------------------------
  
  coords <- c("decimalLatitude", "decimalLongitude")
  
  if (  exists("Event")) {
    
    # Checks run only when the Event table exists
    #--------------------------------------------- 
    
    
  if (sum(coords %in% names(Event)) == 2) {
    
    Event$decimalLongitude <- as.numeric(Event$decimalLongitude)
    Event$decimalLatitude <- as.numeric(Event$decimalLatitude)
    
    if (is.numeric(Event$decimalLatitude) == FALSE) {
      
      no_numeric_lat <- data.frame (level = 'error', 
                                    field = 'decimalLatitude',
                                    message = 'Some decimalLatitude values are not numeric')
    }
    
    if (is.numeric(Event$decimalLongitude) == FALSE) {
      
      no_numeric_long <- data.frame (level = 'error', 
                                     field = 'decimalLongitude',
                                     message = 'Some decimalLongitude values are not numeric')
    }
    
    if ("coordinateUncertaintyInMeters" %in% names(Event)) {
      Event$coordinateUncertaintyInMeters <- as.numeric(Event$coordinateUncertaintyInMeters)
      if (is.numeric(Event$coordinateUncertaintyInMeters) == FALSE) {
        no_numeric_coord_uncer <- data.frame(level = 'error', 
                                             field = 'coordinateUncertaintyInMeters',
                                             message = 'Some coordinateUncertaintyInMeters values are not numeric')
      }
    }
    
    
    suppressWarnings(ev_flat$decimalLongitude <- as.numeric(ev_flat$decimalLongitude))
    suppressWarnings(ev_flat$decimalLatitude <- as.numeric(ev_flat$decimalLatitude))
    
    
    GoodCords <- suppressWarnings(ev_flat %>% fncols(c("minimumDepthInMeters", "maximumDepthInMeters", "coordinateUncertaintyInMeters")) %>% 
                                              select (-coordinateUncertaintyInMeters) %>%
                                              filter (decimalLatitude < 90, 
                                                      decimalLatitude > -90, 
                                                      !is.na(decimalLatitude),
                                                      decimalLongitude < 180, 
                                                      decimalLongitude > -180, 
                                                      !is.na(decimalLongitude)) %>%
                                              inner_join(Event %>% select (eventID, one_of(c("coordinateUncertaintyInMeters"))), 
                                                         by = "eventID") %>%  
                                              select (eventID,decimalLatitude, decimalLongitude, one_of (c("coordinateUncertaintyInMeters")), minimumDepthInMeters, maximumDepthInMeters)) 
    
    if (nrow(GoodCords)>0){
      
        OnLand <- tryCatch({suppressWarnings(check_onland(GoodCords, buffer = 3000))},
                           error=function(x) { occurrenceID <- c(NA)       
                           data.frame(occurrenceID) }) 
    
        depth <- tryCatch({suppressWarnings(check_depth(GoodCords %>% filter (!is.na(minimumDepthInMeters) | 
                                                                              !is.na(!maximumDepthInMeters)), 
                                                        depthmargin = 150))},
                          error=function(x) { occurrenceID <- c(NA)       
                                              data.frame(occurrenceID) })
    
    
        Cords00_rep <- Event %>% mutate (level = 'error', 
                                         field = 'coordinates_error', 
                                         row = row_number(),
                                         message = 'decimalLatitude and decimalLongitude are both 0') %>%
                                 filter (decimalLatitude ==0 , 
                                         decimalLongitude == 0) %>% 
                                 select (level, field, row, message)
        

        ###### Creating the plot_coordinates table: Geographical cover of the dataset & Issues on map
        ######-----------------------------------------------------------------------------------------              
    
        plot_coordinates <- suppressWarnings(GoodCords %>% mutate (quality = if_else ((eventID %in% OnLand$eventID), 'On Land', 
                                                                                  if_else ((eventID %in% depth$eventID), 'Check Depth',
                                                                                       if_else ((decimalLatitude ==0 & decimalLongitude == 0), '0-0 Coordinates',        
                                                                                                'OK')))) %>% 
                                                           select (eventID, decimalLatitude, decimalLongitude, one_of (c("coordinateUncertaintyInMeters")), quality) %>% 
                                                           distinct())
    
    
    
        depth_rep <- tryCatch({suppressWarnings(check_depth(GoodCords, report=T, depthmargin = 150))}, 
                                    error=function(x) { occurrenceID <- c(NA)       
                                    data.frame(occurrenceID) }) # fails when obis api is down. Trycatch solves it
    
    
    
        OnLand_rep <-  tryCatch({suppressWarnings(check_onland(GoodCords,report=T, buffer = 3000) %>% mutate (field = 'coordinates_error'))}, 
                                  error=function(x) { occurrenceID <- c(NA)       
                                  data.frame(occurrenceID) }) # fails when obis api is down. Trycatch solves it
    
      if (ncol(depth_rep) > 1 & ncol(OnLand_rep) > 1){
        goodcord_rep <- GoodCords %>% select(eventID) %>% 
                                      mutate (row = row_number()) %>% 
                                      inner_join(rbind(depth_rep, OnLand_rep), 
                                                 by=c("row")) %>% 
                                      select (-row)
    
        coord_rep <- Event %>% select(eventID) %>% 
                               mutate (row = row_number()) %>% 
                               inner_join(goodcord_rep, 
                                          by=c("eventID")) %>% 
                               select (-eventID)}
    
    
        parentdepts <- ev_flat0 %>% fncols (c("decimalLatitude", "decimalLongitude", "minimumDepthInMeters", "maximumDepthInMeters")) %>%
                                    filter(eventID %in% (Event %>% filter (eventID  %in% depth$eventID) %>% 
                                                                   select(parentEventID) %>% 
                                                                   distinct())$parentEventID & 
                                                         !is.na(decimalLatitude) & 
                                                         !is.na(decimalLongitude) & 
                                                         !is.na(minimumDepthInMeters) & 
                                                         !is.na(maximumDepthInMeters))
    
    if (nrow(parentdepts)>0) {
      
        depth_rep2 <- check_depth(parentdepts, report=T, depthmargin = 150)
      
        parentdepts_rowconv <- Event %>% select(eventID) %>% 
                                         mutate (rownew = row_number()) %>% 
                                         inner_join(parentdepts %>% mutate (rowold = row_number()) %>% 
                                                                    select(eventID, rowold), 
                                                    by=c("eventID")) %>% 
                                         select (-eventID)
      
        depth_rep2_conv <- depth_rep2 %>% inner_join(parentdepts_rowconv, 
                                                     by=c("row"="rowold")) %>% 
                                          select (-row, row = rownew)
      
        coord_rep <- rbind(coord_rep,depth_rep2_conv)
    }
    
        
        # Preparing general_issues table: Overview of all issues
    
    eventerror <- bind_rows(if(exists("eventerror")){eventerror},
                            if(exists("coord_rep")) coord_rep,
                            if(exists("Cords00_rep")) Cords00_rep)
                            
  }  
    
    eventerror <- bind_rows(if(exists("eventerror")){eventerror},
                            if(exists("no_numeric_lat")) no_numeric_lat,
                            if(exists("no_numeric_long")) no_numeric_long,
                            if(exists("no_numeric_coord_uncer")) no_numeric_coord_uncer)
    
    } } else {
      
      
      # Checks run only when the Event table does not exists
      #------------------------------------------------------ 
      
      if (exists("Occurrence")) {
    
    if (sum(coords %in% names(Occurrence)) == 2) {
      
      Occurrence$decimalLongitude <- as.numeric(Occurrence$decimalLongitude)
      Occurrence$decimalLatitude <- as.numeric(Occurrence$decimalLatitude)
      
      
      if (is.numeric(Occurrence$decimalLatitude) == FALSE) {
        
        no_numeric_lat <- data.frame (level = 'error', 
                                      field = 'decimalLatitude',
                                      message = 'Some decimalLatitude values are not numeric')
      }
      
      if (is.numeric(Occurrence$decimalLongitude) == FALSE) {
        
        no_numeric_long <- data.frame (level = 'error', 
                                       field = 'decimalLongitude',
                                       message = 'Some decimalLongitude values are not numeric')
      }
      
      if ("coordinateUncertaintyInMeters" %in% names(Occurrence)) {
        Occurrence$coordinateUncertaintyInMeters <- as.numeric(Occurrence$coordinateUncertaintyInMeters)
        if (is.numeric(Occurrence$coordinateUncertaintyInMeters) == FALSE) {
          no_numeric_coord_uncer <- data.frame(level = 'error', 
                                               field = 'coordinateUncertaintyInMeters',
                                               message = 'Some coordinateUncertaintyInMeters values are not numeric')
        }
      }
      
    
    
    
    GoodCords <- suppressWarnings(Occurrence %>% fncols(c("minimumDepthInMeters", "maximumDepthInMeters"))  %>% 
                                                 filter (decimalLatitude < 90, 
                                                         decimalLatitude > -90, 
                                                         !is.na(decimalLatitude),
                                                         decimalLongitude < 180, 
                                                         decimalLongitude > -180, 
                                                         !is.na(decimalLongitude)) %>%
                                                 select (occurrenceID,decimalLatitude, decimalLongitude, one_of (c("coordinateUncertaintyInMeters")), minimumDepthInMeters, maximumDepthInMeters))
    
    
    if (nrow(GoodCords)>0){
      
        OnLand <- suppressWarnings(check_onland(GoodCords, buffer = 3000))
    
        depth <- tryCatch({suppressWarnings(check_depth(GoodCords %>% filter (!is.na(minimumDepthInMeters) | 
                                                                              !is.na(!maximumDepthInMeters)), 
                                                        depthmargin = 150))},
                          error=function(x) { occurrenceID <- c(NA)       
                                              data.frame(occurrenceID) })
    
        plot_coordinates <- suppressWarnings(GoodCords %>% mutate (quality = if_else ((occurrenceID %in% OnLand$occurrenceID), 'On Land', 
                                                                                  if_else ((occurrenceID %in% depth$occurrenceID), 'Check Depth',
                                                                                       if_else ((decimalLatitude ==0 & decimalLongitude == 0), '0-0 Coordinates',
                                                                                                'OK')))) %>% 
                                                           select (occurrenceID, decimalLatitude, decimalLongitude,one_of (c("coordinateUncertaintyInMeters")), quality ) %>% 
                                                           distinct())
    
    
        Cords00_rep <- Occurrence %>% mutate (level = 'error', 
                                              field = 'coordinates', 
                                              row = row_number(),
                                              message = 'decimalLatitude and decimalLongitude are both 0') %>%
                                      filter (decimalLatitude == 0 , 
                                              decimalLongitude == 0) %>% 
                                      select (level, field, row, message)
    
        depth_rep <- check_depth(GoodCords, report=T, depthmargin = 150)
        
        OnLand_rep <- check_onland(GoodCords,report=T, buffer = 3000) %>% mutate (field = 'coordinates_error')
    
        goodcord_rep <- GoodCords %>% select(occurrenceID) %>% 
                                      mutate (row = row_number()) %>% 
                                      inner_join(rbind(depth_rep, OnLand_rep), 
                                                 by=c("row")) %>% 
                                      select (-row)
    
        coord_rep <- Occurrence %>% select(occurrenceID) %>% 
                                    mutate (row = row_number()) %>% 
                                    inner_join(goodcord_rep, 
                                               by=c("occurrenceID")) %>% 
                                    select (-occurrenceID)

        
        # Preparing general_issues table: Overview of all issues
    
        occurrenceerror <- bind_rows(if(exists("occurrenceerror")){
                                     if(nrow(occurrenceerror) > 0) {occurrenceerror}}, 
                                     if(exists("coord_rep")) coord_rep,
                                     if(exists("Cords00_rep")) Cords00_rep)
    }
    
    occurrenceerror <- bind_rows(if(exists("occurrenceerror")){occurrenceerror},
                                 if(exists("no_numeric_lat")) no_numeric_lat,
                                 if(exists("no_numeric_long")) no_numeric_long,
                                 if(exists("no_numeric_coord_uncer")) no_numeric_coord_uncer)
    
    }}}
  
  if (exists("plot_coordinates")){
    
  IPTreport$plot_coordinates <- plot_coordinates
  
  }
  
 
  #### QC checks - Check if the content of the fields is correct
  ####-----------------------------------------------------------
  
if(exists("Occurrence")){  
  # occurrenceStatus is standardized 
  #-----------------------------------
  
  occStat_valid <- c("present", "absent")
  
  if("occurrenceStatus" %in% names(Occurrence)){
  badOccStat <- Occurrence %>% mutate (level = 'error', 
                                        field = 'occurrenceStatus', 
                                        row = row_number(),
                                        message = 'occurrenceStatus does not seem to contain a valid term such as: "present", "absent"') %>%
                                filter (!is.na(occurrenceStatus) &
                                        !occurrenceStatus %in% occStat_valid) %>% 
                                select (level, field, row, message)
  
  if(!"present" %in% Occurrence$occurrenceStatus){
  no_present <- data.frame(level = 'warning',
                           field = 'occurrenceStatus',
                           message = 'None of the occurrence records have occurrenceStatus = "present"')
  }
  
  
  # Preparing general_issues table: Overview of all issues

  occurrenceerror <- bind_rows(if(exists("occurrenceerror")){occurrenceerror},
                               if(exists("badOccStat")) badOccStat,
                               if(exists("no_present")) no_present)
    
  }
  
  
  # basisOfRecord is standardized 
  #---------------------------------
  
  basORec_valid <- c("PreservedSpecimen", "FossilSpecimen", "LivingSpecimen", "HumanObservation",
                     "MachineObservation", "MaterialSample", "Occurrence")
  
  if("basisOfRecord" %in% names(Occurrence)){
  badbasORec <- Occurrence %>% mutate (level = 'error', 
                                       field = 'basisOfRecord', 
                                       row = row_number(),
                                       message = 'basisOfRecord does not seem to contain a valid term such as: "PreservedSpecimen", "FossilSpecimen", "LivingSpecimen", "HumanObservation", "MachineObservation", "MaterialSample", "Occurrence"') %>%
                               filter (!is.na(basisOfRecord) &
                                       !basisOfRecord %in% basORec_valid) %>% 
                               select (level, field, row, message)
  
  
  # Preparing general_issues table: Overview of all issues
  
  occurrenceerror <- bind_rows(if(exists("occurrenceerror")){occurrenceerror}, 
                               if(exists("badbasORec")){badbasORec})
  }
  
}
  # datasetName is standardized 
  #------------------------------
  
  if(is.null(IPTreport$title) == FALSE & exists("no_ev_datasetName") == FALSE & exists("no_oc_datasetName") == FALSE) {
      if(exists("Event")){
        if(nrow(Event %>% select (datasetName) %>% filter (!is.na(datasetName)) %>% distinct()) != 1) {
          manyDatname <- data.frame(level = 'warning',
                                    field = 'datasetName',
                                    message = 'datasetName contains more than one unique values, excluding NA values')
        } else{
          if (stringsim(as.character(Event %>% select (datasetName) %>% filter (!is.na(datasetName)) %>% distinct()),
                        as.character(IPTreport$title)
                        ) < 1 ){ # 100% as percentage of similarity, ideally the datasetName must be identical to the title of the dataset although due to encoding issues they may differ
            datnameNotTitle <- data.frame(level = 'warning',
                                          field = 'datasetName',
                                          message = 'datasetName is slightly different from the title of the IPT resource')
          }
        }

        
        # Preparing general_issues table: Overview of all issues
        
        eventerror <- bind_rows(if(exists("eventerror")){eventerror},
                                if(exists("manyDatname")) manyDatname,
                                if(exists("datnameNotTitle")) datnameNotTitle) 
        
    } else {
      if(exists("Occurrence")){
        if(nrow(Occurrence %>% select (datasetName) %>% filter (!is.na(datasetName)) %>% distinct()) != 1) {
          manyDatname <- data.frame(level = 'warning',
                                    field = 'datasetName',
                                    message = 'datasetName contains more than one unique values, excluding NA values')
        } else{
          if (stringsim(as.character(Occurrence %>% select (datasetName) %>% filter (!is.na(datasetName)) %>% distinct()),
                        as.character(IPTreport$title)
                        ) < 1 ){
            datnameNotTitle <- data.frame(level = 'warning',
                                          field = 'datasetName',
                                          message = 'datasetName is slightly different from the title of the IPT resource')
          }
        }

      # Preparing general_issues table: Overview of all issues
      
      occurrenceerror <- bind_rows(if(exists("occurrenceerror")){occurrenceerror},
                                   if(exists("manyDatname")) manyDatname,
                                   if(exists("datnameNotTitle")) datnameNotTitle)
      
    }}
  }

   
  # Check dates against the ISO format  
  #-------------------------------------
  
  if (  exists("Event") ) {
    
        date_rep <- check_eventdate(Event %>% fncols(c("eventDate"))) 
        if(nrow(date_rep)> 0) {
          date_rep <- filter (date_rep, message != "eventDate NA does not seem to be a valid date")
        }  
          
        dates_plot <- Event %>% fncols(c("eventDate")) %>% 
                                select (eventDate) %>% 
                                filter (!is.na(eventDate)) %>% 
                                mutate (year = as.numeric((substr(eventDate, 1,4))))

        
        # Preparing general_issues table: Overview of all issues
        
        eventerror <- bind_rows(eventerror, date_rep)  
    
    
  } else {
    if(exists("Occurrence")){
    
        date_rep <- check_eventdate(Occurrence %>% fncols(c("eventDate")))
        if(nrow(date_rep)> 0) {
             date_rep <- filter (date_rep, message != "eventDate NA does not seem to be a valid date")
           }
          
                                                 
        dates_plot <- Occurrence %>% fncols(c("eventDate")) %>% 
                                     select (eventDate) %>% 
                                     filter (!is.na(eventDate)) %>% 
                                     mutate (year = as.numeric((substr(eventDate, 1,4))))
        
        # Preparing general_issues table: Overview of all issues
        
        occurrenceerror <- bind_rows(occurrenceerror, date_rep)  
  } }
  
  IPTreport$dates_plot <- if(exists("dates_plot")){dates_plot}  
  
  
  
  
  # Checking potential duplicate occurrences  
  #--------------------------------------------
  
  
  # Including BODCbiometrics into duplicates check when the eMoF table exists
  #---------------------------------------------------------------------------
if(exists("Occurrence")){ 
  if (exists("eMoF")  ){
    
        mof_biometric <- eMoF %>% filter (!is.na(occurrenceID), 
                                          measurementTypeID %in% (BODCbiometrics) ) %>% 
                                  group_by(occurrenceID, measurementType, measurementTypeID) %>%
                                  summarise (measurementValue = max(measurementValue)) %>% 
                                  ungroup()
    
    if (exists("Event") ) {
      
        duplicatescheck <- c ('eventID', 'lifeStage', 'sex', 'scientificName',  'scientificNameID', 'identificationRemarks',
                              'identificationQualifier') # terms to check in case of Event Core
      
      if (nrow(mof_biometric) > 0 ) {
        
        mof_biometric <-  mof_biometric  %>% pivot_wider(id_cols = !measurementTypeID, 
                                                         names_from = measurementType, 
                                                         values_from = measurementValue)
        biometricterms <- names(mof_biometric %>% select (-occurrenceID))
        duplicatescheck <- c(duplicatescheck, biometricterms)
        
        occ_dups_rows <- (fncols( (Occurrence %>% left_join(mof_biometric, 
                                                           by = "occurrenceID") ), duplicatescheck)  %>% 
                          select (duplicatescheck) %>% 
                          duplicated () +
                          fncols((Occurrence %>% left_join(mof_biometric, 
                                                           by = "occurrenceID")), duplicatescheck)  %>% 
                          select (duplicatescheck) %>% 
                          duplicated (fromLast=TRUE))
        
      } else {
        
        occ_dups_rows <- (fncols(Occurrence, duplicatescheck)  %>% 
                          select (duplicatescheck) %>% 
                          duplicated () +
                          fncols(Occurrence, duplicatescheck)  %>% 
                          select (duplicatescheck) %>% 
                          duplicated (fromLast=TRUE) )
        
      }} else  {
        
        duplicatescheck <- c ("decimalLatitude", "decimalLongitude", 'eventDate', 'eventTime', 'minimumDepthInMeters', 'maximumDepthInMeters'
                              , 'eventID', 'lifeStage', 'sex', 'samplingProtocol', 'scientificName',  'scientificNameID', 'identificationRemarks',
                              'identificationQualifier') # terms to check in case of occurrence Core
        
        if (nrow(mof_biometric) > 0 )    {
          
              mof_biometric <-  mof_biometric %>% pivot_wider(id_cols = !measurementTypeID, 
                                                              names_from = measurementType, 
                                                              values_from = measurementValue)
              biometricterms <- names(mof_biometric %>% select (-occurrenceID))
              duplicatescheck <- c(duplicatescheck, biometricterms)
              
              occ_dups_rows <- (fncols((Occurrence %>% left_join(mof_biometric, 
                                                                 by = "occurrenceID")), duplicatescheck)  %>% 
                                select (duplicatescheck) %>% 
                                duplicated () +
                                fncols((Occurrence %>% left_join(mof_biometric, 
                                                                 by = "occurrenceID")), duplicatescheck)  %>% 
                                select (duplicatescheck) %>% 
                                duplicated (fromLast=TRUE))
              
        } else {
          
          occ_dups_rows <- (fncols(Occurrence, duplicatescheck)  %>% 
                            select (duplicatescheck) %>% 
                            duplicated () +
                            fncols(Occurrence, duplicatescheck)  %>% 
                            select (duplicatescheck) %>% 
                            duplicated (fromLast=TRUE) )
          
        }}} else {
          
          
          # Checking potential duplicate occurrences: This check runs if there is no eMoF table
          #-------------------------------------------------------------------------------------
          
          
   if (exists("Event")) {
     
         duplicatescheck <- c ('eventID', 'lifeStage', 'sex', 'scientificName',  'scientificNameID', 'identificationRemarks',
                                  'identificationQualifier') # terms to check in case of event Core
            
         occ_dups_rows <-(fncols(Occurrence, duplicatescheck)  %>% 
                          select (duplicatescheck) %>% 
                          duplicated () +
                          fncols(Occurrence, duplicatescheck)  %>% 
                          select (duplicatescheck) %>% 
                          duplicated (fromLast=TRUE) )
         
    } else {
            
         duplicatescheck <- c ("decimalLatitude", "decimalLongitude", 'eventDate', 'eventTime', 'minimumDepthInMeters', 'maximumDepthInMeters', 
                               'eventID', 'lifeStage', 'sex', 'samplingProtocol', 'scientificName',  'scientificNameID', 'identificationRemarks',
                               'identificationQualifier') # terms to check in case of occurrence Core
            
         occ_dups_rows <- (fncols(Occurrence, duplicatescheck)  %>% 
                           select (duplicatescheck) %>% 
                           duplicated () +
                           fncols(Occurrence, duplicatescheck)  %>% 
                           select (duplicatescheck) %>% 
                           duplicated (fromLast=TRUE))
            
          }}
  
  occ_dups_rows <- occ_dups_rows > 0
  
  
  occ_dups <- (Occurrence %>% mutate (row = row_number()))[occ_dups_rows,] %>%  mutate (field = 'occurrence_error', 
                                                                                        level = 'warning',  
                                                                                        message = 'Potential duplicate record') %>%
                                                                                select (level,field, row ,message)
  
  # Preparing general_issues table: Overview of all issues
  
  occurrenceerror <- rbind(occurrenceerror, occ_dups)  
  
  
  
  # Checking taxa 
  #-----------------
  
  if (!is.null(Occurrence$scientificNameID)){
    
    snidUnique <- unique(na.omit(Occurrence$scientificNameID[nchar(Occurrence$scientificNameID)>35]))
    
    if ( length(snidUnique)!=0 & sum(grepl("urn:lsid:marinespecies.org:taxname:", snidUnique)) > 0) {
    
      as.integer(gsub("urn:lsid:marinespecies.org:taxname:", "", snidUnique)) # Ruben thinks that this line is useless
      reversedmatch <- reversetaxmatch (as.integer(gsub("urn:lsid:marinespecies.org:taxname:", "", snidUnique)))
    
      
      
      ###### Creating the taxa table: Overview unmatched taxa
      ######--------------------------------------------------     
      
      IPTreport$dtb$taxa <- Occurrence %>% group_by (scientificName, scientificNameID) %>% 
                                           summarise(count = n()) %>% 
                                           left_join(reversedmatch, 
                                                     by = c("scientificNameID" = "lsid"))
    
      if (exists ("Event") & exists ("plot_coordinates") ) { 
        
        ###### Creating the MarTaxaonLand table: Marine taxa on land
        ######-------------------------------------------------------  
        
        IPTreport$MarTaxaonLand <- IPTreport$dtb$taxa %>% ungroup() %>% 
                                                          filter (isMarine == 1 & 
                                                                  (isBrackish == 0 | is.na(isBrackish))  &  
                                                                  (isFreshwater == 0 | is.na(isFreshwater)) & 
                                                                  (isTerrestrial == 0 | is.na(isTerrestrial)) ) %>% 
                                                          inner_join (fncols(Occurrence, c("occurrenceStatus")) %>%  filter (is.na(occurrenceStatus) | 
                                                                                                                             occurrenceStatus == 'present' ), 
                                                                      by = c("scientificNameID"), suffix = c("", "_orig")) %>% 
                                                          filter (eventID %in% (OnLand$eventID))

       
                
        #    IPTreport$nonMartaxaonAtSea <- IPTreport$dtb$taxa %>%  ungroup() %>% filter (isMarine == 0 & isBrackish == 0) %>% inner_join (Occurrence, by = c("scientificNameID")) %>% filter (!eventID %in% (OnLand$eventID))
        IPTreport$plot_coordinates <- IPTreport$plot_coordinates %>% mutate (quality = if_else ((eventID %in% IPTreport$MarTaxaonLand$eventID), 'Marine Taxa on Land', quality))
        
        
        
        occ_onland <- Occurrence %>% mutate (row = row_number()) %>% 
                                     filter (scientificNameID %in% IPTreport$MarTaxaonLand$scientificNameID, 
                                             eventID %in% IPTreport$MarTaxaonLand$eventID) %>% 
                                     select (row) %>%
                                     mutate (level = 'warning', 
                                             field ='scientificNameID', 
                                             message = 'Marine taxon located on land' )
 
        # Preparing general_issues table: Overview of all issues
        
        occurrenceerror <- rbind(occurrenceerror, occ_onland)                                     
        
      } else if (exists ("plot_coordinates")) {
      
        IPTreport$MarTaxaonLand <- IPTreport$dtb$taxa %>% ungroup() %>% 
                                                          filter (isMarine == 1 & 
                                                                  (isBrackish == 0 | is.na(isBrackish))  &  
                                                                  (isFreshwater == 0 | is.na(isFreshwater)) & 
                                                                  (isTerrestrial == 0 | is.na(isTerrestrial)) ) %>% 
                                                          inner_join (OnLand %>% select (occurrenceID) %>% 
                                                                                 inner_join (fncols(Occurrence, c("occurrenceStatus")) %>% 
                                                                                             filter (is.na(occurrenceStatus) | 
                                                                                                     occurrenceStatus == 'present' ), 
                                                                                             by =c("occurrenceID"),  suffix = c("", "_orig")) %>% 
                                                                                 select (occurrenceID, scientificNameID ), 
                                                                      by = c("scientificNameID"))
        
        #    IPTreport$nonMartaxaonAtSea <- IPTreport$dtb$taxa %>%  ungroup() %>% filter (isMarine == 0 & isBrackish == 0) %>% filter (!scientificNameID %in% (OnLand$scientificNameID))
        
        IPTreport$plot_coordinates <- IPTreport$plot_coordinates %>% mutate (quality = if_else ((occurrenceID %in% IPTreport$MarTaxaonLand$occurrenceID), 'Marine Taxa on Land', quality))
        
        
        occ_onland <- Occurrence %>% mutate (row = row_number()) %>% 
                                     filter (occurrenceID %in% IPTreport$MarTaxaonLand$occurrenceID) %>% 
                                     select (row) %>%
                                     mutate (level = 'warning', 
                                             field ='scientificNameID', 
                                             message = 'Marine taxon located on land' )

        
        # Preparing general_issues table: Overview of all issues
        
        occurrenceerror <- rbind(occurrenceerror, occ_onland)                                     
      
      
      }
      
        
      ###### Creating the kingdoms table: Taxonomic cover of the dataset
      ######--------------------------------------------------------------        
      
        IPTreport$kingdoms <- IPTreport$dtb$taxa %>% filter(!is.na(kingdom)) %>% 
                                                     group_by (kingdom, class)%>% 
                                                     summarise(counts = sum(count))
 
      
      # Checking if scientificNameID is standardised
      #-----------------------------------------------      
      
             
        occ_notmatched <- Occurrence %>% mutate (row = row_number()) %>% 
                                         filter (!is.na(scientificNameID)) %>% 
                                         filter (scientificName %in% (IPTreport$dtb$taxa %>% filter(is.na(scientificNameMatch)))$scientificName) %>% 
                                         filter (scientificNameID %in% (IPTreport$dtb$taxa %>% filter(is.na(scientificNameMatch)))$scientificNameID) %>% 
                                         select (row) %>%  
                                         mutate (level = 'warning', 
                                                 field ='scientificNameID', 
                                                 message = 'scientificNameID does not resolve' )
  } else {
    
    occ_notmatched <- data.frame (row = NA, 
                                  level = c('warning'), 
                                  field = c('scientificNameID'), 
                                  message = c('None of the scientificNameIDs are LSID for WoRMS' ))
    
  }
    
    # Preparing general_issues table: Overview of all issues
    
    occurrenceerror <- rbind(occurrenceerror, occ_notmatched)          
  }
}
  # Checking content of DNA related fields
  #-----------------------------------------------
if(exists("DNA")){
  dnaerror <- rbind(dnaerror, check_content_dna_fields(Occurrence, DNA))
}
  
  #-------------------------------------------------------------------------------#
  ####                    Generate report table                                ####
  #-------------------------------------------------------------------------------#
  
  
  ###### Creating the error tables: eventerror_table, occurrenceerror_table & emoferror_table 
  ######--------------------------------------------------------------------------------------
  
  if (exists("Event")) {
    
    if(is.null(eventerror) == FALSE & nrow(eventerror) > 0) {
      if(is.null(eventerror$row) == FALSE) {
        if(nrow(eventerror %>% filter (!is.na(row))) > 0) {
      
      IPTreport$dtb$eventerror_table <- eventerror %>% distinct() %>% 
                                                       filter (!is.na(row)) %>% 
                                                       pivot_wider(id_cols = !level, 
                                                                   names_from = field, 
                                                                   values_from = message, 
                                                                    values_fn = max) %>%
                                                       inner_join (Event %>% mutate (row = row_number()), 
                                                                   by = "row", 
                                                                   suffix = c("_error", "")) %>% 
                                                       arrange(eventID, row)
        } } } }
  
  
  if(exists("Occurrence")){
     if(is.null(occurrenceerror) == FALSE & nrow(occurrenceerror) > 0) {
       if(is.null(occurrenceerror$row) == FALSE) {
        if(nrow (occurrenceerror %>% filter (!is.na(row))) >0 ) {
     
     IPTreport$dtb$occurrenceerror_table <- occurrenceerror %>% distinct() %>% 
                                                                filter (!is.na(row)) %>% 
                                                                pivot_wider(id_cols = !level, 
                                                                            names_from = field, 
                                                                            values_from = message, 
                                                                            values_fn = max) %>%
                                                                inner_join (Occurrence %>% mutate (row = row_number()), 
                                                                            by = "row", 
                                                                            suffix = c("_error", "")) %>% 
                                                                arrange(occurrenceID, scientificName) 
     
        }}}} 
  
  if (exists("eMoF")) {
    
    if(is.null(emoferror) == FALSE & nrow(emoferror) > 0) {
      if(is.null(emoferror$row) == FALSE) {
        if(nrow(emoferror %>% filter (!is.na(row))) > 0) {
    
     IPTreport$dtb$emoferror_table <- emoferror %>% distinct() %>% 
                                                    filter (!is.na(row)) %>% 
                                                    pivot_wider(id_cols = !level, 
                                                                names_from = field, 
                                                                values_from = message, 
                                                                values_fn = max) %>%
                                                    inner_join (eMoF %>% mutate (row = row_number()), 
                                                                by = "row", 
                                                                suffix = c("_error", "")) %>% 
                                                    arrange(eventID, row)
  } } } }
  
  
  
  ###### Creating the error_report tables: Invalid Event Records, Invalid Occurrence Records & Invalid eMoF Records 
  ######------------------------------------------------------------------------------------------------------------
  
  if (exists("Event")) {
      if(is.null(eventerror) == FALSE & nrow(eventerror) > 0) {
    
    eventerror_report <- eventerror  %>% distinct() %>% 
                                         mutate (message = (if_else(grepl("is greater than the value found in the bathymetry raster", message, fixed = TRUE),
                                                                    "Depth value is greater than the value found in the bathymetry raster" , as.character(message)))) %>%                         
                                         mutate (message = (if_else(grepl("does not seem to be a valid date", message, fixed = TRUE),
                                                                    "eventDate does not seem to be a valid date" , as.character(message)))) %>%
                                         mutate (message = (if_else(grepl("is greater than maximum", message, fixed = TRUE),
                                                                    "Minimum depth is greater than maximum depth" , as.character(message)))) %>% 
                                         mutate (message = (if_else(grepl("has no corresponding eventID", message, fixed = TRUE),
                                                                    "This parentEventID has no corresponding eventID" , as.character(message)))) %>% 
                                         select(-level) %>%
                                         group_by (field, message) %>% 
                                         summarize(count = n()) %>% 
                                         mutate (table = "event")
  }} 
  
  
  if(exists("Occurrence")){
  if(is.null(occurrenceerror) == FALSE & nrow (occurrenceerror) >0) {

    occurrenceerror_report <- occurrenceerror  %>% distinct() %>%
                                                   mutate (message = (if_else(grepl("is greater than the value found in the bathymetry raster", message, fixed = TRUE),
                                                                              "Depth value is greater than the value found in the bathymetry raster" , as.character(message)))) %>%                         
                                                   mutate (message = (if_else(grepl("does not seem to be a valid date", message, fixed = TRUE),
                                                                              "eventDate does not seem to be a valid date" , as.character(message)))) %>% 
                                                   mutate (message = (if_else(grepl("is greater than maximum", message, fixed = TRUE),
                                                                              "Minimum depth is greater than maximum depth" , as.character(message)))) %>% 
                                                   select(-level) %>%
                                                   group_by (field, message) %>% 
                                                   summarize(count = n()) %>%
                                                   mutate (table = "occurrence")
    
  }} else { # Warning for cases where occurrence table doesn't exist
    
    occurrenceerror_report <- bind_rows(if(exists("occurrenceerror")){occurrenceerror},
                                        data.frame(message = "The dataset does not seem to have an occurrence table",
                                                   count = 1,
                                                   table = "occurrence"))
  }
  
  if (exists("eMoF")) {
      if(is.null(emoferror) == FALSE & nrow(emoferror) > 0) {
    
    emoferror_report <- emoferror  %>% distinct() %>% 
                                       mutate(field = as.character(field), 
                                              message = as.character(message)) %>% 
                        mutate (message = (if_else(grepl("has no corresponding eventID in the core", message, fixed = TRUE),
                                                   "This eventID has no corresponding eventID in the core" , message))) %>%
                        select(-level) %>%
                        group_by (field, message) %>% 
                        summarize(count = n()) %>% 
                        mutate (table = "emof") 
  }}     
  
  
  if (is.null(IPTreport$ipt_url) == FALSE) {
    if(exists("metadataerror")){
      if(is.null(metadataerror) == FALSE) {
        metadataerror_report <- metadataerror %>% select(-level)
    }}}
  
  if (exists("dnaerror")) {
    if(is.null(dnaerror) == FALSE & nrow(dnaerror) > 0) {
      dnaerror_report <- dnaerror  %>% distinct() %>% 
        select(-level) %>%
        group_by (field, message) %>% 
        summarize(count = n()) %>% 
        mutate (table = "dna")
    }
  } 
  
  ###### Creating the general_issues table: Overview of all issues
  ######----------------------------------------------------------- 
    
  IPTreport$dtb$general_issues <- bind_rows(if(exists("eventerror_report")) eventerror_report, 
                                            if(exists("occurrenceerror_report")) occurrenceerror_report, 
                                            if(exists("emoferror_report")) emoferror_report,
                                            if(exists("metadataerror_report")) metadataerror_report,
                                            if(exists("dnaerror_report")) dnaerror_report)
  
  
  
  if (is.null(IPTreport$dtb$general_issues) == FALSE){
    if(nrow(IPTreport$dtb$general_issues) > 0){
      
      IPTreport$dtb$general_issues  <- IPTreport$dtb$general_issues %>% mutate (count = as.integer(count)) %>% 
                                                                        arrange(table, field, desc(count))
    } else {
      IPTreport$dtb$general_issues <- c ("There don't seem to be any issues")
    }} else {
      IPTreport$dtb$general_issues <- c ("There don't seem to be any issues")
    }
  
  
  return(IPTreport)
  
}

