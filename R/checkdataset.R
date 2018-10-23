checkdataset = function(Event = NULL, Occurrence = NULL, eMoF = NULL, IPTreport = list(), tree = FALSE){
  library(stringr)

  if (is.null(Event)) {rm(Event)}
  if (is.null(eMoF)) {rm(eMoF)}
  
  #---------------------------------------------------------------------------#
  ###                   Fix formatting                                        ####
  #---------------------------------------------------------------------------#      
  #### Occurrence fix
  
  Occurrence[Occurrence =='NA' | Occurrence =='' | Occurrence ==' '] <- NA
  Occurrence <- Occurrence[,colSums(is.na(Occurrence))<nrow(Occurrence)]
  #     Occurrence <- fncols(Occurrence, c("eventDate"))
  
  
  #### Event fix
  
  if (  exists("Event") == TRUE  )
  {
    Event[Event =='NA' | Event =='' | Event ==' '] <- NA
    Event <- Event[,colSums(is.na(Event))<nrow(Event)]
    Event <- fncols(Event, c("parentEventID"))
    #        Event[Event =='NA' | Event =='' | Event ==' '] <- NA
    
    #       Event <- fncols(Event, c("eventDate"))
  }
  
  #### MoF fix
  
  if ( exists("eMoF") == TRUE  )
  { eMoF[eMoF =='NA' | eMoF =='' | eMoF ==' '] <- NA
  eMoF <- eMoF[,colSums(is.na(eMoF))<nrow(eMoF)]
  eMoF <- fncols(eMoF, c("measurementTypeID","measurementValueID", "measurementValue", "measurementUnitID", "eventID", "measurementUnit"))
  #       eMoF[eMoF =='NA' | eMoF =='' | eMoF ==' '] <- NA
  
  if ( exists("Event") == TRUE){
    eMoF$eventID <- eMoF$id #eventID column is required in the measurements table.
  } else {
    eMoF$occurrenceID <- eMoF$id #occurrenceID column is required in the measurements table.
  }
  }
  
  
  
  #----------------------------------------------------------------------------#
  ####                    Generate summary                                  ####
  #----------------------------------------------------------------------------#
  
  occurrencetemp <- suppressWarnings(if(("occurrenceStatus" %in% names(Occurrence)) == FALSE) {mutate  (Occurrence, occurrenceStatus = "present")} else {Occurrence} %>% fncols ("basisOfRecord") %>%
                                       select (one_of(c("occurrenceStatus","basisOfRecord", "eventID"))))
  
  
  if (exists("Event")) {
    if("absent" %in% occurrencetemp$occurrenceStatus)  {     
      IPTreport$datasummary <- Event %>% fncols("type") %>% select (eventID, type) %>% left_join(occurrencetemp, by = "eventID") %>% 
        group_by(eventID, type, occurrenceStatus, basisOfRecord ) %>% summarise( occount = sum(!is.na(basisOfRecord))) %>%
        dcast (type + eventID + basisOfRecord ~occurrenceStatus, value.var=c("occount"), fun=(sum)) %>% fncols(c("absent", "NA", "present")) %>%
        mutate(absent = as.integer(absent)) %>% group_by(type, basisOfRecord) %>% 
        summarise(n_events = sum(!is.na(unique(eventID))), n_absent = sum(absent),  n_present = sum(present),n_NA = sum(NA)) %>%
        select(type,n_events, basisOfRecord, n_present, n_absent, n_NA )
    } else {
      IPTreport$datasummary <- Event %>% fncols("type") %>% select (eventID, type) %>% left_join(occurrencetemp, by = "eventID") %>% 
        mutate (occurrenceStatus = if_else(!is.na(occurrenceStatus), paste("n_", occurrenceStatus, sep ="") , occurrenceStatus )) %>%
        group_by(type, occurrenceStatus, basisOfRecord ) %>% 
        summarise( occount = sum(!is.na(basisOfRecord)), n_events = sum(!is.na(unique(eventID)))) %>%  
        mutate (occount = if_else(occount =="0", as.integer(NA), occount )) %>% 
        dcast (type + n_events + basisOfRecord ~occurrenceStatus, value.var=c("occount") )
    }
    IPTreport$datasummary[IPTreport$datasummary =='0' | IPTreport$datasummary  =='NA'] <- NA
    IPTreport$datasummary <- IPTreport$datasummary[,names(IPTreport$datasummary) == "type" |colSums(is.na(IPTreport$datasummary))<nrow(IPTreport$datasummary)] 
    
    
  } else {
    IPTreport$datasummary <- occurrencetemp   %>%   group_by(basisOfRecord, occurrenceStatus) %>% summarize(countOccurrence = n())        
  }
  
  if (exists("eMoF")) {
    
    if (exists("BODC")) {
      IPTreport$mofsummary <-  suppressWarnings(eMoF %>% mutate(type = if_else(is.na(occurrenceID) , "EventMoF", "OccurrenceMoF" ), measurementValue =  as.numeric(measurementValue)) %>% 
                                                  mutate (measurementTypeID = if_else(str_sub(measurementTypeID, -1, -1)=='/',measurementTypeID, paste(measurementTypeID, "/",  sep = '') )) %>%    
                                                  group_by(type, measurementType,  measurementTypeID, measurementUnit) %>% summarize(count = n(), minValue = min(measurementValue), maxValue = max(measurementValue) ) %>% ungroup() %>%
                                                  left_join(BODC$parameters, by = c("measurementTypeID"="uri")) %>%
                                                  select (type, measurementType, minValue,  maxValue, measurementUnit,count,   preflabel, definition))
      #IPTreport$mofsummary$minValue <- format(IPTreport$mofsummary$minValue, digits=2) 
      #IPTreport$mofsummary$maxValue <- format(IPTreport$mofsummary$maxValue, digits=2) 
      
      
      IPTreport$mofsummary_values <- eMoF %>% filter(!is.na(measurementValueID)) %>% mutate(type = if_else(is.na(occurrenceID) , "EventMoF", "OccurrenceMoF" )) %>% 
        mutate (measurementValueID = if_else(str_sub(measurementValueID, -1, -1)=='/',measurementValueID, paste(measurementValueID, "/",  sep = '') )) %>%    
        group_by(type, measurementType, measurementValue, measurementValueID) %>% summarize(count = n()) %>% ungroup() %>%
        left_join(BODC$values, by = c("measurementValueID"="uri")) %>%
        select (type, measurementType, measurementValue,preflabel, definition)
      
    } else {
      IPTreport$mofsummary <-  suppressWarnings(eMoF %>% mutate(type = if_else(is.na(occurrenceID) , "EventMoF", "OccurrenceMoF"),  measurementValue =  as.numeric(measurementValue))   %>% 
                                                  group_by(type, measurementType, measurementUnit) %>% summarize(count = n(), minValue = min(measurementValue), maxValue = max(measurementValue)) %>%
                                                  select (type, measurementType, minValue,  maxValue, measurementUnit,count))
      
    }
    
    if (is.data.frame(IPTreport$mofsummary)){if (nrow(IPTreport$mofsummary)>0){
      IPTreport$mofsummary  <- IPTreport$mofsummary %>% arrange(type , desc(count))
    }}
  }
  
  
  
  #-----------------------------------------------------------------------#
  ####                    Integrety Checks			                   ####
  #-----------------------------------------------------------------------#
  
  
  if ( exists("Event")){
    ev_check_id <- check_eventids(Event)  # Checks that all parenteventIDs linked to an eventID in the dataset
    oc.ev_check_id <- check_extension_eventids(Event,Occurrence) # Checks if all eventIDs in your occurrence extentsion  link to the event Core
    
    if (  exists("eMoF")){
      mof.ev_check_id <- check_extension_eventids(Event,eMoF)  # Checks the all eventIDs in your eMoF extentsion  link to the event Core
      
      mof.oc_check_id <- eMoF %>% mutate (level = 'error', field = 'occurrenceID', row = row_number(),
                                          message = 'This occurrenceID as no corresponding occurrenceID in the occurrence Extension') %>%
        filter (!is.na(occurrenceID)) %>%  anti_join(Occurrence, by = "occurrenceID")  %>%
        select (level, field, row, message) # Checks the all eMoF eventIDs linked to the same event as the related occurrence
      
      
      mof.oc.ev_check_id <- eMoF %>% mutate (level = 'error', field = 'eventID', row = row_number(),
                                             message = 'This eventID differs from the eventID provided in the related Occurrence') %>%
        inner_join(Occurrence, by = "occurrenceID") %>% anti_join(eMoF, by = c(  "eventID.y"=  "eventID", "occurrenceID" = "occurrenceID")) %>%
        select (level, field, row, message) # Checks the all eMoF eventIDs linked to the same event as the related occurrence
    }
  }
  
  if ( exists("Event") == FALSE &  exists("eMoF") == TRUE  ){
    mof.oc_check_id <- eMoF %>% mutate (level = 'error', field = 'occurrenceID', row = row_number(),
                                        message = 'This occurrenceID as no corresponding occurrenceID in the occurrence Extension') %>%
      filter (!is.na(occurrenceID)) %>%  anti_join(Occurrence, by = "occurrenceID")  %>%
      select (level, field, row, message) # Checks the all eMoF eventIDs linked to the same event as the related occurrence
    
  }    
  
  
  emoferror <- rbind(if(exists("mof.ev_check_id")) mof.ev_check_id, if(exists("mof.oc_check_id")) mof.oc_check_id,
                     if(exists("mof.oc.ev_check_id")) mof.oc.ev_check_id, if(exists("mof.oc_check_id")) mof.oc_check_id)
  
  occurrenceerror <- rbind(if(exists("oc.ev_check_id")) oc.ev_check_id)
  
  eventerror <- rbind(if(exists("ev_check_id")) ev_check_id)
  
  
  #-----------------------------------------------------------------------#
  ####                    Tree structure                               ####
  #-----------------------------------------------------------------------#
  
  if ( exists("Event") &  exists("eMoF")  & tree == TRUE) {if (   nrow(ev_check_id) == 0) {
    IPTreport$tree <- treeStructure(Event, Occurrence, eMoF)
  }}
  
  
  
  #-----------------------------------------------------------------------#
  ####                    measurementsorfacts                          ####
  #-----------------------------------------------------------------------#
  
  if (  exists("eMoF")){
    mof_noUnit <- c('N', 'NA', 'Dimensionless', 'Dmnless', '', ' ') # add values to the list if there refer to 'no unit'
    
    mof_oc_noTypeID <- eMoF %>% filter (!is.na(occurrenceID), is.na(measurementTypeID) ) %>% select (measurementType, measurementUnit) %>% mutate(IDlink = 'occurrence', message = 'measurementtypeID is missing') %>% group_by (IDlink,measurementType, measurementUnit, message) %>% summarize(count = n())
    
    mof_noValueID <- eMoF %>%
      filter ((is.na(measurementUnit) | (measurementUnit %in% mof_noUnit)  | measurementUnitID == "http://vocab.nerc.ac.uk/collection/P06/current/UUUU/") &
                is.na(measurementValueID) & (!measurementType %in% c("count") & !measurementTypeID %in% "http://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01/")) %>%
      mutate(IDlink = if_else(!is.na(occurrenceID),"occurrence", "event") ) %>%
      mutate(message = 'measurementValues which may need a measurementValueID') %>%  group_by (IDlink, measurementType, measurementValue, message) %>% summarize(count = n()) %>% arrange (desc(measurementType))
    
    
    if (sum(grepl(BODC$instrument, unique(eMoF$measurementTypeID)))== 0){
      mof_noInstrument <- data.frame(level = c('warning'),field = c('measurementType'), row = NA, 
                                     message = c('No sampling instrument present'))
    }
    
    if (sum(grepl(paste(BODC$effort, collapse="|"), unique(eMoF$measurementTypeID)))== 0){
      mof_noSamplingdescriptor <- data.frame(level = c('warning'),field = c('measurementType'), row = NA, 
                                             message = c('No sampling descriptors present: see http://vocab.nerc.ac.uk/collection/Q01/current/'))
    }
    
    
    if (  exists("BODC")){ 
      mof_oc_TypeID_NotResolve <- eMoF %>% filter (!is.na(occurrenceID), !is.na(measurementTypeID) ) %>% 
        select (measurementType, measurementTypeID, measurementUnit) %>% 
        mutate (measurementTypeID = if_else(str_sub(measurementTypeID, -1, -1)=='/',measurementTypeID, paste(measurementTypeID, "/",  sep = '') )) %>%
        anti_join(BODC$parameters, by = c("measurementTypeID"="uri")) %>%
        mutate(IDlink = 'occurrence', message = 'measurementTypeID does not resolve') %>% 
        group_by (IDlink,measurementType, measurementTypeID, measurementUnit, message) %>% summarize(count = n())
      
      mof_oc_ValueID_NotResolve <- eMoF %>% 
        filter (!is.na(occurrenceID), !is.na(measurementValueID) ) %>% 
        mutate (measurementValueID = if_else(str_sub(measurementValueID, -1, -1)=='/',measurementValueID, paste(measurementValueID, "/",  sep = '') )) %>%
        select (measurementValue, measurementValueID) %>% 
        anti_join(BODC$values, by = c("measurementValueID"="uri")) %>%
        mutate(IDlink = 'occurrence', message = 'measurementValueID does not resolve') %>% 
        group_by (IDlink,measurementValue, measurementValueID, message) %>% summarize(count = n())
    }
    
    
    mof_ValueNull <- eMoF %>%
      mutate (level = 'error', field = 'measurementValue', row = row_number(),
              message = 'MeasurmentValue of Null') %>%
      filter (is.na(measurementValue))  %>% select (level,field, row ,message)
    
    mof_oc_Value0 <- suppressWarnings(eMoF %>%
                                        mutate (level = 'error', field = 'measurementValue', row = row_number(),
                                                message = 'Biological value of 0 while Occurrencestatus is present') %>%
                                        inner_join (fncols(Occurrence, c("occurrenceStatus")) %>%  filter (is.na(occurrenceStatus) | occurrenceStatus == 'present' ) %>% select (occurrenceID), by = 'occurrenceID') %>%
                                        mutate(measurementValue = as.numeric(measurementValue)) %>% filter ( measurementValue == 0 ) %>%  select (level,field, row ,message))
    
    mof_oc_dubs <- eMoF %>% filter (!is.na(occurrenceID), !is.na(measurementTypeID)) %>%
      select (occurrenceID, measurementTypeID) %>% group_by (occurrenceID, measurementTypeID) %>% summarize(count = n())  %>% filter (count >1 )  %>%
      inner_join((eMoF) %>% mutate (row = row_number()), by = c("occurrenceID", "measurementTypeID") ) %>% ungroup() %>%
      mutate (field = 'measurementType', level = 'error',  message = 'Duplicate eMoF record linked to occurrence') %>%
      select (level,field, row ,message)
    
    
    if (  exists("Event") ){
      mof_ev_dubs <- eMoF %>% filter (is.na(occurrenceID), !is.na(measurementTypeID)) %>%
        select (eventID, measurementTypeID) %>% group_by (eventID, measurementTypeID) %>% summarize(count = n())  %>% filter (count >1 )  %>%
        inner_join((eMoF) %>% mutate (row = row_number()), by = c("eventID", "measurementTypeID") )  %>% ungroup() %>%
        mutate (field = 'measurementType', level = 'error',  message = 'Duplicate eMoF record linked to event') %>%
        select (level,field, row ,message)
      
      
      mof_ev_noTypeID <- eMoF %>% filter (!is.na(eventID), is.na(occurrenceID), is.na(measurementTypeID) ) %>% 
        select (measurementType, measurementUnit) %>% 
        mutate(IDlink = 'event', message = 'measurementtypeID is missing') %>% 
        group_by (IDlink,measurementType, measurementUnit, message) %>% summarize(count = n())
      
      if (  exists("BODC")){      
        mof_ev_TypeID_NotResolve <- eMoF %>% filter (!is.na(eventID), is.na(occurrenceID),  !is.na(measurementTypeID) ) %>% 
          select (measurementType, measurementTypeID, measurementUnit) %>% 
          mutate (measurementTypeID = if_else(str_sub(measurementTypeID, -1, -1)=='/',measurementTypeID, paste(measurementTypeID, "/",  sep = '') )) %>%
          anti_join(BODC$parameters, by = c("measurementTypeID"="uri")) %>%
          mutate(IDlink = 'event', message = 'measurementTypeID does not resolve') %>% 
          group_by (IDlink,measurementType, measurementTypeID, measurementUnit, message) %>% summarize(count = n())
        
        
        mof_ev_ValueID_NotResolve <- eMoF %>% 
          filter (!is.na(eventID), is.na(occurrenceID), !is.na(measurementValueID) ) %>% 
          mutate (measurementValueID = if_else(str_sub(measurementValueID, -1, -1)=='/',measurementValueID, paste(measurementValueID, "/",  sep = '') )) %>%
          select (measurementValue, measurementValueID) %>% 
          anti_join(BODC$values, by = c("measurementValueID"="uri")) %>%
          mutate(IDlink = 'event', message = 'measurementValueID does not resolve') %>% 
          group_by (IDlink,measurementValue, measurementValueID, message) %>% summarize(count = n())
      }}
    
    
    IPTreport$dtb$mof_issues <- suppressWarnings(rbind(mof_oc_noTypeID, mof_noValueID,
                                                       if(exists("mof_oc_TypeID_NotResolve")) mof_oc_TypeID_NotResolve, 
                                                       if(exists("mof_oc_ValueID_NotResolve")) mof_oc_ValueID_NotResolve,
                                                       if(exists("mof_ev_noTypeID")) mof_ev_noTypeID, 
                                                       if(exists("mof_ev_TypeID_NotResolve"))  mof_ev_TypeID_NotResolve,
                                                       if(exists("mof_ev_ValueID_NotResolve"))  mof_ev_ValueID_NotResolve ) %>%
                                                   select (one_of(c("IDlink" , "measurementType", "measurementTypeID", "measurementValue", 
                                                                    "measurementValueID",  "measurementUnit" , "message" , "count"))))
    
    if (is.data.frame(IPTreport$dtb$mof_issues)){if (nrow(IPTreport$dtb$mof_issues)>0){
      IPTreport$dtb$mof_issues <- IPTreport$dtb$mof_issues[,colSums(is.na(IPTreport$dtb$mof_issues))<nrow(IPTreport$dtb$mof_issues)] %>% arrange(IDlink , message, desc(count))
    }}
    
    emoferror <- rbind(emoferror, mof_ValueNull, mof_oc_Value0, mof_oc_dubs, if(exists("mof_ev_dubs")) mof_ev_dubs,
                       if(exists("mof_noInstrument")) mof_noInstrument,  
                       if(exists("mof_noSamplingdescriptor")) mof_noSamplingdescriptor
    )
    
  }
  
  #-----------------------------------------------------------------------#
  ####                    QC checks                                    ####
  #-----------------------------------------------------------------------#
  
  
  
  # QC checks - Check if all required fields are there  ---------------------------------
  
  if (  exists("Event") ){ if ( nrow(ev_check_id) == 0) {
    
    ev_flat <- flatten_event(Event) %>% filter (!eventID %in% Event$parentEventID)
    
    ev_CheckFields <- check_fields(ev_flat, level = "warning") %>% filter (field %in% event_fields())
    oc_CheckFields <- check_fields(Occurrence, level = "warning") %>% filter (!field %in% event_fields())
  } else
  {
    ev_flat <- Event %>% filter (!eventID %in% Event$parentEventID)
    
    ev_CheckFields <- check_fields(ev_flat, level = "warning") %>% filter (field %in% event_fields())
    oc_CheckFields <- check_fields(Occurrence, level = "warning") %>% filter (!field %in% event_fields())
    
    
  } 
    if ( nrow(ev_CheckFields) > 0) {
      ev_CheckFields <- Event  %>% select(eventID) %>% mutate (rownew = row_number()) %>% inner_join(ev_flat %>% select(eventID) %>% 
                                                                                                       mutate (row = row_number()), by = "eventID") %>%
        inner_join(ev_CheckFields, by="row") %>% select (-eventID, -row) %>% select (row=rownew) 
      
    }} else  {
      oc_CheckFields <- check_fields(Occurrence, level = "warning")
    }
  
  
  occurrenceerror <- rbind(occurrenceerror, if(exists("oc_CheckFields")) oc_CheckFields)
  eventerror <- rbind(eventerror, if(exists("ev_CheckFields")) ev_CheckFields)
  
  
  
  
  # QC checks - Check coordinates  ---------------------------------
  
  if (  exists("Event")) {
    GoodCords <- suppressWarnings(ev_flat %>% fncols(c("minimumDepthInMeters", "maximumDepthInMeters", "coordinateUncertaintyInMeters")) %>% 
                                    select (-coordinateUncertaintyInMeters) %>%
                                    filter (decimalLatitude < 90, decimalLatitude > -90, !is.na(decimalLatitude),
                                            decimalLongitude < 180, decimalLongitude > -180, !is.na(decimalLongitude)) %>%
                                    inner_join(Event %>% select (eventID, one_of(c("coordinateUncertaintyInMeters"))), by = "eventID") %>%  
                                    select (eventID,decimalLatitude, decimalLongitude, one_of (c("coordinateUncertaintyInMeters")), minimumDepthInMeters, maximumDepthInMeters)) 
    
    
    OnLand <- suppressWarnings(check_onland(GoodCords, buffer = 3000))
    depth <- suppressWarnings(check_depth(GoodCords %>% filter (!is.na(minimumDepthInMeters) | !is.na(maximumDepthInMeters)), depthmargin = 150))
    
    Cords00_rep <- Event %>% mutate (level = 'error', field = 'coordinates_error', row = row_number(),
                                     message = 'decimalLatitude and decimalLongitude are both 0') %>%
      filter (decimalLatitude ==0 , decimalLongitude == 0) %>% select (level, field, row, message)
    
    
    plot_coordinates <- suppressWarnings(GoodCords %>% mutate (quality = if_else ((eventID %in% OnLand$eventID), 'On Land', 
                                                                                  if_else ((eventID %in% depth$eventID), 'Check Depth',
                                                                                           if_else ((decimalLatitude ==0 & decimalLongitude == 0), '0-0 Coordinates',        
                                                                                                    'OK')))) %>% 
                                           select (eventID, decimalLatitude, decimalLongitude, one_of (c("coordinateUncertaintyInMeters")), quality) %>% distinct())
    
    
    
    depth_rep <- check_depth(GoodCords, report=T, depthmargin = 150)
    OnLand_rep <- check_onland(GoodCords,report=T, buffer = 3000) %>% mutate (field = 'coordinates_error')
    
    goodcord_rep <- GoodCords %>% select(eventID) %>% mutate (row = row_number()) %>% 
      inner_join(rbind(depth_rep, OnLand_rep), by=c("row")) %>% select (-row)
    
    coord_rep <- Event %>% select(eventID) %>% mutate (row = row_number()) %>% 
      inner_join(goodcord_rep, by=c("eventID")) %>% select (-eventID)
    
    
    eventerror <- bind_rows(eventerror, coord_rep, Cords00_rep)
    
  } else {
    
    GoodCords <- suppressWarnings(Occurrence %>%fncols(c("minimumDepthInMeters", "maximumDepthInMeters"))  %>% 
                                    filter (decimalLatitude < 90, decimalLatitude > -90, !is.na(decimalLatitude),
                                            decimalLongitude < 180, decimalLongitude > -180, !is.na(decimalLongitude)) %>%
                                    select (occurrenceID,decimalLatitude, decimalLongitude, one_of (c("coordinateUncertaintyInMeters")), minimumDepthInMeters, maximumDepthInMeters))
    
    OnLand <- suppressWarnings(check_onland(GoodCords, buffer = 3000))
    depth <- suppressWarnings(check_depth(GoodCords %>% filter (is.na(minimumDepthInMeters) | is.na(maximumDepthInMeters)), depthmargin = 150))
    
    plot_coordinates <- suppressWarnings(GoodCords %>% mutate (quality = if_else ((occurrenceID %in% OnLand$occurrenceID), 'On Land', 
                                                                                  if_else ((occurrenceID %in% depth$occurrenceID), 'Check Depth',
                                                                                           if_else ((decimalLatitude ==0 & decimalLongitude == 0), '0-0 Coordinates',
                                                                                                    'OK')))) %>% 
                                           select (occurrenceID, decimalLatitude, decimalLongitude,one_of (c("coordinateUncertaintyInMeters")), quality ) %>% distinct())
    
    
    Cords00_rep <- Occurrence %>% mutate (level = 'error', field = 'coordinates', row = row_number(),
                                          message = 'decimalLatitude and decimalLongitude are both 0') %>%
      filter (decimalLatitude ==0 , decimalLongitude == 0) %>% select (level, field, row, message)
    
    depth_rep <- check_depth(GoodCords, report=T, depthmargin = 150)
    OnLand_rep <- check_onland(GoodCords,report=T, buffer = 3000) %>% mutate (field = 'coordinates_error')
    
    goodcord_rep <- GoodCords %>% select(occurrenceID) %>% mutate (row = row_number()) %>% 
      inner_join(rbind(depth_rep, OnLand_rep), by=c("row")) %>% select (-row)
    
    coord_rep <- Occurrence %>% select(occurrenceID) %>% mutate (row = row_number()) %>% 
      inner_join(goodcord_rep, by=c("occurrenceID")) %>% select (-occurrenceID)
    
    
    occurrenceerror <- bind_rows(occurrenceerror, coord_rep, Cords00_rep)  
  }   
  
  IPTreport$plot_coordinates <- plot_coordinates
  
  
  
  
  # QC checks - Check dates against the ISO format  ---------------------------------
  if (  exists("Event") ) {
    
    date_rep <- check_eventdate(Event %>% fncols(c("eventDate"))) %>% filter (message !='eventDate NA does not seem to be a valid date')
    dates_plot <- Event %>% fncols(c("eventDate")) %>% select (eventDate) %>% filter (!is.na(eventDate)) %>% mutate (year = as.numeric((substr(eventDate, 1,4))))
    
    eventerror <- rbind(eventerror, date_rep)  
    
    
  } else {
    
    date_rep <- check_eventdate(Occurrence %>% fncols(c("eventDate")))  %>% filter (message !='eventDate NA does not seem to be a valid date')
    dates_plot <- Occurrence %>% fncols(c("eventDate")) %>% select (eventDate) %>% filter (!is.na(eventDate)) %>% mutate (year = as.numeric((substr(eventDate, 1,4))))
    
    occurrenceerror <- rbind(occurrenceerror, date_rep)  
  } 
  
  IPTreport$dates_plot <- dates_plot  
  
  
  
  
  # QC checks potential duplicate occurrences  ---------------------------------
  #with emof
  if (  exists("eMoF")  ){
    mof_biometric <- eMoF %>% filter (!is.na(occurrenceID), measurementTypeID %in% (BODC$biometrics) ) 
    
    if (exists("Event") ) {
      
      duplicatescheck <- c ('eventID', 'lifeStage', 'sex', 'scientificName',  'scientificNameID', 'identificationRemarks',
                            'identificationQualifier') # terms to check in case of event Core
      
      if (nrow(mof_biometric) > 0 ) {
        
        mof_biometric <-  mof_biometric %>% dcast(occurrenceID  ~ measurementType, value.var=c("measurementValue"))
        biometricterms <- names(mof_biometric %>% select (-occurrenceID))
        duplicatescheck <- c(duplicatescheck, biometricterms)
        
        occ_dups_rows <- (fncols((Occurrence %>% left_join(mof_biometric, by = "occurrenceID")), duplicatescheck)  %>% select (duplicatescheck) %>% duplicated () +
                            fncols((Occurrence %>% left_join(mof_biometric, by = "occurrenceID")), duplicatescheck)  %>% select (duplicatescheck) %>% duplicated (fromLast=TRUE))
      } else {
        occ_dups_rows <- (fncols(Occurrence, duplicatescheck)  %>% select (duplicatescheck) %>% duplicated ()
                          +  fncols(Occurrence, duplicatescheck)  %>% select (duplicatescheck) %>% duplicated (fromLast=TRUE) )
        
      }} else  {
        duplicatescheck <- c ("decimalLatitude", "decimalLongitude", 'eventDate', 'eventTime', 'minimumDepthInMeters', 'maximumDepthInMeters'
                              , 'eventID', 'lifeStage', 'sex', 'samplingProtocol', 'scientificName',  'scientificNameID', 'identificationRemarks',
                              'identificationQualifier') # terms to check in case of occurrence Core
        
        if (  nrow(mof_biometric) > 0  )    {
          mof_biometric <-  mof_biometric %>% dcast(occurrenceID  ~ measurementType, value.var=c("measurementValue"))
          biometricterms <- names(mof_biometric %>% select (-occurrenceID))
          duplicatescheck <- c(duplicatescheck, biometricterms)
          
          occ_dups_rows <- (fncols((Occurrence %>% left_join(mof_biometric, by = "occurrenceID")), duplicatescheck)  %>% select (duplicatescheck) %>% duplicated () +
                              fncols((Occurrence %>% left_join(mof_biometric, by = "occurrenceID")), duplicatescheck)  %>% select (duplicatescheck) %>% duplicated (fromLast=TRUE))
        } else {
          occ_dups_rows <- (fncols(Occurrence, duplicatescheck)  %>% select (duplicatescheck) %>% duplicated ()
                            + fncols(Occurrence, duplicatescheck)  %>% select (duplicatescheck) %>% duplicated (fromLast=TRUE) )
          
        }}} else {
          #without emof
          
          if (exists("Event")) {
            duplicatescheck <- c ('eventID', 'lifeStage', 'sex', 'scientificName',  'scientificNameID', 'identificationRemarks',
                                  'identificationQualifier') # terms to check in case of event Core
            
            occ_dups_rows <-(fncols((Occurrence %>% left_join(mof_biometric, by = "occurrenceID")), duplicatescheck)  %>% select (duplicatescheck) %>% duplicated ()
                             + fncols((Occurrence %>% left_join(mof_biometric, by = "occurrenceID")), duplicatescheck)  %>% select (duplicatescheck) %>% duplicated (fromLast=TRUE) )
          } else {
            duplicatescheck <- c ("decimalLatitude", "decimalLongitude", 'eventDate', 'eventTime', 'minimumDepthInMeters', 'maximumDepthInMeters'
                                  , 'eventID', 'lifeStage', 'sex', 'samplingProtocol', 'scientificName',  'scientificNameID', 'identificationRemarks',
                                  'identificationQualifier') # terms to check in case of occurrence Core
            
            occ_dups_rows <- (fncols(Occurrence, duplicatescheck)  %>% select (duplicatescheck) %>% duplicated () +
                                fncols(Occurrence, duplicatescheck)  %>% select (duplicatescheck) %>% duplicated (fromLast=TRUE))
            
          }}
  
  occ_dups_rows <- occ_dups_rows > 0
  
  
  occ_dups <- (Occurrence %>% mutate (row = row_number()))[occ_dups_rows,] %>%
    mutate (field = 'occurrence_error', level = 'warning',  message = 'Potential duplicate record') %>%
    select (level,field, row ,message)
  
  
  occurrenceerror <- rbind(occurrenceerror, occ_dups)  
  
  
  
  
  #-------------------------------------------------------------------------------#
  ####                    Generate report table                                ####
  #-------------------------------------------------------------------------------#
  
  #Generate error tables
  
  if (exists("Event")) {if(is.null(eventerror) == FALSE & nrow(eventerror %>% filter (!is.na(row))) > 0){
    IPTreport$dtb$eventerror_table <- suppressWarnings(eventerror %>% distinct() %>% filter (!is.na(row)) %>% dcast(row ~ field, value.var=c("message"), fun=max) %>%
                                                         inner_join (Event %>% mutate (row = row_number()), by = "row", suffix = c("_error", "")) ) 
  } }
  
  if(is.null(occurrenceerror) == FALSE  & nrow (occurrenceerror %>% filter (!is.na(row))) >0 ) {
    IPTreport$dtb$occurrenceerror_table <- suppressWarnings(occurrenceerror %>% distinct() %>% filter (!is.na(row)) %>% dcast(row ~ field, value.var=c("message"), fun=max) %>%
                                                              inner_join (Occurrence %>% mutate (row = row_number()), by = "row", suffix = c("_error", "")))
  }
  
  if (exists("eMoF")) {if(is.null(emoferror) ==FALSE & nrow(emoferror %>% filter (!is.na(row))) > 0) {
    IPTreport$dtb$emoferror_table <- emoferror %>% distinct() %>% filter (!is.na(row)) %>% dcast(row ~ field, value.var=c("message")) %>%
      inner_join (eMoF %>% mutate (row = row_number()), by = "row", suffix = c("_error", ""))
  } }
  
  
  
  #Generate all_report_issues
  
  if (exists("Event")) {if(is.null(eventerror) ==FALSE) {
    eventerror_report <- eventerror  %>% distinct() %>% 
      mutate (message = (if_else(grepl("is greater than the value found in the bathymetry raster", message, fixed = TRUE),
                                 "Depth value is greater than the value found in the bathymetry raster" , message))) %>%                         
      mutate (message = (if_else(grepl("does not seem to be a valid date", message, fixed = TRUE),
                                 "eventDate does not seem to be a valid date" , message))) %>%
      mutate (message = (if_else(grepl("is greater than maximum", message, fixed = TRUE),
                                 "Minimum depth is greater than maximum depth" , message))) %>% 
      group_by (field, message) %>% summarize(count = n()) %>% 
      mutate (table = "event")
  }} 
  
  if(is.null(occurrenceerror) == FALSE & nrow (occurrenceerror) >0) {
    occurrenceerror_report <- occurrenceerror  %>% distinct() %>% 
      mutate (message = (if_else(grepl("is greater than the value found in the bathymetry raster", message, fixed = TRUE),
                                 "Depth value is greater than the value found in the bathymetry raster" , message))) %>%                         
      mutate (message = (if_else(grepl("does not seem to be a valid date", message, fixed = TRUE),
                                 "eventDate does not seem to be a valid date" , message))) %>% 
      mutate (message = (if_else(grepl("is greater than maximum", message, fixed = TRUE),
                                 "Minimum depth is greater than maximum depth" , message))) %>% 
      group_by (field, message) %>% summarize(count = n()) %>% 
      mutate (table = "occurrrence")
  }
  
  if (exists("eMoF")) {if(is.null(emoferror) ==FALSE) {
    emoferror_report <- emoferror  %>% distinct() %>% 
      group_by (field, message) %>% summarize(count = n()) %>% 
      mutate (table = "emof")
  }}     
  
  IPTreport$dtb$general_issues <- rbind(if(exists("eventerror_report")) eventerror_report 
                                        , if(exists("occurrenceerror_report")) occurrenceerror_report, if(exists("emoferror_report")) emoferror_report )
  
  
  
  if (is.null(IPTreport$dtb$general_issues) == TRUE ){
    IPTreport$dtb$general_issues <- c ("there don't seem to be any issues")} else {
      IPTreport$dtb$general_issues  <- IPTreport$dtb$general_issues %>% arrange(table , field, desc(count))
      
    }
  
  
  return(IPTreport)

}
