#' import IPT datasets 
#'
#' imports and cleans IPT datasets
#' @param file mandatory a link the DWC-A file (URL to IPT or a zip file)
#' @import parsedate
#' @export
#' @examples
#' output <- importiptdata("http://ipt.vliz.be/training/archive?r=biofun_2009")


importiptdata <- function (file){ 
  output<-list()
  
  if (grepl("resource?", file)==TRUE) {
    file <- gsub("resource?", "archive", file) }
  
  
  if ( ((grepl("archive?", file)==FALSE | url.exists(file) == FALSE & valid_url(file) == FALSE)) & grepl(".zip", file) == FALSE){
    output$error <- ("Link does not resolve to a public IPT resource")
  }  else   {
  
  
dwca_cache$delete_all()

tryCatch(
  {out <- dwca_read(file, read = TRUE)}, 
  error=function(x) { print("Link does not resolve to a public IPT resource")}
)

if (exists("out") == FALSE) {
    output$error <- "Link  does not resolve to a public IPT resource"    
  } else  { 
  
  output$name <- names(NamesToVector(file))
  output$title <- out$emlmeta$dataset$title$title[[1]]@.Data
  
  if (length(out$emlmeta$dataset$coverage$temporalCoverage)>0) {
    for (i in 1:length(out$emlmeta$dataset$coverage$temporalCoverage)) {
      output$startdate <- c(output$startdate, out$emlmeta$dataset$coverage$temporalCoverage[[1]]$beginDate$calendarDate@.Data)
      output$enddate <- c(output$enddate, out$emlmeta$dataset$coverage$temporalCoverage[[i]]$endDate$calendarDate@.Data)
    }
    if(length(output$startdate)>0) {output$startdate <- min(output$startdate)}
    if(length(output$enddate)>0) {output$enddate <- max(output$enddate)}
  }
  
  if (is.null(out$data[["event.txt"]]) == FALSE){
    Event <-out$data[["event.txt"]]
    
    if (length(Event) >1 ) {
      
      if("eventDate" %in% colnames(Event)){
        if(TRUE %in% (class(Event$eventDate) != "character")){
          if(class(Event$eventDate) == "integer"){
            Event$eventDate <- as.character(Event$eventDate) 
          } else {          
            Event$eventDate <- format_iso_8601(Event$eventDate)
        }} # format to ISO 8601 if not character (can also use lubridate::format_ISO8601)
      }
      if("modified" %in% colnames(Event)){
        if(TRUE %in% (class(Event$modified) != "character")){
          Event$modified <- format_iso_8601(Event$modified)
        }
      } # format to ISO 8601 if not character 
      
      if("verbatimEventDate" %in% colnames(Event)){
        if(TRUE %in% (class(Event$verbatimEventDate) != "character")){
          Event$verbatimEventDate <- format_iso_8601(Event$verbatimEventDate)
        }
      } # format to ISO 8601 if not character
      
      Event<-cleandataframe(Event, vector = FALSE)
      output$Event <- fncols(Event, c("parentEventID", "eventDate"))
    }
    
    }
  
  if (is.null(out$data[["occurrence.txt"]]) == FALSE){
    Occurrence <-out$data[["occurrence.txt"]] 
    
    if (length(Occurrence) >1 ) {
      
      if("modified" %in% colnames(Occurrence)){
        if(TRUE %in% (class(Occurrence$modified) != "character")){
          Occurrence$modified <- format_iso_8601(Occurrence$modified)
          }
        } # format to ISO 8601 if not character 
      
      if("dateIdentified" %in% colnames(Occurrence)){
        if(TRUE %in% (class(Occurrence$dateIdentified) != "character")){
          Occurrence$dateIdentified <- format_iso_8601(Occurrence$dateIdentified)
        }
      } # format to ISO 8601 if not character 
      
      if("verbatimEventDate" %in% colnames(Occurrence)){
        if(TRUE %in% (class(Occurrence$verbatimEventDate) != "character")){
          Occurrence$verbatimEventDate <- format_iso_8601(Occurrence$verbatimEventDate)
        }
      } # format to ISO 8601 if not character
        
      if("eventDate" %in% colnames(Occurrence)){
        if(TRUE %in% (class(Occurrence$eventDate) != "character")){
          if(class(Event$eventDate) == "integer"){
            Occurrence$eventDate <- as.character(Occurrence$eventDate) 
          } else { 
            Occurrence$eventDate <- format_iso_8601(Occurrence$eventDate)
          }}
        } # format to ISO 8601 if not character 
    
    if (length(Occurrence) < 2 ){ rm(Occurrence)} else {
    output$Occurrence<-cleandataframe(Occurrence,  vector = FALSE)}}}
  
    if (  exists("Occurrence") == FALSE  ) {   
    output$error <- ("The dataset does not have an occurrence file")
    }
  
  if (is.null(out$data[["extendedmeasurementorfact.txt"]]) == FALSE){
    eMoF <-out$data[["extendedmeasurementorfact.txt"]]  
      }
  
  if(exists("eMoF") == FALSE & is.null(out$data[["measurementorfact.txt"]]) == FALSE) { 
    eMoF <- out$data[["measurementorfact.txt"]] }
  
  if(exists("eMoF") ) { 
    if (length(eMoF) > 2 ){
    
      if("measurementDeterminedDate" %in% colnames(eMoF)){
        if(TRUE %in% (class(eMoF$measurementDeterminedDate) != "character")){
          eMoF$measurementDeterminedDate <- format_iso_8601(eMoF$measurementDeterminedDate)
        }
      } # format to ISO 8601 if not character
      
      eMoF<-cleandataframe(eMoF,  vector = FALSE)
      eMoF <- cleanemof(eMoF) 
  
      if ( exists("Event") == TRUE){
        eMoF$eventID <- eMoF$id #eventID column is required in the measurements table.
      } else {
        eMoF$occurrenceID <- eMoF$id #occurrenceID column is required in the measurements table.
    
  }
      output$eMoF <- eMoF
  }}
  
  
} }
  return (output)
}
