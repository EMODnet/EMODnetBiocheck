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
  
  
  if ( ((grepl("archive?", file)==FALSE | url.exists(file) == FALSE & valid_url(file) == FALSE)) & grepl(".zip", file) == FALSE)
  {
    output$error <- ("Link does not resolve to a public IPT resource")
  }  else   
  {
  
  
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
    
      if(TRUE %in% (class(Event$eventDate) != "character")){
        Event$eventDate <- format_iso_8601(Event$eventDate)
      } # format to ISO 8601 if not character (can also use lubridate::format_ISO8601)
      
      Event<-cleandataframe(Event, vector = FALSE)
      output$Event <- fncols(Event, c("parentEventID"))
    }
    
    }
  
  if (is.null(out$data[["occurrence.txt"]]) == FALSE){
    Occurrence <-out$data[["occurrence.txt"]] 
    if (length(Occurrence) < 2 ){ rm(Occurrence)} else {
    output$Occurrence<-cleandataframe(Occurrence,  vector = FALSE)}}
  
    if (  exists("Occurrence") == FALSE  ) {   
    output$error <- ("The dataset does not have an occurrence file")
    }
  
  if (is.null(out$data[["extendedmeasurementorfact.txt"]]) == FALSE){
    eMoF <-out$data[["extendedmeasurementorfact.txt"]]  
      }
  
  if(exists("eMoF") == FALSE & is.null(out$data[["measurementorfact.txt"]]) == FALSE) { 
    eMoF <- out$data[["measurementorfact.txt"]] }
  
  if(exists("eMoF") ) { if (length(eMoF) > 2 ){
    
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
