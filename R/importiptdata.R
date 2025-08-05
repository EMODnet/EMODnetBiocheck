#' import IPT datasets 
#'
#' imports and cleans IPT datasets
#' @param file mandatory a link the DWC-A file (URL to IPT or a zip file)
#' @import parsedate
#' @export
#' @examples
#' \dontrun{
#' output <- importiptdata("http://ipt.vliz.be/training/archive?r=biofun_2009")
#' }


importiptdata <- function (file){ 
  output<-list()
  
  #trims whitespaces
  file <- trimws(file)
  
  # replaces resource by archive to donwload the dwca
  if (grepl("resource?", file)==TRUE) {
    file <- gsub("resource?", "archive", file) }
  
  # accounts for URLs in edit mode
   if (grepl("manage/", file)==TRUE) {
     file <- gsub("manage/", "", file) }
  
  
  if (grepl(".zip", file) == FALSE && ((grepl("archive?", file)==FALSE | url.exists(file) == FALSE & valid_url(file) == FALSE))){
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
  output$title <- tryCatch(out$emlmeta$dataset$title$title[[1]]@.Data, 
                           error = function(e) {
                             out$emlmeta$dataset$title
                           }
  )
  output$ipt_url <- file
  
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
    
    if (length(Event) == 1 & nrow(Event) != 0){
      Event <- txt_to_cols(Event)
    }
    
    if (length(Event) >1 ) {
      
      Event<-cleandataframe(Event, vector = FALSE)
      output$Event <- fncols(Event, c("parentEventID", "eventDate"))
    }
    
    
    }
  
  if (is.null(out$data[["occurrence.txt"]]) == FALSE){
    Occurrence <-out$data[["occurrence.txt"]] 
    
    if (length(Occurrence) == 1 & nrow(Occurrence) != 0){
      Occurrence <- txt_to_cols(Occurrence)
     }
    
    if (length(Occurrence) >1 ) {
      
      if (length(Occurrence) < 2 ){ rm(Occurrence)
        } else {
          output$Occurrence<-cleandataframe(Occurrence,  vector = FALSE)}}
    
    }
  
    if (  exists("Occurrence") == FALSE  ) {   
    output$warning <- ("The dataset does not have an occurrence file")
    }
  
  if (is.null(out$data[["dnaderiveddata.txt"]]) == FALSE){
    output$DNA <-out$data[["dnaderiveddata.txt"]]  
  }
  else if (is.null(out$data[["dna.txt"]]) == FALSE){
    output$DNA <-out$data[["dna.txt"]]  
  }
  
  
  if (is.null(out$data[["extendedmeasurementorfact.txt"]]) == FALSE){
    eMoF <-out$data[["extendedmeasurementorfact.txt"]]  
      }
  
  if(exists("eMoF") == FALSE & is.null(out$data[["measurementorfact.txt"]]) == FALSE) { 
    eMoF <- out$data[["measurementorfact.txt"]] }
  
  if(exists("eMoF") ) {
    
    if (length(eMoF) == 1 & nrow(eMoF) != 0){
      eMoF <- txt_to_cols(eMoF)
    }
    
    if (length(eMoF) > 2 ){
    
      eMoF<-cleandataframe(eMoF,  vector = FALSE)
      eMoF <- cleanemof(eMoF) 
  
      if ( exists("Event") == TRUE){
        if ("id" %in% names(Event)){
          
        eMoF$eventID <- eMoF$id #eventID column is required in the measurements table.
      }} else {
        if ("id" %in% names(Occurrence)){
        eMoF$occurrenceID <- eMoF$id #occurrenceID column is required in the measurements table.
        }
  }
      output$eMoF <- eMoF
    }
    
    
    }
  
  
} }
  return (output)
}
