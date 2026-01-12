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
    
  out$data <- lapply(out$data, strip_outer_quotes_if_needed)
 
  output$ipt_url <- file
  
  if (length(out$emlmeta$dataset$coverage$temporalCoverage)>0) {
    for (i in 1:length(out$emlmeta$dataset$coverage$temporalCoverage)) {
      output$startdate <- c(output$startdate, out$emlmeta$dataset$coverage$temporalCoverage[[1]]$beginDate$calendarDate@.Data)
      output$enddate <- c(output$enddate, out$emlmeta$dataset$coverage$temporalCoverage[[i]]$endDate$calendarDate@.Data)
    }
    if(length(output$startdate)>0) {output$startdate <- min(output$startdate)}
    if(length(output$enddate)>0) {output$enddate <- max(output$enddate)}
  }
  
  #get filenames
  eventname <- dwca_rowtype_filenames(out, "Event")
  occurrencename <- dwca_rowtype_filenames(out, "Occurrence")
  dnaname <- dwca_rowtype_filenames(out, "DNADerivedData")
  emofname <- dwca_rowtype_filenames(out, "ExtendedMeasurementOrFact")
  mofname <- dwca_rowtype_filenames(out, "MeasurementOrFact")
  
  if (is.null(out$data[[eventname]]) == FALSE){
    Event <-out$data[[eventname]]
    
    if (length(Event) == 1 & nrow(Event) != 0){
      Event <- txt_to_cols(Event)
    }
    
    if (length(Event) >1 ) {
      
      Event<-cleandataframe(Event, vector = FALSE)
      output$Event <- fncols(Event, c("parentEventID", "eventDate"))
    }
    
    
    }
  
  if (is.null(out$data[[occurrencename]]) == FALSE){
    Occurrence <-out$data[[occurrencename]] 
    
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
  
  if (is.null(out$data[[dnaname]]) == FALSE){
    output$DNA <-out$data[[dnaname]]  
  }
  
  
  if (is.null(out$data[[emofname]]) == FALSE){
    eMoF <-out$data[[emofname]]  
      }
  
  if(exists("eMoF") == FALSE & is.null(out$data[[mofname]]) == FALSE) { 
    eMoF <- out$data[[mofname]] }
  
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

#helper function to get filenames of different DwC tables based on the meta.xml file
dwca_rowtype_filenames <- function(dwca, rowtype) {
  stopifnot(is.character(rowtype), length(rowtype) == 1, nzchar(rowtype))
  
  if (is.null(dwca$files) || is.null(dwca$files$xml_files)) {
    stop("`dwca` must be the output of finch::dwca_read(...).")
  }
  
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' is required. Install it with install.packages('xml2').")
  }
  
  meta_path <- grep("meta\\.xml$", dwca$files$xml_files, value = TRUE)[1]
  if (is.na(meta_path) || !nzchar(meta_path)) {
    stop("Could not find meta.xml in dwca$files$xml_files.")
  }
  
  meta <- xml2::read_xml(meta_path)
  xml2::xml_ns_strip(meta)
  
  cond <- if (grepl("^https?://", rowtype)) {
    sprintf('contains(@rowType, "%s")', rowtype)
  } else {
    sprintf('contains(@rowType, "/%s")', rowtype)
  }
  
  nodes <- xml2::xml_find_all(meta, sprintf("//core[%s] | //extension[%s]", cond, cond))
  files <- xml2::xml_text(xml2::xml_find_all(nodes, "files/location"))
  files <- unique(files[nzchar(files)])
  
  if (length(files) == 0) NA_character_ else files
}

#helper function to strip outer quotes if they were present around strings in the original data
strip_outer_quotes_if_needed <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(df)
  
  first_name <- names(df)[1]
  
  # Only run cleanup if the *first* column name is wrapped in quotes
  if (!is.na(first_name) && grepl('^".*"$', first_name)) {
    
    # Clean column names
    names(df) <- gsub('^"|"$', "", names(df))
    
    # Clean only character columns (remove a single pair of outer quotes)
    is_chr <- vapply(df, is.character, logical(1))
    df[is_chr] <- lapply(df[is_chr], function(x) gsub('^"|"$', "", x))
  }
  
  df
}
