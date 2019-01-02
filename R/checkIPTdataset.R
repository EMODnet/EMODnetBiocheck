checkIPTdataset = function(link, tree = FALSE){
  
  IPTreport=list()
  #-----------------------------------------------------------------------#
  ####                    check if is downloadable file                ####
  #-----------------------------------------------------------------------#
  
  
  if (grepl("resource?", link)==TRUE) {
    link <- gsub("resource?", "archive", link) }
  
  
  if ( ((grepl("archive?", link)==FALSE | url.exists(link) == FALSE)) & grepl(".zip", link) == FALSE)
  {
    IPTreport$error <- ("Link does not resolve to a public IPT resource")
  }  else   
  {
    
    
    #-----------------------------------------------------------------------#
    ####                    Inport data                                  ####
    #-----------------------------------------------------------------------#
    
    dwca_cache$delete_all()
    file <- link
    
    tryCatch(
      {out <- dwca_read(file, read = TRUE)}, 
      error=function(x) {"Link does not resolve to a public IPT resource"}
    )
    
    if (exists("out")){ 
      
      IPTreport$name <- names(NamesToVector(link))
      IPTreport$title <- out$emlmeta@dataset@title[[1]]@.Data  
      
      if (length(out$emlmeta@dataset@coverage@temporalCoverage)>0) {
        for (i in 1:length(out$emlmeta@dataset@coverage@temporalCoverage)) {
          IPTreport$startdate <- c(IPTreport$startdate, out$emlmeta@dataset@coverage@temporalCoverage[[i]]@rangeOfDates@beginDate@calendarDate@.Data)
          IPTreport$enddate <- c(IPTreport$enddate, out$emlmeta@dataset@coverage@temporalCoverage[[i]]@rangeOfDates@endDate@calendarDate@.Data)
        }
        if(length(IPTreport$startdate)>0) {IPTreport$startdate <- min(IPTreport$startdate)}
        if(length(IPTreport$enddate)>0) {IPTreport$enddate <- max(IPTreport$enddate)}
      }
      
      if (is.null(out$data[["event.txt"]]) == FALSE){
        Event <-out$data[["event.txt"]] }
      
      if (is.null(out$data[["occurrence.txt"]]) == FALSE){
        Occurrence <-out$data[["occurrence.txt"]]  }
      
      if (is.null(out$data[["extendedmeasurementorfact.txt"]]) == FALSE){
        eMoF <-out$data[["extendedmeasurementorfact.txt"]]  }
      
      if(exists("eMoF") == FALSE & is.null(out$data[["measurementorfact.txt"]]) == FALSE) { 
        eMoF <- out$data[["measurementorfact.txt"]] }
      
      
      
      if (  exists("Occurrence") == FALSE  ) {   
        IPTreport$error <- ("The dataset does not have an occurrence file")
      } else {
        
        
        IPTreport <- checkdataset (if(exists("Event"))Event , Occurrence, if(exists("eMoF"))eMoF, IPTreport, tree)
      }}
    else {
      IPTreport$error <- "Link  does not resolve to a public IPT resource"    
    }
  }
  return(IPTreport)
}
