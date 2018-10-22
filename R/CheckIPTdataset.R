CheckIPTdataset = function(link, tree = FALSE){

  IPTreport=list()
  #-----------------------------------------------------------------------#
  ####                    check if is downloadable file                ####
  #-----------------------------------------------------------------------#
  

  if (grepl("resource?", link)==TRUE) {
    link <- gsub("resource?", "archive", link) }
  
  
  if ( ((grepl("archive?", link)==FALSE | url.exists(link) == FALSE)) & grepl(".zip", link) == FALSE)
  {
IPTreport$dtb$general_issues <- ("please provide the IPT url")
  }  else   
  {

      
    #-----------------------------------------------------------------------#
    ####                    Inport data                                  ####
    #-----------------------------------------------------------------------#

      dwca_cache$delete_all()
      file <- link
    out <- dwca_read(file, read = TRUE)
    
    IPTreport$name <- names(NamesToVector(link))
    IPTreport$title <- out$emlmeta@dataset@title[[1]]@.Data  
    
      
      if (is.null(out$data[["event.txt"]]) == FALSE){
    Event <-out$data[["event.txt"]] }
      
      if (is.null(out$data[["occurrence.txt"]]) == FALSE){
    Occurrence <-out$data[["occurrence.txt"]]  }
      
      if (is.null(out$data[["extendedmeasurementorfact.txt"]]) == FALSE){
    eMoF <-out$data[["extendedmeasurementorfact.txt"]]  }
      
      if(exists("eMoF") == FALSE & is.null(out$data[["measurementorfact.txt"]]) == FALSE) { 
    eMoF <- out$data[["measurementorfact.txt"]] }
      
      
     
    if (  exists("Occurrence") == FALSE  ) {   
IPTreport$dtb$general_issues <- ("The dataset does not have an occurrence file")
    } else {
  
      
IPTreport <- Checkdataset (if(exists("Event"))Event , Occurrence, if(exists("eMoF"))eMoF, IPTreport, tree)

return(IPTreport)

}} }
