#' Check the quality of the dataset
#'
#' This function takes a DWC file as input and carries out the checkdataset funtion. The output is a list.
#' @param link mandatory parameter, link to an IPT resource or an DwC zip file
#' @param tree optional parameter, takes value yes if you want the QC report to include the OBIS tree hierachy 
#' @export
#' @examples
#' IPTreport <- checkIPTdataset(link = "http://ipt.vliz.be/upload/resource?r=biofun2009", tree = FALSE)
#' IPTreport <- checkIPTdataset(link = "dwca-biofun2009-v1.1.zip", tree = "yes")


checkIPTdataset = function(link, tree = FALSE){
  

  output <- importiptdata (link)
  
  if (is.null(output$error)) {
   
    IPTreport <- checkdataset(IPTreport=output, tree)
    
      }
  else {
    IPTreport <- output$error
  }
  return(IPTreport)
}
