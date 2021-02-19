#' Check the quality of multiple dataset
#'
#' This function takes multiple ipt or dwc files as input and generates html QC reports
#' @param x mandatory parameter, a vector with one or more IPT links or DwC file locations
#' @param tree optional parameter, takes value yes if you want the QC report to include the OBIS tree hierachy 
#' @export
#' @examples
#' \dontrun{
#' loopcheckIPTdataset(c("http://ipt.vliz.be/upload/resource?r=biofun2009","http://ipt.vliz.be/upload/resource?r=ukranianromanian_benthos_blacksea_iaea_1998"))
#' }



loopcheckIPTdataset = function(x, tree = FALSE){


  x <-  NamesToVector(x)

  #Start loop over all files
  
  for(i in names(x)) {

    ## create the export folder
    if (file.exists(names(x[i]))){
      setwd(file.path(getwd()))
    } else {
      dir.create(file.path(getwd(), names(x[i])))
      setwd(file.path(getwd()))
    }
    
    
link <- x[[i]][1]
IPTreport <- checkIPTdataset(link, tree)



exportIPT(IPTreport)


  }
  }
