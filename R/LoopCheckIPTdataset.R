loopcheckIPTdataset = function(x, tree = FALSE, obisqc = FALSE){


  x <-  NamesToVector(x)

  #Start loop over all files
  
  for(i in names(x)) {

    #empty workspace again
    rm(list=setdiff(ls(), c("x", "i", "j", "BODC", "fncols", "getPackage", "obisqc", "tree")))

    ## create the export folder
    if (file.exists(names(x[i]))){
      setwd(file.path(getwd()))
    } else {
      dir.create(file.path(getwd(), names(x[i])))
      setwd(file.path(getwd()))
    }
    
    
link <- x[[i]][1]
IPTreport <- checkIPTdataset(link, tree)



exportIPT(IPTreport, obisqc)


  }
  }