####  setup the working environment



LoopCheckIPTdataset = function(x, tree = FALSE, obisqc = FALSE){

  if (tree == FALSE) { tree <- FALSE } else {tree <- TRUE}
  if (obisqc == FALSE) { obisqc <- FALSE } else {obisqc <- TRUE}

  
  x <-  NamesToVector(x)

  #Start loop over all files
  
  for(i in names(x)) {

    #empty workspace again
    rm(list=setdiff(ls(), c("x", "i", "j", "biometrics", "fncols", "getPackage", "obisqc")))

    ## create the export folder
    if (file.exists(names(x[i]))){
      setwd(file.path(getwd()))
    } else {
      dir.create(file.path(getwd(), names(x[i])))
      setwd(file.path(getwd()))
    }
    
    
    
link <- x[[i]][1]
IPTreport <- CheckIPTdataset(link, tree)



#exportIPT(IPTreport,  names(x[i]), obisqc )


  }
  }

