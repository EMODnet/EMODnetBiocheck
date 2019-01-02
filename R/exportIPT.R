exportIPT = function (IPTreport, obisqc = FALSE) {

if(is.null(IPTreport$name) == TRUE) {
  IPTreport$name <- "QCexport" }
if (file.exists(IPTreport$name) == FALSE) {
    dir.create(file.path(getwd(), IPTreport$name))
} 

formatLabel<-function (st,parname=NULL) {
    # onclick function for the map
    p1=""; colns=colnames(st);
    if("Lat" %in% colns) p1=paste(p1,"Lat:",round(st$Lat,3));
    if("Lon" %in% colns) p1=paste(p1,"Long:",round(st$Lon,3));
    if("decimalLatitude" %in% colns) p1=paste(p1,"decimalLatitude:",st$decimalLatitude, "<br/>");
    if("decimalLongitude" %in% colns) p1=paste(p1,"decimalLongitude:",st$decimalLongitude, "<br/>");
    if("coordinateUncertaintyInMeters" %in% colns) p1=paste(p1,"coordinateUncertaintyInMeters:",st$coordinateUncertaintyInMeters, "<br/>" );
    if("occurrenceID" %in% colns) p1=paste(p1,"occurrenceID:",st$occurrenceID, "<br/>" );
    if("eventID" %in% colns) p1=paste(p1,"eventID:",st$eventID, "<br/>");
    if("quality" %in% colns) p1=paste(p1,"issue:", st$quality);
    return(p1)
  }
  

download.file("https://raw.githubusercontent.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool/master/R/overview.Rmd",
                "overview.Rmd")
download.file("https://raw.githubusercontent.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool/master/R/qcreport.Rmd",
                "qcreport.Rmd")

render("overview.Rmd", "html_document", output_dir=paste(getwd(), "/" ,IPTreport$name, sep="" ) , output_file = paste0("overview", ".html"))
render("qcreport.Rmd", "html_document", output_dir=paste(getwd(), "/" ,IPTreport$name, sep="" ) , output_file = paste0("qcreport", ".html"))

file.remove("overview.Rmd")
file.remove("qcreport.Rmd")

  
  
  
## step 2 export non tables
    if (is.null(IPTreport$tree) == FALSE   ) {
exportTree(IPTreport$tree, paste(IPTreport$name, "/",  "tree",  ".html", sep = ""), view=FALSE )
    }



## step 3 export the generated tables

    
    for (t in names(IPTreport$dtb)) {
      if (is.data.frame(IPTreport$dtb[[t]]))    if (nrow(IPTreport$dtb[[t]]) >0 ) {
        path <- paste0(IPTreport$name, "/", t, ".txt")
        write.table(IPTreport$dtb[[t]], file=path, row.names = FALSE, sep = "\t", quote = FALSE, na = "")
      }  }
    
}










