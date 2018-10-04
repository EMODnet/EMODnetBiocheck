
exportIPT = function (IPTreport, x, obisqc = FALSE) {


  # Generate QC report
  
files <-  c(if(is.data.frame(IPTreport$eventerror_table)) "eventerror_table.txt", if(is.data.frame(IPTreport$occurrenceerror_table)) "occurrenceerror_table.txt",
    if(is.data.frame(IPTreport$emoferror_table)) "emoferror_table.txt")

    if (is.data.frame(IPTreport$all_report_issues)) {if (nrow(IPTreport$all_report_issues) > 0) { 
all_report_issues <- IPTreport$all_report_issues %>% select (table,  field, 	message, 	count)
      } else { 
all_report_issues <- c("no issues dectected")} }
    if (is.data.frame(IPTreport$mofreport)) {
mofreport <- IPTreport$mofreport

          }
  
 download.file("https://raw.githubusercontent.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool/master/R/reportfile.Rmd",
                "reportfile.Rmd")
  render("reportfile.Rmd", "html_document", output_dir=paste(getwd(), "/" ,x, sep="" ) , output_file = paste("EMODnetQualityReport_", x, ".html", sep ="" ) )
  
  file.remove("reportfile.Rmd")
  
  
  
  
## step 2 export non tables
    if (  exists("IPTreport$tree") == TRUE   ) {
exportTree(IPTreport$tree, paste(x, "/",  "tree_", x, ".html", sep = ""), view=FALSE )
    }


      if  (obisqc == TRUE) {
      if (  exists("IPTreport$tree") == TRUE  ){ 
report(flatten_occurrence(IPTreport$ev_flat,IPTreport$Occurrence) %>% filter(!is.na(decimalLatitude)), file =  paste("OBISreport_",  x, ".html", sep = "") ,  dir = paste( getwd(),"/",x, sep = "") , view = FALSE)
    } else {
report(IPTreport$Occurrence %>% filter(!is.na(decimalLatitude),!is.na(decimalLongitude) ), file =  paste("OBISreport_",  x, ".html", sep = "") ,  dir = paste( getwd(),"/",x, sep = "") , view = FALSE)
      }}



## step 3 export the generated tables

    if(is.data.frame(IPTreport$eventerror_table)){
write.table(IPTreport$eventerror_table, paste(x, "/", "eventerror_table", "_", x, ".txt", sep = ""),
              row.names = FALSE, sep = "\t", quote = FALSE, na = "")
    }
  
    if(is.data.frame(IPTreport$occurrenceerror_table)){
write.table(IPTreport$occurrenceerror_table, paste(x, "/", "occurrenceerror_table", "_", x, ".txt", sep = ""),
                row.names = FALSE, sep = "\t", quote = FALSE, na = "")
    }
    if(is.data.frame(IPTreport$emoferror_table)){
write.table(IPTreport$emoferror_table, paste(x, "/", "emoferror_table", "_", x, ".txt", sep = ""),
                row.names = FALSE, sep = "\t", quote = FALSE, na = "")
    }  
  

}


