#' fitness and QC 
#'
#' Function which returns codes refering to the fitness of use or an the quality
#' @param link a link the DWC-A file (URL to IPT or a zip file) 
#' @export
#' @examples
#' \dontrun{
#' qc <- assesfitnessandqc("http://ipt.vliz.be/training/archive?r=biofun_2009")
#' }



assesfitnessandqc <- function (link) {

output <- importiptdata (link)

if (is.null(output$error)) {
fitness <- assessfitness (output=output)

IPTreport <- checkdataset(IPTreport=output)

if (!is.null(IPTreport$dtb$eventerror_table)) {
  occevents <- IPTreport$dtb$eventerror_table %>% inner_join(output$Occurrence, by = "eventID") %>% select (occurrenceID)
  }
if (!is.null(IPTreport$dtb$occurrenceerror_table)){
  occs <- IPTreport$dtb$occurrenceerror_table %>% select (occurrenceID)
}
if (!is.null(IPTreport$dtb$emoferror_table)){
  occmofs <- IPTreport$dtb$emoferror_table %>% inner_join(output$Occurrence, by = "occurrenceID") %>% select (occurrenceID)
}
issuesperc <- nrow(bind_rows(if(exists("occevents"))occevents, if(exists("occs"))occs, if(exists("occmofs"))occmofs ) %>% distinct()) / nrow(output$Occurrence)

#if (issuesperc==0) {qcflag = "A"}
#if (issuesperc < 0.10) {qcflag = "B"}
#if (issuesperc < 0.20) {qcflag = "C"}
#if (issuesperc > 0.20) {qcflag = "F"}
qcflag = suppressWarnings(as.integer(10 - (issuesperc *10)))


out <- data.frame(IPT = link , fitness = fitness$fitvalue, qc = qcflag)
} else {
out <- data.frame(IPT = link ,error = output$error)
}


return(out)
}