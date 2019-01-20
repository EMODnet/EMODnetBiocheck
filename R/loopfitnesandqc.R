#' fitness and QC loop
#'
#' loops fitness and qc function
#' @param mandatory a link the DWC-A file (URL to IPT or a zip file) 
#' @export
#' @examples
#' qc <- assesfitnessandqc("http://ipt.vliz.be/training/archive?r=biofun_2009")



loopfitnessandqc <- function (links) {

  for (link in links) {
    
    out <- assesfitnessandqc(link)

  

if (exists("qcout")) {
  qcout <- bind_rows(qcout, out) } else {
  qcout <- out }
  }  
  return (qcout)  
    
  }