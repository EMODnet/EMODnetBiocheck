install.packages("devtools")
library("devtools")

install.packages("rlang")
install.packages("processx")
install.packages("Rtools")
install.packages("backports")
install.packages("roxygen2")


library("roxygen2")
getwd()
create("EMODnetBiocheck")
setwd("./EMODnetBiocheck")


## copy functions to folder
document()

setwd("..")

install("EMODnetBiocheck")
library("EMODnetBiocheck")
?REMODnetBioQC



install.packages("devtools") # document that you will need to install this package first!
devtools::install_github("iobis/obistools") # document that you will need to install this package first!


#test2
link <- "http://ipt.iobis.org/training/archive?r=biofun2009_solution"

IPTreport <- checkIPTdataset (link)


setwd("./test")
link <- "http://ipt.iobis.org/training/resource?r=biofun_2009"

loopcheckIPTdataset (link)


