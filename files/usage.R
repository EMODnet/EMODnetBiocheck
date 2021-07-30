##---------------------------------------------------------------------------------
## Ruben Perez Perez
## Science Officer Data Center at VLIZ 
## EurOBIS - EMODnet Biology
## "Fri Jul 30 12:02:20 2021"

## Code for EMODnetBiocheck R package tutorials

## Two tutorials need to be made to properly document the usage of the EMODnetBiocheck R package both using an 
## IPT resource URL as input and using the loaded data tables as input.

##------------------------------------------------------------------------------------------------------------

## Installing and loading necessary libraries
install.packages("devtools")
devtools::install_github("EMODnet/EMODnetBiocheck")
library(EMODnetBiocheck)


#-------------------------------------------------------------------------------------------------------------

#### Using an IPT resource as input

# One IPT resource
loopcheckIPTdataset ("http://ipt.iobis.org/training/archive?r=biofun_2009", tree="yes")


#Several IPT resources at the same time
loopcheckIPTdataset (c("http://ipt.iobis.org/training/archive?r=biofun_2009", "http://ipt.vliz.be/eurobis/resource?r=benthic-fauna-arrabida-2007-2009"), tree="yes")


#-------------------------------------------------------------------------------------------------------------

#### Using loaded data tables as input
IPTreport <- checkdataset(Event = myeventtable, Occurrence = myoccurrencetable, eMoF = myemoftable)

# Exploring results
datasummary <- IPTreport$datasummary # Overview of event and occurrence records
mofsummary <- IPTreport$mofsummary # Overview of measurement or fact records (types and units)
mofsummary_values <- IPTreport$mofsummary_values # Overview of measurement or fact records (standardized values)
dates_plot <- IPTreport$dates_plot # Temporal cover of the dataset
kingdoms <- IPTreport$kingdoms # Taxonomic cover of the dataset


general_issues <- IPTreport$dtb$general_issues
mof_issues <- IPTreport$dtb$mof_issues
taxa <- IPTreport$dtb$taxa # Overview of the non-matched taxa

eventerror_table <- IPTreport$dtb$eventerror_table
occurrenceerror_table <- IPTreport$dtb$occurrenceerror_table
emoferror_table <- IPTreport$dtb$emoferror_table


#### Using an IPT resource as input for the checkdataset function
IPTreport2 <- checkdataset(IPTreport = importiptdata("http://ipt.iobis.org/training/archive?r=biofun_2009"))


#-------------------------------------------------------------------------------------------------------------

