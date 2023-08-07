
# These lines might be deprecated-----
# BODC = list()
# 
# BODCunits  <- read.csv("https://raw.githubusercontent.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool/master/BODCdata/units.csv", stringsAsFactors = FALSE )
# BODCvalues <-read.csv("https://raw.githubusercontent.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool/master/BODCdata/values.csv", stringsAsFactors = FALSE )
# BODCparameters <-read.csv("https://raw.githubusercontent.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool/master/BODCdata/parameters.csv", stringsAsFactors = FALSE )
# 
# To-do: Call data from data folder instead of from github
# load("data/BODCunits.rda")
# load("data/BODCvalues.rda")
# load("data/BODCparameters.rda")
# -------------------------------------

# Update data files

BODCnomofvalues <- c('http://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01/', 'http://vocab.nerc.ac.uk/collection/P01/current/SSAMPC01/')

BODCbiometrics <- c ('http://vocab.nerc.ac.uk/collection/P01/current/LSTAGE01/', 'http://vocab.nerc.ac.uk/collection/P01/current/OBSINDLX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/LGPIXEL1/', 'http://vocab.nerc.ac.uk/collection/P01/current/AGEBENTX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/CELLVOLM/', 'http://vocab.nerc.ac.uk/collection/P01/current/OBSMAXLX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/OBSMINLX/', 'http://vocab.nerc.ac.uk/collection/P01/current/CTROPH01/' 
                      ,'http://vocab.nerc.ac.uk/collection/P01/current/ENTSEX01/' ) #create a list of the measurementtypeIDs to be taken into account when checking the occurrence table for duplicates 

BODCeffort <- c('AREABEDS', 'Q01', 'VOLWBSMP', 'LENTRACK' , 'AZDRZZ01' ,'VOLFFMXX')

BODCinstrument <- c('Q0100002')




BODCquantity <- c('http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL07/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL08/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL09/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL10/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL12/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL13/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL14/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SACFORN1/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SACFOR01/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL01/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL02/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL03/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL04/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL05/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/ADBIOL01/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/ADBIOL02/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/ODRYBM01/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/OWETBM01/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01/',
                  'http://vocab.nerc.ac.uk/collection/P01/current/USPBIOSV/')



p01todwc <- data.frame(P01s = c("SNANID01", "SCNAME01", "SSAMID01", "SAMPPROT", "SAMPID01", "MAXWDIST", "MINWDIST", "PRESABS1", "EVNTID01"),
                       dwc  = c("scientificNameID", "scientificName", "eventID" ,"samplingProtocol", "parentEventID","maximumDepthInMeters", "minimumDepthInMeters", "occurrenceStatus", "eventID2"),
                       stringsAsFactors = FALSE)


# use_data(BODCunits, overwrite = TRUE)
# use_data(BODCvalues, overwrite = TRUE)
# use_data(BODCparameters, overwrite = TRUE)
use_data(BODCnomofvalues, overwrite = TRUE)
use_data(BODCbiometrics, overwrite = TRUE)
use_data(BODCeffort, overwrite = TRUE)
use_data(BODCinstrument, overwrite = TRUE)
use_data(BODCquantity, overwrite = TRUE)
use_data(p01todwc, overwrite = TRUE)
use_data(skossconcepts, overwrite = TRUE)
use_data(eunishabitats, overwrite = TRUE)


