

BODC = list()

BODCunits  <- read.csv("https://raw.githubusercontent.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool/master/BODCdata/units.csv", stringsAsFactors = FALSE )
BODCvalues <-read.csv("https://raw.githubusercontent.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool/master/BODCdata/values.csv", stringsAsFactors = FALSE )
BODCparameters <-read.csv("https://raw.githubusercontent.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool/master/BODCdata/parameters.csv", stringsAsFactors = FALSE )


BODCnomofvalues <- c('http://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01/', 'http://vocab.nerc.ac.uk/collection/P01/current/SSAMPC01/')

BODCbiometrics <- c ('http://vocab.nerc.ac.uk/collection/P01/current/LSTAGE01/', 'http://vocab.nerc.ac.uk/collection/P01/current/OBSINDLX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/LGPIXEL1/', 'http://vocab.nerc.ac.uk/collection/P01/current/AGEBENTX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/CELLVOLM/', 'http://vocab.nerc.ac.uk/collection/P01/current/OBSMAXLX/',
                      'http://vocab.nerc.ac.uk/collection/P01/current/OBSMINLX/', 'http://vocab.nerc.ac.uk/collection/P01/current/ENTSEX01/' ) #create a list of the measurementtypeIDs to be taken into account when checking the occurrence table for duplicates

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



use_data(BODCunits, overwrite = TRUE)
use_data(BODCvalues, overwrite = TRUE)
use_data(BODCparameters, overwrite = TRUE)
use_data(BODCnomofvalues, overwrite = TRUE)
use_data(BODCbiometrics, overwrite = TRUE)
use_data(BODCeffort, overwrite = TRUE)
use_data(BODCinstrument, overwrite = TRUE)
use_data(BODCquantity, overwrite = TRUE)


