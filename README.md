# LifeWatch-EMODnet-Biology-QC-tool

## Tool for checking if IPT datasets conform to the EMODnet guidelines 
The tool performs a basic QC on OBIS-env datasets and occurrence core dataset.  It allows checking of multiple IPTs at once. It checks:

- Is the dataset integrety ok?
  * Do all eventID's in the occurrence extention refer to an Event Record?
  * Do all eventID's in the eMoF extention refer to an Event Record?
  * Do all occurrenceID's in the eMoF extention refer to an occurrence Record?
  * In case of a biometrical parameter, is the eventID in the eMoF link the same one as in the occurrence extension?
  * Are there 'duplicate occurrences' meaning is the same taxon listed twice at the same EventID without any diference in any of the biometric paramters?
  * Are there 'duplicate measurements' meaning does the same measurement occur twice for the same occurrenceID or the same EventID?
*    Are all mandatory fields present in the dataset?
*    Are all mandatory field filled out?
*    Does eventDate follow the required format?
*    Does the scientificNameID follow the required format?
*    Are there coordinates located on land? (buffer of 3km is taken under consideration)
*    Are there depths at the location deeper than the depths stored by GEBCO? (a margin of 150m is taken under consideration)
*    Do all measurementtypes have a MeasurementTypeID?
*    Does the measurementTypeID / measurementValueID refer to an existing term in the BODC vocabulary?
*    Do all measurementValues that refer to facts have a measurementValueID?
*    Are there records where measurementValue is NULL?
*    Is there a sampling instrument present?
*    Are there other sampling descriptors present?

*    Are there records that refer to biological measurement  where measurementValue = 0 (and where occurrenceStatus is not absent)?

Plots of the coordinates and the distribution of the temporal cover are provided to allow for quick comparison with the metadata. A tree view of the event hierarchy can be used to inspect the structure.

Additional checks which are planned to be implemented are:

 *   Provide an overview of the number of taxa per kingdom and class.
 *   Check for non-marine and non-brackish taxa.
 *   Has the unit provided the same base unit as specified by the measurementTypeID?
 *   List the non-matched taxa
 *   Add geographical cover and temporal cover from the eml to the overview page.
 
The tool is was created using the OBIS tools package (https://github.com/iobis/obistools) and is avaiable from the LifeWatch services at http://rshiny.lifewatch.be/BioCheck/
The R functions are available from the EMODnet github repository at https://github.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool


## usage of the R Code:
```R
source("https://raw.githubusercontent.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool/master/R/installallneededpackages.R")
source("https://raw.githubusercontent.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool/master/R/emodnetqc.R")
BODC <- getbodc()  
LoopCheckdataset ("http://ipt.iobis.org/training/archive?r=biofun_2009")
LoopCheckdataset (c("http://ipt.iobis.org/training/archive?r=biofun_2009", "http://ipt.vliz.be/eurobis/resource?r=benthic-fauna-arrabida-2007-2009"))
```
