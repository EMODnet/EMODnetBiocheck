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
-    Are all mandatory fields present in the dataset?
-    Are all mandatory field filled out?
-    Does eventDate follow the required format?
-    Does the scientificNameID follow the required format?
-    Are there coordinates located on land? (buffer of 3km is taken under consideration)
-    Do the coordinates on land refer to marine taxa?
-    Are there depths at the location deeper than the depths stored by GEBCO? (a margin of 150m is taken under consideration)
-    Do all measurementtypes have a MeasurementTypeID?
-    Does the measurementTypeID / measurementValueID refer to an existing term in the BODC vocabulary?
-    Do all measurementValues that refer to facts have a measurementValueID?
-    Are there records where measurementValue is NULL?
-    Are there records that refer to biological measurement  where measurementValue = 0 (and where occurrenceStatus is not absent)?
-   Is there a sampling instrument present?
-   Are there other sampling descriptors present?
-   Provide an overview of the number of taxa per kingdom and class.
-   List the non-matched taxa (including deleted and quarantined matches)
-   Plots of the coordinates and the distribution of the temporal cover are provided to allow for quick comparison with the metadata. 
-   A tree view of the event hierarchy to inspect the structure.
 -  Has the unit provided the same base unit as specified by the measurementTypeID?

Additional checks which are planned to be implemented are:

 -  Are there non-marine / non-brackisch taxa found at sea? 

 
The tool is was created using the OBIS tools package (https://github.com/iobis/obistools) and is avaiable from the LifeWatch services at http://rshiny.lifewatch.be/BioCheck/
The R functions are available from the EMODnet github repository at https://github.com/EMODnet/LifeWatch-EMODnet-Biology-QC-tool


## Installation

Installing `EMODnetBiocheck` requires the `devtools`, `skosxml` and `obistools` packages:

```R
install.packages("devtools")
devtools::install_github("iobis/obistools")
devtools::install_github("EMODnet/skosxml")
devtools::install_github("EMODnet/EMODnetBiocheck")
```


## usage:
```R
loopcheckIPTdataset ("http://ipt.iobis.org/training/archive?r=biofun_2009", tree="yes")
loopcheckIPTdataset (c("http://ipt.iobis.org/training/archive?r=biofun_2009", "http://ipt.vliz.be/eurobis/resource?r=benthic-fauna-arrabida-2007-2009", tree="yes"))
```
