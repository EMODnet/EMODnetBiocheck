
# EMODnetBiocheck: LifeWatch-EMODnetBiology QC tool

  <!-- badges: start -->
  [![R-CMD-check](https://github.com/EMODnet/EMODnetBiocheck/workflows/R-CMD-check/badge.svg)](https://github.com/EMODnet/EMODnetBiocheck/actions)
  [![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
  [![Funding](https://img.shields.io/static/v1?label=powered+by&message=lifewatch.be&labelColor=1a4e8a&color=f15922)](http://lifewatch.be)
  <!-- badges: end -->

#### A tool for Quality Controlling Darwin Core based datasets according to the EMODnet Biology guidelines. The tool performs a thorough QC on OBIS-env datasets and occurrence core datasets. It can use an IPT resource URL as input

- [Rationale](#rationale)
- [Installation](#installation)
- [Usage](#usage)
- [Understanding the output](#understanding-the-output)
- [Integrity Checks](#integrity-checks)
- [Format Checks](#format-checks)
- [Visual Checks](#visual-checks)
- [BioCheck Rshiny application](#biocheck-rshiny-application)

***

## Rationale
Quality controlling a dataset is fundamental in order to ensure its appropriate usage. The EMODnetBiocheck R package is developed in the framework of the LifeWatch and EMODnet Biology projects, and managed by the <a href="http://www.eurobis.org/" target="_blank">EurOBIS</a> (European Ocean Biodiversity Information System) Data Management Team at the Flanders Marine Institute (<a href="https://www.vliz.be/" target="_blank">VLIZ</a>). It helps users to Quality Control their (marine) biological datasets by performing a varied number of quality checks on both published and unpublished datasets. This R package also allows a thorough visual exploration of the dataset, while highlighting potential issues within the dataset. The R package can be used on: i) public IPT resources; ii) loaded data tables. The only requirement to use the R package is the existence of an Occurrence table in the dataset, although the analysis reaches its full potential using an IPT resource with <a href="https://obis.org/manual/dataformat/" target="_blank">OBIS-ENV data format</a> (Core: "Event"; Extensions: "Occurrence" and "Extended Measurements or Facts").


## Installation

Installing `EMODnetBiocheck` requires the `devtools` package:

```R
install.packages("devtools")
devtools::install_github("EMODnet/EMODnetBiocheck")
library(EMODnetBiocheck)
```


## Usage:

For IPT resources (recommended)
```R
# This function can be run on several IPT resources at the same time
loopcheckIPTdataset ("http://ipt.iobis.org/training/archive?r=biofun_2009", tree="yes")
loopcheckIPTdataset (c("http://ipt.iobis.org/training/archive?r=biofun_2009", "http://ipt.vliz.be/eurobis/resource?r=benthic-fauna-arrabida-2007-2009"), tree="yes")
```
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tutorial: <a href="https://drive.google.com/file/d/1ItFD_qkg9le8MOOaELqLMMq0FefeAB3G/view?usp=sharing" target="_blank">loopcheckIPTdataset function usage</a>

For loaded data tables (to use on unpublished datasets or if no IPT resource is available)
```R
IPTreport <- checkdataset(Event = youreventtablename, Occurrence = youroccurrencetablename, eMoF = youremoftablename)
```
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tutorial: <a href="https://drive.google.com/file/d/1NMGbbeRvIV67QaQQuvpcJXb3i85bElJz/view?usp=sharing" target="_blank">checkdataset function usage</a>

***

## Understanding the output

The tool is based on and builds on top of the <a href="https://github.com/iobis/obistools" target="_blank">obistools R package</a> to provide a visual exploration of the dataset and highlight potential issues running the following checks:

<br/><br/>

#### ***Integrity Checks***

| Quality check | Error message | Output location | Why this check? | 
| --- | - | --- | --------------------- |
| Check if all eventIDs in an extension have corresponding eventIDs in the core. | This eventID has no corresponding eventID in the core | IPTreport\$dtb\$general_issues | The eventIDs from the extension tables must always refer to existing eventIDs, therefore, these eventIDs must be present in the core table. | 
| Checks that all eMoF eventIDs are linked to the same eventID as the related occurrenceID | This eventID differs from the eventID provided in the related Occurrence | IPTreport\$dtb\$general_issues | The records in the eMoF table may have both an eventID and an occurrenceID. This occurrenceID must exist in the occurrence table, where it will be linked to an eventID. This eventID must be the same eventID that is referred to in the eMoF table. The reason is that one occurrenceID can only be linked to one eventID. Therefore it is not coherent that the eMoF record links to an occurrenceID and an eventID that are not related. | 
| Check if eventID and parentEventID are present, and parentEventIDs have corresponding eventIDs | Field "x" is missing / eventID "x" is duplicated / parentEventID "x" has no corresponding eventID | IPTreport\$dtb\$general_issues | The parentEventID field is used to link 2 different eventIDs, that exist under the eventID field, in a hierarchy relationship. In this reationship, one of the eventIDs will be the parent event and the other one will be the child event. Therefore if the parentEventID field is populated, it must be populated with an existing eventID from the eventID field. Adding to the parentEventID field something different than an existing eventID is incoherent. | 
| Checks that occurrenceID is unique in the occurrence table | occurrenceID "x" is duplicated in the Occurrence table | IPTreport\$dtb\$general_issues | The occurrenceID acts as the Primary Key for the occurrence table, therefore two records with the same occurrenceID cannot exist in the occurrence table. | 
| Checks that all occurrenceIDs in the eMoF table exist in the Occurrence table | This occurrenceID has no corresponding occurrenceID in the occurrence Extension | IPTreport\$dtb\$general_issues | The occurrenceID from the extension tables must always refer to existing occurrenceIDs, therefore, these occurrenceIDs must be present in the occurrence table. | 
| Check if none of the records has occurrenceStatus = "present" | None of the occurrence records have occurrenceStatus = "present" | IPTreport\$dtb\$general_issues | Normally some of the occurrence records in a dataset would be "presences", having only "absences" in a dataset will likely be an error. | 
| Check on a number of fields (including some MoFs) for potential duplicated occurrences | Potential duplicate record | IPTreport\$dtb\$general_issues | There shouldn't be duplicated occurrences in the Occurrence table. This check only looks at a limited number of fields ("decimalLatitude", "decimalLongitude", 'eventDate', 'eventTime', 'minimumDepthInMeters', 'maximumDepthInMeters', 'eventID', 'lifeStage', 'sex', 'samplingProtocol', 'scientificName',  'scientificNameID', 'identificationRemarks', 'identificationQualifier') to assess if two records are duplicated. It may happen that the records are not duplicates but that their difference is not reflected into any of the fields checked, this is why these are "potential" duplicates and not just duplicates. | 
| Check if coordinateUncertaintyInMeters contains non numeric values | Some coordinateUncertaintyInMeters values are not numeric | IPTreport\$dtb\$general_issues | The coordinateUncertaintyInMeters field is expressed as a number so its content must be numeric. | 
| Check if latitude contains non numeric values | Some decimalLatitude values are not numeric | IPTreport\$dtb\$general_issues | The decimalLatitude field is expressed as a number so its content must be numeric. | 
| Check which points are located on land. | Coordinates are located on land | IPTreport\$dtb\$general_issues | Marine datasets shouldn't contain records very far inland, unless these happen in large estuaries, are about marine birds or similar. | 
| Check if both lat and long are 0 | decimalLatitude and decimalLongitude are both 0 | IPTreport\$dtb\$general_issues | It is theoretically possible that a sampling event happens at 0-0 coordinates, however it is very common to inadvertedly add 0-0 when the actual coordinates are missing. Therefore, 0-0 coordinates should always be double checked. | 
| Check if longitude contains non numeric values | Some decimalLongitude values are not numeric | IPTreport\$dtb\$general_issues | The decimalLongitude field is expressed as a number so its content must be numeric. | 
| Check which points have potentially invalid depths / Are there depths at the location deeper than the depths stored by GEBCO? (a margin of 150m is taken under consideration) | Minimum depth is greater than maximum depth / Depth value is greater than the value found in the bathymetry raster | IPTreport\$dtb\$general_issues | A depth value must always be numeric and not much higher than the maximum depth at a certain location, it should also not be a negative value (above water) if the record is offshore. In addition, the minimum depth cannot be greater than the maximum depth. | 
| Is any BODC vocabulary term used as measurementTypeID deprecated? | This measurementTypeID is deprecated | IPTreport\$dtb\$mof_issues | It is not recommended using deprecated vocabulary terms, when possible, the updated version should be used. | 
| Check if there are duplicated measurementTypeIDs linked to the same eventID (for non occurrence related mofs) / Does the same measurement occurs more than once for the same eventID? | Duplicate eMoF record linked to event | IPTreport\$dtb\$general_issues | Ideally a given eventID can be linked to the same measurementTypeID only once. E.g. It doesn't make sense to say that the instrument used to collect this eventID is a corer in one record but a net in another record, it must be or either a corer or a net, but all expressed in only one record. | 
| Check if there are duplicated measurementTypeIDs linked to the same occurrenceID / Does the same measurement occurs more than once for the same occurrenceID? | Duplicate eMoF record linked to occurrence | IPTreport\$dtb\$general_issues | Ideally a given occurrenceID can be linked to the same measurementTypeID only once. E.g. It doesn't make sense to say that one same occurrenceID contains adults in one record but larvae in another record, it must be or either adults or larvae or both, but all expressed in only one record. | 
| Is any BODC vocabulary term used as measurementUnitID deprecated? | This measurementUnitID is deprecated | IPTreport\$dtb\$mof_issues | It is not recommended using deprecated vocabulary terms, when possible, the updated version should be used. | 
| Check if measurementValue is NULL | MeasurementValue of Null | IPTreport\$dtb\$general_issues | The measurementValue field must always be populated in every record of the eMoF table. The record would be incoherent if it misses the measurementValue. | 
| Check if an occurrence related measurementValue is 0 while that occurrence has occurrenceStatus = "present" | Biological value of 0 while occurrenceStatus is present | IPTreport\$dtb\$general_issues | If an occurrence is labelled as present, as existing, most measurements related to it must be higher than 0. E.g. It doesn't make sense that an existing fish weights 0 grams or is 0 centimetres long. | 
| Is any BODC vocabulary term used as measurementValueID deprecated? | This measurementValueID is deprecated | IPTreport\$dtb\$mof_issues | It is not recommended using deprecated vocabulary terms, when possible, the updated version should be used. | 
| Checks if the content of the datasetName field differs from the Title of the IPT resource | datasetName is slightly different from the title of the IPT resource | IPTreport\$dtb\$general_issues | The title and the name of a dataset should be the same. Therefore, the title of the IPT resource should be the same than the datasetName. | 
| Checks if datasetName contains more than one unique values | datasetName contains more than one unique values, excluding NA values | IPTreport\$dtb\$general_issues | One dataset should contain only one name and therefore only one datasetName | 
| Check if the occurrences on land are marine species | Marine taxon located on land | IPTreport\$dtb\$general_issues | The records found on land should not be purely marine species. There are some exceptions such as for fossiles. | 

<br/><br/>

#### ***Format Checks***

| Quality check | Error message | Output location | Why this check? | 
| --- | - | --- | --------------------- |
| Check if basisOfRecord contains standardized terms | basisOfRecord does not seem to contain a valid term such as: "PreservedSpecimen", "FossilSpecimen", "LivingSpecimen", "HumanObservation", "MachineObservation", "MaterialSample", "Occurrence" | IPTreport\$dtb\$general_issues | Some fields such as basisOfRecord, must contain only standardised terms. For basisOfRecord the terms that can be used are "PreservedSpecimen", "FossilSpecimen", "LivingSpecimen", "HumanObservation", "MachineObservation", "MaterialSample", "Occurrence". | 
| Check if datasetName field is missing | Required field datasetName is missing | IPTreport\$dtb\$general_issues | A minimum number of fields are needed for the dataset to have certain coherence, for EurOBIS the dataseName field is required. Have a better overview in https://docs.google.com/spreadsheets/d/1mWvg0jyayAs9h0SuuqGgHuenvgOlSKYp1DW9q607lEo/edit#gid=1315772191 | 
| Check if eventDate format is compliant with ISO8601. | eventDate does not seem to be a valid date | IPTreport\$dtb\$general_issues | The content of the eventDate field must follow the ISO8601 format. | 
| Check if institutionCode field is missing | Required field institutionCode is missing | IPTreport\$dtb\$general_issues | A minimum number of fields are needed for the dataset to have certain coherence, for EurOBIS the institutionCode field is required. Have a better overview in https://docs.google.com/spreadsheets/d/1mWvg0jyayAs9h0SuuqGgHuenvgOlSKYp1DW9q607lEo/edit#gid=1315772191 | 
| Check if occurrenceStatus contains standardized terms | occurrenceStatus does not seem to contain a valid term such as: "present", "absent" | IPTreport\$dtb\$general_issues | Some fields such as occurrenceStatus, must contain only standardised terms. For occurrenceStatus the terms that can be used are "present" and "absent". | 
| Check if scientificNameID resolves to a WoRMS LSID | scientificNameID does not resolve / None of the scientificNameIDs are LSID for WoRMS | IPTreport\$dtb\$general_issues | The scientificNameID field must be a standardised term that resolves to a WoRMS LSID | 
| Check if the required and recommended OBIS fields are present and filled in. | Required field "x" is missing / Empty value for required field "x" / Recommended field "x" is missing / Empty value for recommended field "x" | IPTreport\$dtb\$general_issues | A minimum number of fields are needed for the dataset to have certain coherence, for OBIS some of these fields will be a requirement ("eventDate", "decimalLongitude", "decimalLatitude", "scientificName", "scientificNameID", "occurrenceStatus", "basisOfRecord") and others only highly recommended ("minimumDepthInMeters", "maximumDepthInMeters"). Have a better overview in https://docs.google.com/spreadsheets/d/1mWvg0jyayAs9h0SuuqGgHuenvgOlSKYp1DW9q607lEo/edit#gid=1315772191 | 
| Check if the IPT resource URL is longer than 255 characters | The IPT resource URL seem to be longer than 255 characters | IPTreport\$dtb\$general_issues | A very long IPT url is unnnecessay and can create problems as data flows. | 
| Any missing measurementTypeIDs in the eMoF table (for non occurrence related mofs) | measurementTypeID is missing | IPTreport\$dtb\$mof_issues | For interoperability reasons, the measurementTypeID field should be populated if the term exists in BODC, in case it doesn't it is good practice to request the creation of one. | 
| Check if measurementTypeIDs resolve to BODC terms (for non occurrence related mofs) | measurementTypeID does not resolve | IPTreport\$dtb\$mof_issues | The vocabulary terms used under the measurementTypeID field must resolve to an existing concept, ideally to a concept from BODC | 
| Check if there is a BODC vocab term for "sampling instrument" in the eMoF table | No sampling instrument present | IPTreport\$dtb\$general_issues | It is highly recommended to add sampling related information to a dataset and to use standardised vocabulary terms for interoperability reasons. | 
| Check if there is a BODC vocab term for "sampling descriptors" in the eMoF table | No sampling descriptors present: see http://vocab.nerc.ac.uk/collection/Q01/current/ | IPTreport\$dtb\$general_issues | It is highly recommended to add sampling related information to a dataset and to use standardised vocabulary terms for interoperability reasons. | 
| Any missing measurementTypeIDs in the eMoF table | measurementTypeID is missing | IPTreport\$dtb\$mof_issues | For interoperability reasons, the measurementTypeID field should be populated if the term exists in BODC, in case it doesn't it is good practice to request the creation of one. | 
| Check if occurrence related measurementTypeIDs resolve to BODC terms | measurementTypeID does not resolve | IPTreport\$dtb\$mof_issues | The vocabulary terms used under the measurementTypeID field must resolve to an existing concept, ideally to a concept from BODC | 
| Check if measurementUnitIDs resolve to BODC terms (for non occurrence related mofs) | measurementUnitID does not resolve | IPTreport\$dtb\$mof_issues | The vocabulary terms used under the measurementUnitID field must resolve to an existing concept, ideally to a concept from BODC | 
| Any measurementUnit that may be missing a measurementUnitID | measurementUnits which may need a measurementUnitID | IPTreport\$dtb\$mof_issues | For interoperability reasons, the measurementUnitID field should be populated if the term exists in BODC, in case it doesn't it is good practice to request the creation of one. | 
| Check if occurrence related measurementUnitIDs resolve to BODC terms | measurementUnitID does not resolve | IPTreport\$dtb\$mof_issues | The vocabulary terms used under the measurementUnitID field must resolve to an existing concept, ideally to a concept from BODC | 
| Check if measurementValueIDs resolve to BODC terms (for non occurrence related mofs) | measurementValueID does not resolve | IPTreport\$dtb\$mof_issues | The vocabulary terms used under the measurementValueID field must resolve to an existing concept, ideally to a concept from BODC | 
| Any measurementValues which may be missing a measurementValueID or a Unit | measurementValues which may need a measurementValueID or a Unit | IPTreport\$dtb\$mof_issues | Records in the eMoF table generally refer either to measurements or to facts. If they are measurements, they will not need a measurementValueID since numbers are self explanatory, however, they will likely need a unit to be populated under the measurementUnit field in order to make sense. If they are a fact, units will not be needed for the record to be coherent, however, for interoperability reasons the measurementValueID field should be populated if the term exists in BODC, in case it doesn't it is good practice to request the creation of one. | 
| Check if occurrence related measurementValueIDs resolve to BODC terms | measurementValueID does not resolve | IPTreport\$dtb\$mof_issues | The vocabulary terms used under the measurementValueID field must resolve to an existing concept, ideally to a concept from BODC | 

<br/><br/>

#### ***Visual Checks***

| Quality check | Output location | Why this check? | 
| --- | --- | --------------------- |
| Overview of event and occurrence records | IPTreport\$datasummary | This table shows how many events of a different "type" there are in the dataset, also it shows if these events are linked to occurrences or not, and to how many occurrences they are linked. It shows as well the type (basisOfRecord) of these occurrences and if these occurrences are presences, absences or "NA". This table allows for: i) understanding the type of sampling carried out using the "type" field and already infering some event hierarchy; ii) noticing event records not linked to any occurrences (such as empty samples or environmental samples); iii) assessing if the basisOfRecord is correct and at the hierarchy level that corresponds; iv) noticing if there are occurrences where the occurrenceStatus field hasn't been assigned as well as assessing if the occurrenceStatus field is correctly used. | 
| Overview of measurement or fact records (types and units) | IPTreport\$mofsummary | This table shows all the different measurementTypes in the dataset, together with their measurementValue range if the values are numeric and their measurementUnit, as well as a count of how many records are there for a given measurementType. If the measurementType is linked to a measurementTypeID (vocabulary term code from BODC), the table also shows the name, definition and standard unit of the linked BODC vocabulary term. This table allows for: i) having an overview of the measurementTypes in the dataset; ii) assessing if the value range is realistic for a specific measurementType; iii) assessing if the unit is realistic for a specific measurementType by comparing the measurementUnit used with the standard unit for that measurementTypeID, iv) assessing if the BODC vocabulary term used is equivalent to the measurementType (both in definition and base units) and therefore correctly used; v) noticing if there is a measurementTypeID missing; vi) assessing if the number of records with a specific measurementType is correct by comparing it with the number of events or occurrences from the "Overview of event and occurrence records" table. | 
| Overview of measurement or fact records (standardised values) | IPTreport\$mofsummary_values | This table shows all the different measurementValueIDs in the dataset. It shows also the measurementValues and measurementTypes linked to those measurementValueIDs as well as providing a definition for the measurementValueID used. This table allows for: i) assessing if the measurementValueID used is correct by comparing its definition to the measurementValue used; ii) assessing if the measurementValues used correspond to the measurementType used. | 
| Option to display and download the OBIS tree structure: https://github.com/iobis/obistools/#dataset-structure | - | This tree plot shows the hierarchy relation between the different events, the relations between these events and the linked occurrences and measurement or facts records and the relations between the occurrences and their linked measurements or fact records. This plot can be used for: i) understanding the data collection methodology of the dataset and the relations between events, occurrences and their associated parameters; ii) assessing if some of the linkages between events, occurrences and measurements/facts are correct or not.  | 
| Temporal cover of the dataset | IPTreport\$dates_plot | This barplot shows the temporal range of the dataset's data, as well as showing the temporal coverage of the metadata. This table can be used for:  i) comparing the temporal ranges of metadata and data, they should be equal. | 
| Marine taxa on land | IPTreport\$MarTaxaonLand | This table shows some taxonomy and attributes of the marine taxa whose associated coordinates fall on land. This table can be used for: i) assessing if a given marine taxon found on land may be correct or not, such as in taxa that are both marine and freshwater. | 
| Geographical cover of the dataset | IPTreport\$plot_coordinates | This interactive map shows the geographical distribution of the dataset's events and highlights potential issues with the location (yellow, orange, red dots) or depth (blue  dots) of the records. If no issues are encountered, all the dots will be green. This map can be used for: i) identifying issues in the coordinates, such as having marine species on land; ii) identifying issues with the depth, such as having depths deeper than the maximum depth at that location. | 
| Taxonomic cover of the dataset | IPTreport\$kingdoms | This barplot shows the taxonomic coverage of the dataset's data divided by kingdom and class and based on the LSIDs populated under the scientificNameID field. It can be used for: i) identifying wrong LSIDs in the scientificNameID field such as having the LSID of a seal in a plankton dataset. | 
| Overview unmatched taxa | IPTreport\$dtb\$taxa | This table shows the scientificNames whose scientificNameID does not resolve to a WoRMS LSID or where the scientificNameID has not been filled in. This table can be used for: i) identifying wrongly added scientificNameIDs; ii) identifying taxa for which a WoRMS LSID might not exist. | 



***

### BioCheck Rshiny application
A more user-friendly application has been developed from the EMODnetBiocheck R package, it is currently available from the LifeWatch services at http://rshiny.lifewatch.be/BioCheck/
