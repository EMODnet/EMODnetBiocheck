#' fncols
#'
#' checks if a collumn exists in the dataframe and adds one if not.
#' @param data mandatory parameter, name dataframe
#' @param cname mandatory parameter, collumn name
#' @import RCurl finch dplyr ggplot2
#' @import knitr leaflet xml2 obistools rmarkdown stringr
#' @importFrom stringr str_sub
#' @export


fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  if(length(add)!=0) data[add] <- as.character(NA)
  data
}





substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


substrLeft <- function(x, n){
  substr(x, 1, n)
}



#' get the rightmost characters from a string
#' @param x string to get the characters from
#' @param y string marking where in x we want to start getting characters, excluding y
#' @param n numeric. number of characters not selected from y to the right
#' 
#' @import stringr
#' 
#' @export
#' @examples
#' richtfrom(x = "abcdeT12345", y = "T", n = 2)

richtfrom <- function(x, y, n = 0) {
  substrRight(x, nchar(x)-str_locate(x, y)[1] - n)
}

#' get the characters left from some chars in a string
#' @param x string to get the characters from
#' @param y string marking where in x we want to start getting characters, including y
#' @param n numeric. number of characters not selected y to the left
#' 
#' @import stringr
#' 
#' @export
#' @examples
#' leftfrom(x = "abcdeT12345", y = "T", n = 2)

leftfrom <- function(x, y, n = 0) {
  substr(x, 1, str_locate(x, y)[1] - n)
}


#' remove empty collumns and make all collumns into characters
#' @param x a dataframe
#' @param vector A logical scalar. If TRUE x will be transformed to character dataframe
#' @import parsedate
#' @export

cleandataframe <- function (x, vector = TRUE) {
  
  # format to ISO 8601 if class POSIXct
  for (field in colnames(x)){
    if(TRUE %in% (class(x[[field]]) == "POSIXct")){
      x[[field]] <- format_iso_8601(x[[field]])
    }
  }
  
  # remove empty columns
  x <- x[,colSums(is.na(x))<nrow(x)]
  
  # Transform to character
  if (vector == TRUE ){
    x <- data.frame(lapply(x, as.character), stringsAsFactors=FALSE)
  }
  
  # Function to replace non-ASCII characters
  replace_non_ascii <- function(x) {
    iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  }
  
  
  # turn character NAs and spaces into an actual empty cell
  x <- x %>% mutate(across(where(is.character), ~na_if(., "NA"))) %>%
    mutate(across(where(is.character), ~na_if(., "NULL"))) %>%
    mutate(across(where(is.character), ~na_if(., ""))) %>%
    mutate(across(where(is.character), ~na_if(., " "))) %>%
    mutate(across(where(is.character), replace_non_ascii))

  return (x)
}

#' add "/" to value if missing
#' @param x dataframe. the eMoF table
#' @export

cleanemof <- function (x) {
  
  x <- x %>% fncols(c("occurrenceID", "measurementTypeID","measurementValueID", "measurementValue", "measurementUnitID", "eventID", "measurementUnit")) %>%
      mutate ( # This must only happen to BODC vocabs, Add condition
      measurementTypeID = if_else (!is.na(measurementTypeID) & measurementTypeID != "" ,
               if_else(str_sub(measurementTypeID, -1, -1)=='/',measurementTypeID, paste(measurementTypeID, "/",  sep = '')),
               measurementTypeID) ,
      measurementValueID = if_else (!is.na(measurementValueID) & measurementValueID!="",
                if_else(str_sub(measurementValueID, -1, -1)=='/',measurementValueID, paste(measurementValueID, "/",  sep = '')),
               measurementValueID),
      measurementUnitID = if_else (!is.na(measurementUnitID) & measurementUnitID!="",
                if_else(str_sub(measurementUnitID, -1, -1)=='/',measurementUnitID, paste(measurementUnitID, "/",  sep = '')),
               measurementUnitID)
      )
  x <- x %>% mutate(across(where(is.character), ~na_if(., "NA"))) %>%
             mutate(across(where(is.character), ~na_if(., ""))) %>%
             mutate(across(where(is.character), ~na_if(., " ")))
  
  return(x)
}



#' convert IPT names to vector
#'
#' Function used by loopcheckIPTdataset to convert the imput to a vector.
#' @export
#' @param x mandatory parameter, input



NamesToVector = function (x) {
  if (is.null(names(x)) == TRUE) {
    for (j in x) {
      if ((grepl("resource?", j)==TRUE) | (grepl("archive?", j)==TRUE)) {
        if (exists("lstnames")==TRUE) {
          lstnames <- c(lstnames, substr(j, regexpr('=', j)+1, nchar(j)))
        } else {
          lstnames <- c(substr(j, regexpr('=', j)+1, nchar(j)))
        }
      } else if (grepl(".zip", j)==TRUE) {
        if (exists("lstnames")==TRUE) {
          lstnames <- c(lstnames, substr(j, if ((grepl("/", j)==FALSE) | (grepl("/", j)==FALSE)) {1}
                                         else { if (grepl("/", j)==TRUE) { rev(gregexpr("\\/", j)[[1]])[1] +1   }
                                           else { nchar(j) -12  }
                                         }, regexpr('.zip', j)-1))
        } else {
          lstnames <- c(substr(j, if ((grepl("/", j)==FALSE) | (grepl("/", j)==FALSE)) {1}
                               else { if (grepl("/", j)==TRUE) { rev(gregexpr("\\/", j)[[1]])[1] +1   }
                                 else { nchar(j) -12  }
                               }, regexpr('.zip', j)-1))
        } } }
    x <- structure(x, names=lstnames)
    rm(lstnames)
    return(x)
  }
}


#' find and select charcters in a string
#'
#' Function and select charcters in a string
#' @export
#' @param x mandatory parameter, the input string
#' @param first mandatory parameter, the character before the text you need
#' @param last mandatory parameter, the character after the text you need
#' @param n optional paramter, the offset to first
#' @param m optional paramter, the offset to last
#' 
#' @import stringr
#' 


midstring <- function (x, first, last, n=0, m=0) {
  substring (x, str_locate(x, first)[[2]] + n, str_locate(x, last)[[1]] + m )}




#' Checks if an url exist
#'
#' Checks if an url exist better than the url.exist function
#' @export
#' @param url_in mandatory parameter, the input string
#' @param t optional parameter, the timeout of the open.connection


valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}




#' txt_to_cols separates a one-column dataframe into a multiple column dataframe based on a separator
#' @param x dataframe to transform
#' @param sep separator to be used to separate the dataframe into multiple columns
#' 
#' 
#' @export



txt_to_cols <- function(x, sep = "(,)(?=(?:[^\"]|\"[^\"]*\")*$)"){

  if (!class(x) %in% "data.frame"){
  warning("The argument x must be a dataframe")}

#get header of dataframe x
x_header <- colnames(x)
#split header into vector
x_header <- strsplit(x_header, sep, perl = T)
#make vector into list
x_header <- unlist(x_header)



#make new dataframe that has x_header as column names
new_data_frame <- data.frame(matrix(NA, nrow = nrow(x), ncol = length(x_header)))

#go over each row in x and split the value by "sep" and put it in new_data_frame
for (i in 1:nrow(x)) {
  all_parts_row <- strsplit(as.character(x[i,]), sep, perl = T)
  
  for(y in 1:length(all_parts_row[[1]])){
    new_data_frame[i, y] <- all_parts_row[[1]][y]
  }
}


#change colnames of the new_data_frame to x_header
colnames(new_data_frame) <- x_header
#change to NA all empty or NA cells
new_data_frame[new_data_frame =='NA' | new_data_frame =='' | new_data_frame ==' '] <- NA

return(new_data_frame)

}

#Check if occurrenceID in extension exists in occurrence core (code adapted from obistools::check_extension_eventids())
check_extension_occurrenceids <- function(occurrence, extension, field = "occurrenceID") {
  rows <- which(!extension[[field]] %in% occurrence$occurrenceID)
  if (length(rows) > 0) {
    return(data.frame(
      field = field,
      level = "error",
      row = rows,
      message = paste0(field, " ", extension[[field]][rows], " has no corresponding occurrenceID in the occurrence table"),
      stringsAsFactors = FALSE
    ))
  } else {
    return(tibble())
  }
}

#check if required / highly recommended fields are present in the dataset
check_required_fields_dna <- function(occ, dna) {
  if(is.null(occ$basisOfRecord)){
    stop("basisOfRecord is missing")
  } else {
    if(all(occ$basisOfRecord == "materialSample")){
      #if materialSample, dna derived data is assumed
      required_fields <- c("associatedSequences", "target_gene")
      highly_recommended_fields <- c("DNA_sequence", "organismQuantity","organismQuantityType","sampleSizeValue","sampleSizeUnit","identificationRemarks","identificationReferences","taxonConceptID","materialSampleID","pcr_primer_forward","pcr_primer_reverse","pcr_primer_name_forward","pcr_primer_name_reverse","pcr_primer_reference","seq_meth","otu_class_appr")
    } else {
      #if not, enriched occurrence is assumed
      required_fields <- c("target_gene")
      highly_recommended_fields <- c("DNA_sequence", "associatedSequences", "pcr_primer_forward","pcr_primer_reverse","pcr_primer_name_forward","pcr_primer_name_reverse","pcr_primer_reference","seq_meth")
    }
  }
  missing_fields_req <- data.frame(fields = setdiff(required_fields, c(names(occ), names(dna))), status = if(length(setdiff(required_fields, c(names(occ), names(dna)))) > 0) "Required" else character(0))
  missing_fields_rec <- data.frame(fields = setdiff(highly_recommended_fields, c(names(occ), names(dna))), status = if(length(setdiff(highly_recommended_fields, c(names(occ), names(dna)))) > 0) "Highly recommended" else character(0))
  missing_fields <- rbind(missing_fields_req, missing_fields_rec)
  #taxonID is highly recommended if DNA_sequence is not provided
  if("DNA_sequence" %in% missing_fields$fields) {
    if(!"taxonID" %in% names(occ)) {
      missing_fields <- rbind(missing_fields, data.frame(fields= "taxonID", status="Highly recommended"))
    }
  }
  if (nrow(missing_fields) > 0) {
    return(data.frame(
      field = missing_fields$fields,
      level = "error",
      row = NA,
      message = paste0(missing_fields$status, " field '", missing_fields$fields, "' is missing"),
      stringsAsFactors = FALSE
    ))
  } else {
    return(tibble())
  }
}

#check content of DNA related fields
check_content_dna_fields <- function(occ, dna){
  error <- tibble()
  if("organismQuantityType" %in% names(occ) && any(occ$organismQuantityType != "DNA sequence reads")){
    error <- bind_rows(error, tibble(
        field = "organismQuantityType",
        level = "warning",
        row = which(occ$organismQuantityType != "DNA sequence reads"),
        message = "organismQuantityType must be 'DNA sequence reads'"
    ))
  }
  if("sampleSizeUnit" %in% names(occ) && any(occ$sampleSizeUnit != "DNA sequence reads")){
    error <- bind_rows(error, tibble(
      field = "sampleSizeUnit",
      level = "warning",
      row = which(occ$sampleSizeUnit != "DNA sequence reads"),
      message = "sampleSizeUnit must be 'DNA sequence reads'"
    ))
  }
  # DNA_sequence should only contain A,C,G,T,R,Y,K,M,S,W,B,D,H,V,N (nucleotides or ambiguity codes)
  if("DNA_sequence" %in% names(dna)){
    invalid_dna_sequences <- which(!grepl("^[ACGTRYKMSWBDHVN]*$", dna$DNA_sequence, ignore.case = TRUE))
    if(length(invalid_dna_sequences) > 0){
      error <- bind_rows(error, tibble(
        field = "DNA_sequence",
        level = "error",
        row = invalid_dna_sequences,
        message = "DNA_sequence contains invalid characters. Only A,C,G,T,R,Y,K,M,S,W,B,D,H,V,N are allowed."
      ))
    }
  }
  return(error)
}