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
  
  for (field in colnames(x)){
    if(TRUE %in% (class(x[[field]]) == "POSIXct")){
      x[[field]] <- format_iso_8601(x[[field]])
    }
   # format to ISO 8601 if class POSIXct
  }
  
  x <- x %>% mutate(across(where(is.character), ~na_if(., "NA"))) %>%
             mutate(across(where(is.character), ~na_if(., ""))) %>%
             mutate(across(where(is.character), ~na_if(., " ")))
  
  
  x <- x[,colSums(is.na(x))<nrow(x)]
  
  if (vector == TRUE ){
    x <- data.frame(lapply(x, as.character), stringsAsFactors=FALSE)
  }

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



txt_to_cols <- function(x, sep = ","){

  if (!class(x) %in% "data.frame"){
  warning("The argument x must be a dataframe")}

#get header of dataframe x
x_header <- colnames(x)
#split header into vector
x_header <- strsplit(x_header, sep)
#make vector into list
x_header <- unlist(x_header)



#make new dataframe that has x_header as column names
new_data_frame <- data.frame(matrix(NA, nrow = nrow(x), ncol = length(x_header)))

#go over each row in x and split the value by ; and put it in new_data_frame
for (i in 1:nrow(x)) {
  all_parts_row <- strsplit(as.character(x[i,]), sep)
  
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
