#' Checks a one to one correspondence between two fields of a dataframe and highlights records where it is not found.
#'
#' This function highlights records where a one to one relationship is expected between two fields but is not found. 
#' Usually relevant to compare fields that should have one-to-one relationship such as scientificName and scientifciNameID
#' @param df mandatory. a dataframe that contains at least two fields that must have a one to one relationship
#' @param field_x mandatory. a character string containing the name of a field that wants to be compared against another field
#' @param field_y mandatory. a character string containing the name of a field that wants to be compared against another field
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' output <- one_to_one_check(df = Occurrence, field_x = "scientificNameID", field_y = "scientificName")
#' output <- one_to_one_check(df = eMoF, field_x = "measurementTypeID", field_y = "measurementType")
#' output <- one_to_one_check(df = eMoF, field_x = "measurementValueID", field_y = "measurementValue")
#' output <- one_to_one_check(df = eMoF, field_x = "measurementUnitID", field_y = "measurementUnit")
#' output <- one_to_one_check(df = Event, field_x = "institutionID", field_y = "institutionCode")
#' }



one_to_one_check <- function(df, field_x, field_y) {

  
      if(
          is.data.frame(df) &&      # Checks that df is a dataframe
          is.character(field_x) &&  # Checks that field_x is a character string
          is.character(field_y) &&  # Checks that field_y is one of the df fields
          field_x %in% names(df) &&
          field_y %in% names(df)
        ) {          
        
        
                # Isolates the wanted names and ids from the data frame
                unique_combinations <- df %>% select (all_of(c(field_x, field_y))) %>% 
                                              distinct() %>%
                                              filter(!is.na(!!sym(field_x)), 
                                                     !is.na(!!sym(field_y)))
                
                
                # Creates data frame with records where a name is linked to several different ids
                multiple_x_per_y <- unique_combinations %>% group_by(across(all_of(field_y)))  %>%
                                                            summarise(n = n()) %>%
                                                            filter (n > 1) %>%
                                                            left_join (unique_combinations,
                                                                       by = field_y) %>%
                                                            distinct() %>%
                                                            mutate (issue = paste0("multiple ", field_x, " per ", field_y))
                  
                # Creates data frame with records where an id is linked to several different names
                multiple_y_per_x <- unique_combinations %>% group_by(across(all_of(field_x))) %>%
                                                            summarise(n = n()) %>%
                                                            filter (n > 1) %>%
                                                            left_join (unique_combinations,
                                                                       by = field_x) %>%
                                                            distinct() %>%
                                                            mutate (issue = paste0("multiple ", field_y, " per ", field_x))
              
                # Unites previously created data frames
                result <- bind_rows(multiple_x_per_y, multiple_y_per_x) 
    
      } else {
        
        warning("--------> Either the field_x and field_y arguments are not character strings, OR they are not fields in df OR df is not a data frame <--------")
        result <- NULL
        }
  
      return(result) 
  
}


