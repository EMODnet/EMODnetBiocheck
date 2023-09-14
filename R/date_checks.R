

#' Identifies dates that are in the future.
#'
#' This function highlights dates that are in the future and flag them as errors
#' @param x mandatory. a dataframe that contains a field called eventDate
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' output <- suspicious_old_date(x = Occurrence)
#' output <- suspicious_old_date(x = Event)
#' }


date_in_future <- function (x) {


  future_dates <- x %>% mutate (level = "error",
                                field = "eventDate",
                                row = row_number(),
                                message = paste0("eventDate = ", eventDate, " is in the future")) %>%
                        filter (eventDate > Sys.Date()) %>%
                        select (level, field, row, message)


  return(future_dates)

  
}


#' Identifies dates that are older than a threshold and flag them as suspicious.
#'
#' This function highlights dates that are very old
#' @param x mandatory. a dataframe that contains a field called eventDate
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' output <- suspicious_old_date(x = Occurrence)
#' output <- suspicious_old_date(x = Event)
#' }
  
  
suspicious_old_date <- function (x) {    

  
  suspicious_dates <- x %>% mutate (level = "warning",
                                    field = "eventDate",
                                    row = row_number(),
                                    message = paste0("eventDate = ", eventDate, " is suspiciously old")) %>%
                            filter (eventDate < as.Date("1100-01-01")) %>% # Threshold, change it to whatever we want
                            select (level,field, row ,message)
                     

  return(suspicious_dates)
  
  
  }



