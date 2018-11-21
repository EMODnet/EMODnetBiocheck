
  reversetaxmatch = function(aphiaIDs) {
    result <- NULL
    
    grab_data     <-    function(webserviceurls) {
      out <- tryCatch(
        {rjson::fromJSON(file = webserviceurls)}, 
        error=function(x) {""})
      return(out)
    }
    
    
    aphiaIDsper50 <-  split(aphiaIDs, ceiling(seq_along(aphiaIDs)/50))
    
    for (i in aphiaIDsper50) { 
    
      for (n in i) { 
        if (!is.na(n)) {
        if (n == i[1]) {  webserviceurls <- paste0("http://marinespecies.org/rest/AphiaRecordsByAphiaIDs?aphiaids[]=",n)
        } else { webserviceurls <- paste0 (webserviceurls , paste0("&aphiaids[]=", n))
        }}}  
      resultchunck <- grab_data(webserviceurls)
      
      result <- c(result,resultchunck )
      
    }
    
    non.null.list <- lapply(result, lapply, function(x)ifelse(is.null(x), NA, x))
    reversematch <-suppressWarnings(plyr::rbind.fill(lapply(non.null.list, as.data.frame,stringsAsFactors = F)) %>%  select(
      lsid, scientificNameMatch = scientificname,  url, status,  scientificNameaccepted = valid_name, rank,  kingdom ,  class, 
      family, isMarine, isBrackish, isFreshwater, isTerrestrial, isExtinct))
    reversematch <- reversematch %>% filter(!is.na(lsid))
    
    return(reversematch)
    
  }
  
  