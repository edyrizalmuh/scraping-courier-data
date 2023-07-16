#' Extract data from a page
#'
#' @param client browser
#' @param url url from search list (for multiple results) or url with latitude 
#'        longitude (for single search result)
#'
#' @return
#' @export
#'
#' @examples
extract_data = function(client, url, multiple_results){
  # Extract the coordinate url while waiting for the load to complete  -------
  
  if (multiple_results == TRUE){
    lat_detect = FALSE
    long_detect = FALSE
    while ((lat_detect == FALSE) | (long_detect == FALSE)){ # wait until the url changes to coordinate url
      coord_url = client$getCurrentUrl()[[1]]
      lat_detect = coord_url %>% str_detect("(?<=@)[0-9.-]+(?=,)")
      long_detect = coord_url %>% str_detect("(?<=,)[0-9.-]+(?=,)")
    }
  } else (
    coord_url = url
  )
  

  # Extract other data ------------------------------------------------------
  
  branch_name =
    tryCatch({
      suppressMessages(
        client$findElement("class", "fontHeadlineLarge")$getElementText()[[1]]
      )
    }, error = function(e){
      return(NA)
    })
  
  rating = 
    tryCatch({
      suppressMessages(
        client$findElement("xpath", '//*[@id="QA0Szd"]/div/div/div[1]/div[2]/div/div[1]/div/div/div[2]/div/div[1]/div[2]/div/div[1]/div[2]/span[1]/span[1]')$getElementText()[[1]]
      ) %>%
        str_replace(",",".") %>%
        as.numeric()
    }, error = function(e){
      return(NA) # temporary or permanently closed place don't have rating or num_review
    })
  
  num_review = 
    tryCatch({
      suppressMessages(
        client$findElement("xpath", '//*[@id="QA0Szd"]/div/div/div[1]/div[2]/div/div[1]/div/div/div[2]/div/div[1]/div[2]/div/div[1]/div[2]/span[2]/span/span')$getElementText()[[1]] 
      ) %>%
        str_extract("[0-9]+") %>%
        as.integer()
    }, error = function(e){
      return(NA) # temporary or permanently closed place don't have rating or num_review
    })
  
  details =
    tryCatch({
      details_list = suppressMessages(client$findElements("class name", "CsEnBe"))
      details = sapply(1:(length(details_list)), function(x) details_list[[x]]$getElementAttribute("aria-label")[[1]])
      details = 
        details[details %>% str_detect(":")] %>% 
        as_tibble() %>%
        mutate(
          detail = str_extract(value, "[A-Za-z ]+(?=:)"),
          value = str_extract(value, "(?<=: ).+") %>% str_trim()
        ) %>%
        relocate(detail)
    }, error = function(e){
      return(NA)
    })
  
  status = 
    tryCatch({
      suppressMessages(
        client$findElement("class", "o0Svhf")$getElementText()[[1]]
      )
    }, error = function(e){
      return("unknown")
    })
  
  longitude = coord_url %>% str_extract("(?<=,)[0-9.-]+(?=,)")
  latitude = coord_url %>% str_extract("(?<=@)[0-9.-]+(?=,)")
  
  
  
  # Output ------------------------------------------------------------------
  return(
    tibble(
      branch_name = branch_name,
      rating = rating,
      num_review = num_review,
      status = status,
      search_url = url,
      coord_url = coord_url,
      longitude = longitude,
      latitude = latitude
    ) %>% 
      bind_cols(details)
  )
  
}
