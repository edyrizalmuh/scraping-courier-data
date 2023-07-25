main = function(client, business_name, aliases = c(), city, dir){
  all_bussiness_names = append(business_name, aliases)
  all_bussiness_names_encode = URLencode(all_bussiness_names, reserved = TRUE)
  visited_urls = suppressMessages(read_csv(paste0(raw_dir,"visited_urls.csv"))[,1]) %>% pull()
  
  keyword = paste(business_name, city)
  client$navigate(main_url)
  search_box = client$findElement("id", "searchboxinput")
  search_box$clearElement()
  search_box$sendKeysToElement(list(keyword, key = "enter"))
  # client$setTimeout("implicit", 5000)
  Sys.sleep(5)
  
  multiple_results = is_list(client)
  if (multiple_results == FALSE){
    url = client$getCurrentUrl()[[1]]
    if (!url %in% visited_urls){
      data = extract_data(client, url, multiple_results)
      visited_urls = append(visited_urls, url)
    }
  } else if (multiple_results == TRUE){
    not_found = 
      tryCatch({
        suppressMessages(client$findElement("class", "Q2vNVc"))
      }, error = function(e){
        return(NULL) # found search results
      })
    if (is.null(not_found)){
      scroll(client)
      urls = extract_search_url(client)
      urls = urls[str_detect(urls, regex(paste(all_bussiness_names_encode,collapse="|"), ignore_case = T))]
      urls = urls[!urls %in% visited_urls]
      
      # extract the data 
      data = tibble()
      idx_branch = 0
      for (url in urls){
        client$navigate(url)
        layer_button = NULL
        while (is.null(layer_button)){
          layer_button = 
            tryCatch({
              suppressMessages(
                client$findElement("class", "qk5Wte")
              )
            }, error = function(e){
              return(NULL)
            })
        }
        temp = extract_data(client, url, multiple_results)
        data = bind_rows(data, temp)
        rm(temp)
        visited_urls = append(visited_urls, url)
        idx_branch = idx_branch + 1
        cat(paste("\r", city, ": ", idx_branch, " of ", length(urls)))
      }
    } else {
      data = tibble()
    }
  }
  write.csv(data, paste0(dir, city, ".csv"), row.names = FALSE)
  write.csv(visited_urls, paste0(raw_dir,"visited_urls.csv"), row.names = FALSE)
}
