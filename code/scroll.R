#' Scroll the search results
#'
#' @return
#' @export
#'
#' @examples
scroll = function(client){
  search_list = client$findElements("xpath", '//*[@id="QA0Szd"]/div/div/div[1]/div[2]/div/div[1]/div/div/div[2]/div[1]')
  current_results = search_list[[1]]$getElementText()[[1]]
  end = NULL
  i = 0
  while (is.null(end)){
    last_results = current_results
    search_list[[1]]$sendKeysToElement(list(key = "page_up"))
    search_list[[1]]$sendKeysToElement(list(key = "page_up"))
    search_list[[1]]$sendKeysToElement(list(key = "end"))
    Sys.sleep(1)
    client$setTimeout("implicit", 0)
    end = 
      tryCatch({
        suppressMessages(client$findElement("class", "HlvSq"))
      }, error = function(e){
        return(NULL)
      })
    
    # if the results are the same as previous results (before scrolling) for 10 
    # consecutive times, then refresh
    current_results = client$findElements("xpath", '//*[@id="QA0Szd"]/div/div/div[1]/div[2]/div/div[1]/div/div/div[2]/div[1]')[[1]]$getElementText()[[1]]
    if (current_results == last_results){i = i + 1} else {i = 0} # add retry if the results are same, reset if not
    if (i == 30){
      client$refresh()
      current_results = client$findElements("xpath", '//*[@id="QA0Szd"]/div/div/div[1]/div[2]/div/div[1]/div/div/div[2]/div[1]')[[1]]$getElementText()[[1]]
      search_list = client$findElements("xpath", '//*[@id="QA0Szd"]/div/div/div[1]/div[2]/div/div[1]/div/div/div[2]/div[1]')
      i = 0
    }
  }
}

