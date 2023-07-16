#' Extract urls of all search results
#'
#' @param client browser
#'
#' @return
#' @export
#'
#' @examples
extract_search_url = function(client){
  search_results = client$findElements("class name", 'hfpxzc')
  urls = c()
  for (idx_result in seq(length(search_results))){
    urls = append(urls, search_results[[idx_result]]$getElementAttribute("href")[[1]])
  }
  return(urls)
}