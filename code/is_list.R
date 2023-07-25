#' Are there multiple search results or is it just a single result?
#' 
#' If there are multiple search results, a scrolling may be needed. If there is 
#' only one results, immidiately extract the data.
#'
#' @param client browser
#'
#' @return
#' @export
#'
#' @examples
is_list = function(client){
  image_pane = 
    tryCatch({
      suppressMessages(
        # client$findElement("xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "lvtCsd", " " ))]//img')
        client$findElement("class", "ZKCDEc")
      )
    }, error = function(e){
      return(NULL)
    })
  # if it does not have image, then it is a list
  return(ifelse(is.null(image_pane), TRUE, FALSE)) 
}



# this is the older approach, but there are some links that have separator 
# despite not being a list of search results
# is_list = function(client){
#   separator =
#     tryCatch({
#       # if a separator exists, then it's a list of multiple results
#       suppressMessages(remDr$findElement("class", "TFQHme"))
#     }, error = function(e){
#       return(NULL)
#     })
#   return(ifelse(!is.null(separator),TRUE,FALSE)) # if separator exists, the result is indeed a list
# }


