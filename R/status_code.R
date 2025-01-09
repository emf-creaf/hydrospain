#' Status code of a HTTP or HTTPS request
#' 
#' @description
#' \code{status_code} uses the \code{GET} functions from the \code{httr} package to 
#' return the status code of the connection.
#' 
#' @param url \code{character} string with the URL address.
#'
#' @return
#' Status code of the response to the issued request.
#' 
#' @export
#' @keywords internal
#'
#' @importFrom httr GET
#' 
#' @examples
#' \donttest{
#'   status_code("https://www.google.com")
#' }
#' 
status_code <- function(url) {
  
  # Checks.
  stopifnot("Input 'url' must be a character string"  = is.character(url))
  stopifnot("Input 'url' must start with 'http://' or 'https://'" = 
              grepl("http://", url) | grepl("https://", url))
  

  stat_code <- httr::GET(url)$status_code
  
  
  return(stat_code)
}