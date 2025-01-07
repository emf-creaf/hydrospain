#' Status code of a HTTP or HTTPS request
#' 
#' @description
#' \code{status_code} uses functions from the \code{httr2} package to check and
#' return the status code of the connection.
#' 
#' @param url \code{character} string with the URL address.
#'
#' @return
#' Status code of the response to the issued request.
#' 
#' @keywords internal
#'
#' @importFrom httr2 request req_perform resp_status
#' 
#' @examples
#' status_code("http://r-project.org")
#' 
status_code <- function(url) {
  
  # Checks.
  stopifnot("Input 'url' must be a character string"  = is.character(url))
  stopifnot("Input 'url' must start with 'http://' or 'https://'" = 
              grepl("http://", url) | grepl("https://", url))
  

  stat_code <- url |> httr2::request() |> httr2::req_perform() |> httr2::resp_status()
  
  
  return(stat_code)
}