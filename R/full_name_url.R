#' Return full URL addresses.
#' 
#' @description
#' \code{full_name_url} builds full URL addresses
#' 
#' @param url a valid URL.
#' 
#' @param file_name \code{data.frame} of one row and three columns named 'file', 'file_coords' and 'id_join',
#' *CEDEX* site, without extension.
#' 
#' @param basin_nam \code{character} with the name of the basins to retrieve 
#' \code{table_name} names for.
#' 
#' @param sf logical, if TRUE (default), 
#' 
#' @return \code{data.frame} with one column named 'files' containing the full url address for files.
#' if \code{sf} is TRUE, a second column named 'coord' is added that includes the full url address
#' of the files where coordinates will be found.
#' 
#' @details
#' 'file_name' will be pasted to the input 'URL' on output.
#' 
#' @examples
#' url <- "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/"
#' file_name <- data.frame(file = "afliq", file_coords = "estaf", id_join = "indroea")
#' x <- full_name_url(url, file_name, "ebro")
#' 
#' @export
#' @keywords internal
#'
full_name_url <- function(url, file_name, basin_nam, sf = TRUE) {

  
  # Check basins.
  basin_nam <- select_basins(basin_nam)
  nbasin <- length(basin_nam$nameceh)
    

  # Initialize variables.
  url_files <- NULL
  if (sf) url_coords <- NULL

  
  #  Main loop to get URL status.
  for (i in 1:nbasin) {
    x <- basin_nam$nameceh[i]
    url_files <- c(url_files, paste0(url, x, "/", file_name$file, ".csv"))
    if (sf) url_coords <- c(url_coords, paste0(url, x, "/", file_name$file_coords, ".csv"))
  }


  # Compose names.
  df <- data.frame(files = url_files)
  if (sf) df <- data.frame(df, coords = url_coords)

  
  return(df)
  
}


