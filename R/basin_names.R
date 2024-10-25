#' Provide basin names for the CEDEX gauging station data
#' 
#' @description
#' \code{basin_names} retrieves basin names for CEDEX
#' 
#'
#' @param basin_nam \code{character} with the name of the basins to retrieve 
#' \code{table_name} names for. Default is to retrieve data for all basins on the *CEDEX* web site. 
#'
#' @return
#' @examples
basin_names <- function(basin_nam = NULL) {
  
  
  # Read csv with info.
  all_basins <- read.csv2(".\\data\\basin_names.csv", na.strings = "")

  
  if (is.null(basin_nam)) {
    
    # If NULL, return the name of all files.
    x <- all_basins
    
  } else {
    
    # Checks.
    basin_nam <- tolower(basin_nam)
    stopifnot("Wrong 'basin' input" = any(basin_nam %in% all_basins$name))
    all_basins <- all_basins[match(basin_nam, all_basins$name), ]
  }
  
  
  return(all_basins)
  
}
