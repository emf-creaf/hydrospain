#' Title
#'
#' @param basin 
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
    x <- all_basins |>
      dplyr::filter(name == basin_nam)
  }
  
  
  return(x)
  
}
