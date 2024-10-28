#' Names of Spanish basins for the CEDEX gauging station data
#' 
#' @description
#' \code{basin_names} retrieves basin names where gauging stations belonging to the CEDEX network
#' can be found.#' 
#'
#' @param basin_nam \code{character} with the name of the basins to retrieve 
#' \code{table_name} names for. Default is to retrieve data for all basins on the *CEDEX* web site. 
#'
#' @return
#' A \code{data.frame} with a column named "name" that contains the names of the basins, and a second
#' column named "nameceh" with the basin names required for the web site.
#' 
#' @importFrom utils read.csv2
#' 
#' @examples
#' df <- basin_names(c("duero", "EbrO"))
basin_names <- function(basin_nam = NULL) {
  
  
  # Read csv with info.
  all_basins <- read.csv2(".\\data\\basin_names.csv", na.strings = "")

  
  if (!is.null(basin_nam)) {
    
    # Checks.
    stopifnot("Input 'basin_nam' must be a character vector" = is.character(basin_nam) & is.vector(basin_nam))

    basin_nam <- tolower(basin_nam)
    stopifnot("Wrong 'basin' input" = any(basin_nam %in% all_basins$name))
    all_basins <- all_basins[match(basin_nam, all_basins$name), ]
    rownames(all_basins) <- 1:nrow(all_basins)
    
  }
  
  
  return(all_basins)
  
}
