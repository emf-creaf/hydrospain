#' Names of Spanish basins for the CEDEX gauging station data
#' 
#' @description
#' \code{select_basins} retrieves basin names where gauging stations belonging to the CEDEX network
#' can be found.#' 
#'
#' @param basin_nam \code{character} with the name of the basins to retrieve 
#' \code{table_name} names for. Default is to retrieve data for all basins on the *CEDEX* web site. 
#'
#' @return
#' A \code{data.frame} with a column named "name" that contains the names of the basins, and a second
#' column named "nameceh" with the basin names required for the web site.
#' 
#' @details
#' It uses internal package data, so it will only work when used by other package functions.
#' 
#' @export
#' @keywords internal
#' 
#' @examples
#' df <- select_basins(c("duero", "EbrO"))
#' 
select_basins <- function(basin_nam = NULL) {
  
  if (!is.null(basin_nam)) {
    
    # Checks.
    stopifnot("Input 'basin_nam' must be a character vector" = is.character(basin_nam) & is.vector(basin_nam))

    basin_nam <- tolower(basin_nam)
    stopifnot("Wrong 'basin' input" = all(basin_nam %in% basin_names$name))
    basin_names <- basin_names[match(basin_nam, basin_names$name), ]
    rownames(basin_names) <- 1:nrow(basin_names)

  }

  return(basin_names)
  
}
