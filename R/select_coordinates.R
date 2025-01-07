#' Select where to find coordinates for CEDEX file
#' 
#' @description
#' \code{select_coordinates} indicates where to find spatial coordinates for \code{table_name}.
#' 
#'
#' @param table_name \code{character} with the name of the file to retrieve from the
#' *CEDEX* site, without extension. If not given, the default value is "estaf".
#'
#' @return
#' A \code{data.frame} of one row only with three fields: "file", containing the value
#' of the "table_name" input, "file_coords", indicating the name of the CEDEX file where
#' coordinates will be found, and "id_join", mentioning which key can be used for left-joins.
#' 
#' @importFrom utils read.csv2
#' 
#' @keywords internal
#' 
#' @examples
#' x <- select_coordinates("afliq")
#' 
select_coordinates <- function(table_name = "estaf") {

  if (!is.null(table_name)) {

    # # Checks.
    stopifnot("Wrong 'table_name' input" = any(table_name %in% file_coordinates$file))

    # # Match 'table' name and file with coordinates.
    file_coordinates <- file_coordinates[file_coordinates$file == table_name,]
    
  }

  
  return(file_coordinates)

}
