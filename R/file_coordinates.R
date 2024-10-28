#' Select where to find coordinates for CEDEX file
#' 
#' @description
#' A short description...
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
#' @examples
#' x <- file_coordinates("afliq")
file_coordinates <- function(table_name = NULL) {
  
  
  # Read csv with info.
  coord_file <- utils::read.csv2(".\\data\\files_coordinates.csv", na.strings = "")
  
  
  if (is.null(table_name)) {
    
    # If NULL, return the name of all files.
    x <- coord_file
    
  } else {
    
    # Checks.
    stopifnot("Wrong 'table_name' input" = any(table_name %in% coord_file$file))
    
    # Match 'table' name and file with coordinates.
    x <- coord_file |> dplyr::filter(file == table_name)
    
  }

  
  return(x)

}
