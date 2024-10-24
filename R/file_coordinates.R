file_coordinates <- function(table_name = NULL) {
  
  
  # Read csv with info.
  coord_file <- read.csv2(".\\data\\files_coordinates.csv", na.strings = "")
  
  
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
