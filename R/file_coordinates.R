file_coordinates <- function(table_name = NULL) {
  
  
  # Read csv with info.
  coord_file <- read.csv2(".\\data-raw\\files.csv", na.strings = "")
  
  
  if (is.null(table_name)) {
    
    # If NULL, return the name of all files.
    fi <- coord_file$file
    
  } else {
    
    # Checks.
    stopifnot("Wrong 'table' name" = any(table_name %in% coord_file$file))
    
    
    # Match 'table' name and file with coordinates.
    fi <- coord_file |> dplyr::filter(file == table_name)
    
  }

  
  return(fi)

}
