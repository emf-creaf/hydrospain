#' Check URLs and return full http addresses.
#'
#' @param url a valid URL.
#' @param basin_names its correctness is not checked.
#' @param file_name 
#' @param sf 
#' @param verbose 

#'
#' @return Full url for files.
#' @keywords internal
#'
check_url_files <- function(url, basin_names, file_name, sf, verbose) {

  
  # Number of basins.
  nbasin <- length(basin_names$nameceh)
  
    
  # cli progress bar update option.
  if (verbose) {
    options(cli.progress_show_after = 0)
    cli::cli_progress_bar(paste0("Checking URL status"), total = nbasin + 7, clear = FALSE)
    cli::cli_progress_update()
  }
  
  
  # Initialize variables.
  url_files <- url_status_files <- NULL
  if (sf) url_coords <- url_status_coords <- NULL
  if (verbose) cli::cli_progress_update()
  
  
  #  Main loop to get URL status.
  for (i in 1:nbasin) {

    x <- basin_names$nameceh[i]
    
    url_files <- c(url_files, paste0(url, x, "//", file_name$file, ".csv"))
    if (sf) url_coords <- c(url_coords, paste0(url, x, "//", file_name$file_coords, ".csv"))
    
    url_status_files <- c(url_status_files, httr::GET(url_files[i])$status_code)
    if (sf) url_status_coords <- c(url_status_coords, httr::GET(url_coords[i])$status_code)
    
    if (verbose) cli::cli_progress_update()
  }

  
  # Only interested in successful status.
  check_status_files <- url_status_files >= 200 & url_status_files < 300
  if (sf) check_status_coords <- url_status_coords >= 200 & url_status_coords < 300
  if (verbose) cli::cli_progress_update()
  

  # If not all checks are successful, abort.
  if (!all(check_status_files)) {
    msg1 <- paste0("URL connection for file ", file_name$file, " was not successful for the following basins: ",
                   paste0(basin_names$name[!check_status_files], collapse = ", "))
  }
  if (verbose) cli::cli_progress_update()
  
  if (sf) {
    if (!all(check_status_coords)) {
      msg2 <- paste0("URL connection for coordinate file ", file_name$file_coords, " was not successful for the following basins: ",
                     paste0(basin_names$name[!check_status_coords], collapse = ", "))
    }
  }
  if (verbose) cli::cli_progress_update()
  
  if (sf) {
    msg <- c(ifelse(exists("msg1"), msg1, ""), ifelse(exists("msg2"), msg2, ""))
    if (any(msg != "")) cli::cli_abort(msg)
  } else {
    if (exists("msg1")) cli::cli_abort(msg1)
  }
  if (verbose) cli::cli_progress_update()

  
  df <- data.frame(files = url_files)
  if (sf) df <- data.frame(df, coords = url_coords)
  if (verbose) cli::cli_progress_update()
  
  
  return(df)
  
}


