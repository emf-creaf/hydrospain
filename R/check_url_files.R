#' Check URLs and return full http addresses.
#' 
#' @description
#' \code{check_url_files} verifies that the requests to 'file_name' files are ok.
#' 
#' @param url a valid URL.
#' 
#' @param file_name \code{data.frame} of one row and three columns named 'file', 'file_coords' and 'id_join',
#' *CEDEX* site, without extension.
#' 
#' @param basin_nam \code{character} with the name of the basins to retrieve 
#' \code{table_name} names for.
#' 
#' @param timeout positive integer specifying the timeout for some Internet operations, in seconds.
#' Default is 120 seconds. Depending on the bandwidth of your internet connection or on the state of the
#' CEDEX servers you may have to set a \code{timeout} value longer than 120.
#' 
#' @param sf logical, if TRUE (default), \code{hydrospain} returns a \code{sf} spatial object.
#' Coordinate system is always \code{EPSG:32630}, which corresponds to WGS84 / UTM zone 30N.
#' 
#' @param verbose \code{logical}, if set to TRUE progress bars are printed on screen.
#'
#' @return Full url for files.
#' 
#' @details
#' 'file_name' will be pasted to the input 'URL' and a status request will be sent.
#' 
#' @examples
#' url <- "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/"
#' file_name <- data.frame(file = "afliq", file_coords = "estaf", id_join = "indroea")
#' check_url_files(url, file_name, "ebro")
#' 
#' @noRd
#'
check_url_files <- function(url, file_name, basin_nam, timeout = 120, sf = TRUE, verbose = TRUE) {

  
  # Check basins.
  basin_nam <- select_basins(basin_nam)
  nbasin <- length(basin_nam$nameceh)
    
  
  # cli progress bar update option.
  if (verbose) {
    options(cli.progress_show_after = 0)
    cli::cli_progress_bar(paste0("Checking URL status"), total = nbasin + 7, clear = FALSE)
    cli::cli_progress_update()
  }
  
  
  # Setting timeout.
  options(timeout = timeout)
  
  
  # Initialize variables.
  url_files <- url_status_files <- NULL
  if (sf) url_coords <- url_status_coords <- NULL
  if (verbose) cli::cli_progress_update()

  
  #  Main loop to get URL status.
  for (i in 1:nbasin) {

    x <- basin_nam$nameceh[i]
    url_files <- c(url_files, paste0(url, x, "/", file_name$file, ".csv"))
    if (sf) url_coords <- c(url_coords, paste0(url, x, "/", file_name$file_coords, ".csv"))
    url_status_files <- c(url_status_files, status_code(url_files[i]))


    if (sf) url_status_coords <- c(url_status_coords, status_code(url_coords[i]))
    if (verbose) cli::cli_progress_update()
  }

  
  # Only interested in successful status.
  check_status_files <- url_status_files >= 200 & url_status_files < 300
  if (sf) check_status_coords <- url_status_coords >= 200 & url_status_coords < 300
  if (verbose) cli::cli_progress_update()
  

  # If not all checks are successful, abort.
  if (!all(check_status_files)) {
    msg1 <- paste0("URL connection for file ", file_name$file, " was not successful for the following basins: ",
                   paste0(basin_nam$name[!check_status_files], collapse = ", "))
  }
  if (verbose) cli::cli_progress_update()
  
  if (sf) {
    if (!all(check_status_coords)) {
      msg2 <- paste0("URL connection for coordinate file ", file_name$file_coords, " was not successful for the following basins: ",
                     paste0(basin_nam$name[!check_status_coords], collapse = ", "))
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
  if (verbose) {
    cli::cli_progress_update()
    cli::cli_end()
  }
  
  
  return(df)
  
}


