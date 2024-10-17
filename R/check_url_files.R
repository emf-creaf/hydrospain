#' Check URLs and return full http addresses.
#'
#' @param url a valid URL.
#' @param basin a character vector containing the names of the basins to download the data for.
#' @param table a character string indicating which table to download.
#'
#' @return Full url for files.
#' @keywords internal
#'
check_url_files <- function(url, basin, table, verbose) {

  # Basin names in upper case for web site.
  upper_basin_names <- basin_name(basin)

  # Paste basin names to table name.
  url_files <- sapply(upper_basin_names, function(x) paste0(url, x, "//", table, ".csv"), USE.NAMES = FALSE)

  # Get URL status.
  if (verbose) {
    url_status <- sapply(cli::cli_progress_along(1:length(url_files),
                                                 paste0("Checking URL for all files '", table, "'"), clear = F),
                         function(i) httr::GET(url_files[i])$status_code)
  } else {
    url_status <- sapply(url_files, function(x) httr::GET(x)$status_code)
  }
  
  # Only interested in successful status.
  check_status <- url_status >= 200 & url_status < 300
  
  # If some are not successful, abort.
  if (!all(check_status)) {
    cli::cli_abort(paste0("URL connection for table ", table, " was not successful for the following basins: ",
                          paste0(basin[!check_status], collapse = ", ")))
  }
  
  return(url_files)
  
}


