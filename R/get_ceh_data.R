#' Read data from gauging stations of most largest Spanish rivers
#' 
#' @description
#' \code{get_ceh_data} retrieves time series datasets and statistics for
#' several Spanish basins from the Centro de Estudios Hidrológicos (CEX) of
#' the Centro de Estudios y Experimentación de Obras Públicas (CEDEX).
#'
#' @param table_name \code{character} with the name of the file to retrieve from the
#' *CEDEX* site, without extension. If not given, the default value is "estaf".
#' 
#' @param basin_names \code{character} with the name of the basins to retrieve 
#' \code{table_name} names for. Default is to retrieve data for all basins on the *CEDEX* web site.
#'
#' @param sf logical, if TRUE (default), \code{get_ceh_data} returns a \code{sf} spatial object.
#' Coordinate system is always \code{EPSG:32630}, which corresponds to WGS84 / UTM zone 30N.
#' 
#' @param verbose \code{logical}, if set to TRUE progress bars are printed on screen.
#' @param anomes_to_date 
#'
#' @return
#' A spatial \code{sf} object with a EPSG coordinate reference system (unless cs = "utm").
#'
#' @details
#' To see a description of the files to retrieve see
#' "https://ceh.cedex.es/anuarioaforos/demarcaciones.asp" and go to the basin you want
#' to get data for.
#' 
#' The coordinate reference system of the resulting \code{sf} object is \code{UTM 30N} always.
#' There are coordinates in other systems in the original CEDEX files but they are not used
#' (although they are also retrieved and included in the output object).
#'
#' @export
#'
#' @examples
#' # Read afliq.csv data.
#' x <- get_ceh_data(table_name = "afliq")
#' 
get_ceh_data <- function(table_name = "estaf", basin_names = NULL, sf = TRUE, convert_to_date = TRUE, verbose = TRUE) {


  # Check 'table_name'.
  stopifnot("Input 'table_name' must be a single string" = is.character(table_name) & length(table_name) == 1)
  file_name <- table_name |>
    tolower() |>
    file_coordinates()
  
  
  # Check basin names.
  if (!is.null(basin_names)) {
    stopifnot("Input 'basin_names' must be a character vector" = is.character(basin_names) & is.vector(basin_names))
    stopifnot("Wrong input! Did you mean 'miño'?" = any(!("mino" %in% basin_names)))
    basin_names <- basin_names |>
      tolower() |>
      replace_accent()
  }
  basin_names <- basin_names(basin_names)

  
  # cli progress bar update option.
  if (verbose) options(cli.progress_show_after = 0)
  

  # Will 'table_name' actually need coordinates?
  if (is.na(file_name$file_coords)) sf <- FALSE

  
  # Get the full URL for files and check them out.
  url <- "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/"
  url_all <- check_url_files(url, basin_names, file_name, sf, verbose)


  # Reads data for all basins
  z <- NULL
  for (i in 1:nrow(url_all)) {
    
    
    # Progress bar setup, if required.
    if (verbose) {
      if (sf) {
        nbars <- ifelse(url_all$files[i] != url_all$coords[i], 8, 7)
      } else {
        nbars <- 7
      }
      cli::cli_progress_bar(paste0("Downloading data for ", basin_names$name[i], " basin"), total = nbars, clear = FALSE)
      cli::cli_progress_update()
    }


    # Read file with data.
    dat <- read.csv2(url_all$files[i]) |> type.convert(as.is = TRUE)

    
    # If column "anomes" is present, transform to a Date object.
    if (convert_to_date) {
      if ("anomes" %in% tolower(colnames(dat))) {
        dat$fecha <- anomes_to_date(dat$anomes)
      }
      if ("fecha" %in% tolower(colnames(dat))) {
        dat$fecha <- as.Date(dat$fecha, "%d/%m/%Y")
      }
    }
    

    # Read file with coordinates to sites.
    if (sf) {
      if (url_all$files[i] != url_all$coords[i]) {
        if (verbose) cli::cli_progress_update()
        dat_coord <- read.csv2(url_all$coords[i], encoding = "latin1") |> type.convert(as.is = TRUE)
      }
    }


    # Coordinates correction.
    if (sf) {
      if (file_name$file_coords == "estaf") {
        if (verbose) cli::cli_progress_update()
        dat_coord <- correction_coordinates(dat_coord)
      }
    }


    # Coordinates.
    if (sf) {
      if (verbose) cli::cli_progress_update()
      j <- match(dat[, file_name$id_join], dat_coord[, file_name$id_join])
      dat$x <- as.numeric(dat_coord$xutm30[j])
      dat$y <- as.numeric(dat_coord$yutm30[j])
    }


  # sf object.
    if (sf) {
      if (verbose) cli::cli_progress_update()
      dat <- dat |> sf::st_as_sf(coords = c("x", "y"), crs = 32630)
    }
    
    
    # Add field "Cuenca" with name of basin.
    if (verbose) cli::cli_progress_update()
    dat$Cuenca <- basin_names$name[i]


    # Add rows.
    if (verbose) cli::cli_progress_update()
    z <- rbind(z, dat)

  }
  
  if (verbose) cli::cli_end()

  return(z)
}
