#' Read data from gauging stations of most largest Spanish rivers
#' 
#' @description
#' \code{hydrospain} retrieves time series datasets and statistics for
#' several Spanish basins from the Centro de Estudios Hidrológicos (CEX) of
#' the Centro de Estudios y Experimentación de Obras Públicas (CEDEX).
#'
#' @param file_name \code{character} string with the name of the file to retrieve from the
#' *CEDEX* site, without extension. If not given, the default value is "estaf". More than one name is
#' not allowed.
#' 
#' @param basin_nam \code{character} with the name of the basins to retrieve 
#' \code{table_name} names for. Default is to retrieve data for all basins on the *CEDEX* web site.
#' 
#' @param timeout positive integer specifying the timeout for some Internet operations, in seconds.
#' Default is 120 seconds. Depending on the bandwidth of your internet connection or on the state of the
#' CEDEX servers you may have to set a \code{timeout} value longer than 120.
#' 
#' @param first_day numeric or character \code{vector} containing the day number(s) that will be set when
#' building the \code{date} object.
#'
#' @param sf logical, if TRUE (default), \code{hydrospain} returns a \code{sf} spatial object.
#' Coordinate system is always \code{EPSG:32630}, which corresponds to WGS84 / UTM zone 30N.
#' 
#' @param verbose \code{logical}, if set to TRUE progress bars are printed on screen.
#'
#' @return
#' A spatial \code{sf} object with a WGS84/UTM zone 30N coordinate reference system (EPSG:32630).
#'
#' @details
#' For a description of the files to retrieve, check
#' "https://ceh.cedex.es/anuarioaforos/demarcaciones.asp" and click on the basin name for which you want
#' to fetch data.
#' 
#' \code{hydrospain} will add UTM30 X-Y coordinates to gauging station data.
#' The coordinate reference system of the resulting \code{sf} object is, thus, \code{UTM 30N} always.
#' There are coordinates in other reference systems in the original CEDEX files tables, although they are
#' retrieved but not used.
#' 
#' Available basin names, as of Nov. 2024, are the following:
#' | **basin_nam**  | **CEDEX full basin name**           |
#' |----------------|---------------------------          |
#' | galicia        | AUGAS DE GALICIA – XUNTA DE GALICIA |
#' | cantabrico     | C.H. CANTABRICO                     |
#' | duero          | C.H. DUERO                          |
#' | ebro           | C.H. EBRO                           |
#' | guadalquivir   | C.H. GUADALQUIVIR                   |
#' | guadiana       | C.H. GUADIANA                       |
#' | jucar          | C.H. JUCAR                          |
#' | mino           | C.H. MIÑO-SIL                       |
#' | segura         | C.H. SEGURA                         |
#' | tajo           | C.H. TAJO                           |
#' 
#'
#' @export
#' 
#' @importFrom utils read.csv2
#'
#' @examples
#' \donttest{
#' # Read afliq.csv data from basin 'cantabrico'.
#' x <- hydrospain(file_name = "afliq", basin_nam = "cantabrico", verbose = FALSE)
#' }
#' 
hydrospain <- function(file_name = "estaf", basin_nam = NULL, timeout = 120, first_day = 1, sf = TRUE, verbose = TRUE) {


  # Check 'table_name'.
  stopifnot("Input 'file_name' must be a single string" = is.character(file_name) & length(file_name) == 1)
    file_name <- file_name |>
    tolower() |>
    select_coordinates()

  
  # Check basin names. Remove diacritics.
  if (!is.null(basin_nam)) {
    stopifnot("Input 'basin_nam' must be a character vector" = is.character(basin_nam) & is.vector(basin_nam))
    stopifnot("Wrong 'mino' input" = any(!("mino" %in% basin_nam)))
    basin_nam <- basin_nam |>
      tolower() |>
      replace_accent()
  }
    
    
  # Setting timeout.
  old <- options()
  on.exit(options(old))
  options(timeout = timeout)

  
  # cli progress bar update option.
  if (verbose) options(cli.progress_show_after = 0)
  

  # Will 'table_name' actually need coordinates?
  if (is.na(file_name$file_coords)) sf <- FALSE

  
  # Get the full URL for files and check them out.
  url <- "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/"
  url_all <- check_url_files(url = url, file_name = file_name, basin_nam = basin_nam,
                             timeout = timeout, sf = sf, verbose = verbose)

  
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
      cli::cli_progress_bar(paste0("Downloading data for ", basin_nam[i], " basin"), total = nbars, clear = FALSE)
      cli::cli_progress_update()
    }


    # Read file with data.
    dat <- utils::read.csv2(url_all$files[i]) |> utils::type.convert(as.is = TRUE)


    # If column "anomes" is present, transform to a Date object.
    if ("anomes" %in% tolower(colnames(dat))) {
      dat$fecha <- anomes_to_date(dat$anomes, first_day = first_day)
    }
    if ("fecha" %in% tolower(colnames(dat))) {
      dat$fecha <- as.Date(dat$fecha, "%d/%m/%Y")
    }
    

    # Read file with coordinates to sites.
    if (sf) {
      print(1)
      print(url_all$files[i])
      print(url_all$coords[i])
      if (url_all$files[i] != url_all$coords[i]) {
        if (verbose) cli::cli_progress_update()
        dat_coord <- utils::read.csv2(url_all$coords[i], encoding = "latin1") |> utils::type.convert(as.is = TRUE)
      } else {
        dat_coord <- dat
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
    dat$Cuenca <- basin_nam[i]


    # Add rows.
    if (verbose) cli::cli_progress_update()
    z <- rbind(z, dat)

  }
  
  if (verbose) cli::cli_end()

  return(z)
}
