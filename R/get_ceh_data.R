#' Read Spanish basin data from the *cedex* site.
#'
#' @description
#' \code{read_ceh_basin} retrieves time series datasets and statistics for
#' several Spanish basins from the Centro de Estudios y Experimentación
#' de Obras Públicas (CEDEX).
#'
#'
#' @param table \code{character} with the name of the file to retrieve from the
#' *cedex* site, without extension. If not given, the default value is "estaf".

#' @param verbose \code{logical}, if set to TRUE progress messages are printed on screen.
#' @param basin 
#' @param sf logical, if TRUE \code{read_ceh_basin} returns a \code{sf} spatial object.
#' @param crs \code{character} indicating the coordinate system of choice. Values can be
#' "UTM", "UTM30" (default), "WGS84", "ED50" or "ETRS89" in lower or upper case letters (e.g. "wgs84" is valid,
#' as is "Wgs84").
#'
#' @return
#' A spatial \code{sf} object with a EPSG coordinate reference system (unless cs = "utm").
#'
#' @details
#' To see a description of the files to retrieve see
#' "https://ceh.cedex.es/anuarioaforos/demarcaciones.asp" and go to the basin you want
#' to get data for.
#' Regarding coordinates, crs = "UTM" indicates local UTM coordinates, which may be a problem
#' if the sites are located on both sides of the UTM demarcation line, so we do not recommend it.
#' crs = "UTM30" actually means Mercator UTM 30 North with datum WGS84, whereas ETRS89 implies
#' Mercator UTM 30 with datum ETRS89. Finally, crs = "ED50" and "WGS84" are longitude-latitude
#' system with those datums.
#'
#' The \code{encoding} argument of the \code{read.csv2} function is set to "latin1" in order to
#' correctly read diacritics and tildes.
#'
#' @export
#'
#' @examples
#' # Read afliq.csv data
#' x <- read_ceh_basin(table = "afliq)
get_ceh_data <- function(table = "estaf", basin = "all", sf = TRUE, crs = "utm30", verbose = TRUE) {


  # Check 'table'.
  stopifnot("Input 'table' must be a single string" = is.character(table) & length(table) == 1)
  table <- tolower(table)
  z <- c("afliq", "mensual_a", "estaf")
  stopifnot("Wrong 'table' value" = any(table %in% z))
  
  
  # Check basin names.
  basin_names <- basin_name()
  if ("all" %in% basin) {
    basin <- basin_names
  } else {
    stopifnot("Wrong basin name" = any(basin %in% basin_names))
  }

  # Check coordinate system.
  crs <- tolower(crs)
  stopifnot("Wrong 'crs'" = any(crs %in% c("utm", "utm30", "wgs84", "ed50", "etrs89")))

  
  # cli progress bar update option.
  if (verbose) options(cli.progress_show_after = 0)
  
  
  # Get the full URL for files and check them out.
  url <- "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/"
  url_files <- check_url_files(url, basin, table, verbose)

  if (table %in% c("afliq", "afliqi", "anual_a", "estadis_a", "extremos_a", "extremos_a_v", "mensual_a", "mensual_a_v")) {
    if (table != "estaf") {
      url_estaf <- check_url_files(url, basin, "estaf", verbose)
    } else {
      url_estaf <- url_files
    }
  }
    
  # If sf is TRUE, we need geographic coordinates.
  if (sf) {
    if (table %in% c("afliq", "afliqi", "anual_a", "estadis_a", "estaf", "extremos_a", "extremos_a_v", "mensual_a", "mensual_a_v")) {
    
    } else if (table %in% c("***")) {
    
  }



  # Reads data for all basins
  z <- NULL
  for (i in 1:length(basin)) {
    if (verbose) cli::cli_progress_bar(paste0("Downloading basin data ", basin[i]), total = 9, clear = FALSE)

    # Change column names and format.
    if (verbose) cli::cli_progress_update()

    # We need "estaf" always to obtain the coordinates to sites.
    dat_coord <- read.csv2(url_estaf[i], encoding = "latin1")
    dat_coord <- set_colmode(dat_coord, c(rep("character", 3), rep("numeric", 19), rep("character", 5), rep("numeric", 2), "character"))
    
    # Other tables.
    if (table != "estaf") {
      
      # Read data.
      dat <- read.csv2(url_files[i])
      
      # Set column formats.
      if (table ==  "afliq") {
        dat <- set_colmode(dat, c("character", "character", "numeric", "numeric"))
        dat$fecha <- as.Date(dat$fecha, format = "%d/%m/%Y")
      } else if (table == "mensual_a") {
        dat <- set_colmode(dat, c(rep("character", 2), rep("numeric", 10)))
        dat$fecha <- with(dat, as.Date(paste0(substr(anomes, 1, 4), "-", substr(anomes, 5, 6), "-01")))
      } else {
      dat <- dat_coord
      }
    }


    # Coordinates correction (email from Carmen Mirta Dimas Suárez, July 17th, 2024).
    if (verbose) cli::cli_progress_update()
    dat_coord <- correction_coordinates(dat_coord)


    # Coordinates.
    if (verbose) cli::cli_progress_update()
    dat_coord <- select_coord(dat_coord, crs)
    i <- match(dat$indroea, dat_coord$indroea)
    dat$x <- dat_coord$x[i]
    dat$y <- dat_coord$y[i]


  # sf object.
    if (verbose) cli::cli_progress_update()
    dat <- dat |> sf::st_as_sf(coords = c("x", "y"))


    # Add field "Cuenca" with name of basin.
    if (verbose) cli::cli_progress_update()
    dat$Cuenca <- basin[i]


    # Add rows.
    if (verbose) cli::cli_progress_update()
    z <- rbind(z, dat)
    
    if (verbose) cli::cli_progress_update()

  }

  if (verbose) cli::cli_process_done()
  
  # Coordinate reference system.
  if (cs != "utm") sf::st_crs(z) <- epsg_ceh(cs)


  return(z)
}
