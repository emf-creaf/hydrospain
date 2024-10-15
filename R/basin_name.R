#' Title
#'
#' @param basin 
#'
#' @return
#' @export
#'
#' @examples
basin_name <- function(basin = NULL) {
  
  
  # Only need info.
  all_basins <- c("galicia", "cantabrico", "duero", "ebro", "guadalquivir",
                  "guadiana", "jucar", "miño", "segura", "tajo")
  if (is.null(basin)) return(all_basins)
  
  
  # Checks.
  stopifnot("Input 'basin' must be a character vector" = is.character(basin) & is.vector(basin))
  stopifnot("Wrong input! Did you mean 'miño'?" = any(!("mino" %in% basin)))
  basin <- tolower(basin)
  stopifnot("Wrong basin names" = any(basin %in% all_basins))
  
  
  # Find basin names for the CEDEX web.
  basin <- sapply(basin, function(x) switch(x,
                                            galicia = "GALICIA%20COSTA",
                                            cantabrico = "CANTABRICO",
                                            duero = "DUERO",
                                            ebro = "EBRO",
                                            guadalquivir = "GUADALQUIVIR",
                                            guadiana = "GUADIANA",
                                            jucar = "JUCAR",
                                            miño = "MIÑO-SIL",
                                            segura = "SEGURA",
                                            tajo = "TAJO"),
                  USE.NAMES = F)
    
  
  return(basin)
  
}