#' Correction of some lat-lon coordinates
#'
#' @param df data.frame with a column named "indroea" and two other columns
#' for latitud and longitude.
#'
#' @description
#' A correction is introduced for some lat-long coordinates in some basins.
#'
#' @return
#' The input data.frame with corrected coordinates. If column "indroea" is not present
#' \code{correction_coordinates} returns the input \code{data.frame}.
#'
#' @details
#' A correction to coordinates, email from Carmen Mirta Dimas Suárez (CEH)
#' on July 17th, 2024. The correction applies to stations 3179, 3191, 3275 and 5107 only.
#'
#' @noRd
#' 
#' @examples
#' df <- data.frame(indroea = c("3179", "3000"), lat = 1:2, long = 3:4)
#' correction_coordinates(df)
correction_coordinates <- function(df) {


  # Checks.
  stopifnot("Input 'df' must be of data.frame type" = inherits(df, "data.frame"))
  
  
  if (any("indroea" %in% colnames(df))) {
    
    # Stations to be modified.
    stations <- c(3179, 3191, 3275, 5107)

    
    # Are there stations to be modified?
    i <- match(df$indroea, stations)
    
    if (sum(!is.na(i)) > 0) {
      
      # Latitude must be corrected in all four stations. Longitude, on the other hand,
      # only in 5107.
      lat <- c(401804, 404504, 403003, 380107)
      lon <- -40826
      
      for (j in 1:3) {
        k <- which(i == j)
        if (sum(!is.na(k)) > 0) df$lat[k] <- lat[j]
      }
      
      k <- which(i == 4)
      if (sum(!is.na(k)) > 0) {
        df$latwgs84[k] <- lat[3]
        df$longwgs84[k] <- lon[1]
      }
      
    }
  }


  return(df)

}
