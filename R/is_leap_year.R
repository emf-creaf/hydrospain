#' Check whether year is leap
#' 
#' @description
#' \code{is_leap_year} checks whether numbers in input vector correspond to leap years.
#' 
#' @param year \code{integer} vector containing (positive) years.
#'
#' @return
#' Logical \code{TRUE} or \code{FALSE} vector depending on whether year is leap or not.
#'
#' @details
#' Straightforward algorithm. Input \code{year} must contain numbers without decimals,
#' although its \code{typeof} value can be \code{"integer"} or \code{"double"}.
#'
#' @examples
#' # Check leap years for XX century.
#' is_leap_year(seq(1900, 1999))
#' 
is_leap_year <- function(year) {
  
  
  # Checks.
  stopifnot("Input 'year' must be a vector" = is.vector(year))
  stopifnot("Input vector 'year' must contain integer numbers" = trunc(year) == year)
  
  return((year %% 4 == 0) & (year %% 100 != 0 | year %% 400 == 0))
}
