#' Convert 'yearmonth' string to a Date object
#' 
#' @description
#' \code{anomes_to_date} converts a 'yearmonth' string like e.g. 199201 to a Date
#' object, where 'year' and 'month' have four and two digits, respectively.
#' 
#' @param dat numeric or character \code{vector} with the strings to extract the date from.
#' @param first_day numeric \code{vector} containing the day number(s) that will be used when
#' building the \code{date} object.
#'
#' @return
#' A \code{Date} object.
#' 
#' @details
#' For February \code{first_day} must be 29 in non-leap years, and 28 on leap years.
#' 
#' @examples
#' # Same result.
#' y <- anomes_to_date(199201)
#'
#' @export
#' 
anomes_to_date <- function(dat, first_day = 1) {
  

  # Checks.
  stopifnot("Input 'dat' must be a vector" = is.vector(dat))
  if (is.character(dat)) dat <- as.numeric(dat)
  stopifnot("Strings/Numbers in 'dat' must have 6 digits" = all(nchar(dat) == 6))
  stopifnot("Input 'first_day' must be numeric vector" = is.numeric(first_day) & is.vector(first_day))
  stopifnot("Input 'first_day' cannot be larger than 31 or smaller than 1" = all(first_day <= 31 & first_day > 0))
  ndat <- length(dat)
  if (length(first_day) > 1) {
    stopifnot("Length of 'first_day' must be equal to 1 or to length of 'dat'" = ndat == length(first_day))
  } else {
    first_day <- rep(first_day, ndat)
  }
  
  
  # Days per month in a non-leap year.
  days_nonleap <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  
  # Extract year and month.
  yr <- as.integer(substr(dat, 1, 4))
  mo <- as.integer(substr(dat, 5, 6))

  
  # Check that month is correct.
  stopifnot("Months must be 1 to 12" = all(mo %in% 1:12))
  
  
  # Check number of days in February for leap years.
  isleap <- is_leap_year(yr)
  i <- which(isleap & mo == 2)
  if (length(i)) {
    stopifnot("February must have a maximum of 29 days in leap years" = all(first_day[i] <= 29))
  }

  
  # Check number of days for other months y/o years.
  i <- which(!isleap | mo != 2)
  if (length(i) > 0) {
    stopifnot("Input 'first_day' is wrong for its month" = all(first_day[i] <= days_nonleap[mo[i]]))
  }


  # Final date string.
  date_out <- as.Date(paste(first_day, mo, yr, sep = "/"), "%d/%m/%Y")
  
  return(date_out)
  
}