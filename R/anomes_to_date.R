#' Title
#'
#' @param x 
#' @param first_day numeric \code{vector} containing the day number(s) that will be used when
#' building the \code{date} object.
#'
#' @return
#' @details
#' For February \code{first_day} must be 29 in non-leap years, and 28 on leap years.
#' 
#'
#' @examples
anomes_to_date <- function(dat, first_day = 1) {
  

  # Checks.
  stopifnot("Input 'dat' must be a numeric vector" = is.numeric(dat) & is.vector(dat))
  stopifnot("Strings in 'dat' must be of 6-character length" = all(nchar(dat) == 6))
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