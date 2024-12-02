test_that("Test leap years", {
  
  yr <- seq(1582, 2020, by = 1)
  
  # To compare with.
  z <- c(F, F, T, rep(c(F, F, F, T), 109))
  
  # Years 1700, 1800 and 1900 are not divisible by 400.
  z[match(c(1700, 1800, 1900), yr)] <- FALSE
  
  # Check.
  expect_identical(is_leap_year(yr), z)
  
  # Integer years.
  expect_error(is_leap_year(1890.5))
  
})
