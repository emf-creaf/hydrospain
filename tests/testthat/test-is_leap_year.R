test_that("Test leap years", {
  
  yr <- seq(-3000, 3000)
  
  # Compare results with similar function in "lubridate".
  expect_identical(lubridate::leap_year(yr), is_leap_year(yr))
  
})
