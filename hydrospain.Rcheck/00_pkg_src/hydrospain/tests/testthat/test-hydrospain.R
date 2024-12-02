test_that("gauging station data", {

  testthat::skip_on_cran()
  
  # No need for coordinates.
  x <- hydrospain("hojas50", basin_nam = "guadalquivir", timeout = 1000, verbose = FALSE)
  expect_false(any("sf" %in% class(x)))
  
  x <- hydrospain(file_name = "afliq", basin_nam = "duero", sf = TRUE, timeout = 1000, verbose = FALSE)
  expect_true("sf" %in% class(x))
  
  y <- hydrospain(file_name = "afliq", basin_nam = "duero", sf = FALSE, timeout = 1000, verbose = FALSE)
  expect_true("data.frame" %in% class(y))
  
  dfx <- sf::st_drop_geometry(x)
  expect_identical(y, dfx)
  
  expect_error(hydrospain(file_name = "afliq", basin_nam = "xx", sf = TRUE, verbose = FALSE))
  expect_error(hydrospain(file_name = "xx", basin_nam = "duero", sf = TRUE, verbose = FALSE))
  expect_error(hydrospain(file_name = "xx", basin_nam = "xx", sf = TRUE, verbose = FALSE))


  
})

