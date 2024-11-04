test_that("gauging station data", {
  print(1)
  x <- get_ceh_data(file_name = "afliq", basin_nam = "duero", sf = TRUE, verbose = FALSE)
  expect_true("sf" %in% class(x))
  print(2)
  y <- get_ceh_data(file_name = "afliq", basin_nam = "duero", sf = FALSE, verbose = FALSE)
  expect_true("data.frame" %in% class(y))
  print(3)
  dfx <- sf::st_drop_geometry(x)
  expect_identical(y, dfx)
  
  expect_error(get_ceh_data(file_name = "afliq", basin_nam = "xx", sf = TRUE, verbose = FALSE))
  expect_error(get_ceh_data(file_name = "xx", basin_nam = "duero", sf = TRUE, verbose = FALSE))
  expect_error(get_ceh_data(file_name = "xx", basin_nam = "xx", sf = TRUE, verbose = FALSE))

  # No need for coordinates.
  expect_no_condition(get_ceh_data("hojas50", basin_nam = "guadalquivir", verbose = FALSE))

})

