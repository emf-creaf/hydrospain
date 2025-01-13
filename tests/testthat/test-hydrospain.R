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


  # These outputs should not be "sf".
  f1 <- c("cdr", "gr_cuenca", "hojas50", "muni", "prov")
  cu <- c("galicia", "cantabrico", "duero", "ebro", "guadalquivir", "guadiana", "jucar", "miño", "segura", "tajo")
  for (i in f1) {
    for (j in cu) {
      expect_false("sf" %in% class(hydrospain(i, j, timeout = 1000, verbose = FALSE)))
    }
  }
  
})

