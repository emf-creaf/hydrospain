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
  
  # But these outputs should. We leave "galicia" and "cantabrico" out due to some data gaps in the CEDEX files.
  f2 <- c("afliq", "afliqc", "afliqci", "afliqe", "afliqi", "anual_a", "anual_c", "anual_e", "canal",
          "embalse", "estadis_a", "estadis_ca", "estadis_en", "estadis_re", "estadis_sa", "estaf",
          "estev", "evap", "extremos_a", "extremos_a_v", "mensual_a", "mensual_a_v", "mensual_c", "mensual_e")
  for (i in f2) {
    for (j in cu[-c(1:2)]) {
      expect_true("sf" %in% class(hydrospain(i, j, timeout = 1000, verbose = FALSE)))
    }
  }
  
})

