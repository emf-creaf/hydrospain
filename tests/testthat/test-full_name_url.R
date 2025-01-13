test_that("check url files", {
  
  testthat::skip_on_cran()
  
  # Vi leser data.
  url <- "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/"
  file_name <- data.frame(file = "afliq", file_coords = "estaf", id_join = "indroea")
  expect_no_error(full_name_url(url, file_name, c("ebro", "duero", "cantabrico"), sf = TRUE))
  
  # De er akkurat samen.
  df <- data.frame(files = c("https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/EBRO/afliq.csv",
                             "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/DUERO/afliq.csv",
                             "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/CANTABRICO/afliq.csv"),
                   coords = c("https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/EBRO/estaf.csv",
                              "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/DUERO/estaf.csv",
                              "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/CANTABRICO/estaf.csv"))
  expect_identical(full_name_url(url, file_name, c("ebro", "duero", "cantabrico"), sf = TRUE), df)
  
  # Vi forventer en fail.
  expect_error(full_name_url(url, file_name, sf = TRUE))
  
})
