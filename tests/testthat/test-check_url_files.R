test_that("check url files", {
  
  # Vi leser data.
  url <- "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/"
  file_name <- data.frame(file = "afliq", file_coords = "estaf", id_join = "indroea")
  
  # Det virker riktig.
  expect_no_condition(check_url_files(url, file_name, c("ebro", "duero", "cantabrico"), sf = TRUE, verbose = FALSE))
  
  
  # Vi forventer en fail.
  expect_error(check_url_files(url, file_name, sf = TRUE, verbose = FALSE))
  
})
