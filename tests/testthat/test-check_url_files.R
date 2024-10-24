test_that("check url files", {
  
  # Vi leser data.
  url <- "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2020-2021/"
  setwd("..\\..")
  basin_names <- basin_names()
  file_name <- file_coordinates("afliq")
  
  # Det virker riktig.
  expect_no_condition(check_url_files(url, basin_names[1:2, ], file_name, sf = TRUE, verbose = FALSE))
  
  # Vi forventer en fail.
  basin_names$nameceh[1] <- "asdf"
  expect_error(check_url_files(url, basin_names[1:2, ], file_name, sf = TRUE, verbose = FALSE))
  
})
