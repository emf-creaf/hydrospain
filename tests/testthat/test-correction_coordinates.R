test_that("Correct coordinates", {

  
  expect_error(correction_coordinates(1))
  
  df <- data.frame(indroea = c("3179", "3000"), lat = 1:2, long = 3:4)
  expect_true(is.data.frame(correction_coordinates(df)))
  
  dfcorr <- df
  dfcorr[1, 2] <- 401804
  expect_identical(dfcorr, correction_coordinates(df))
  
  
})
