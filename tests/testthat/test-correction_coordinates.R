test_that("Correct coordinates", {

  # Wrong input.
  expect_error(correction_coordinates(1))
  
  # No correction.
  df <- data.frame(indroea = c(3159, 3000), lat = 1:2, long = 3:4)
  expect_identical(correction_coordinates(df), df)
  
  # Correction is applied.
  df <- data.frame(indroea = c("3179", "3000"), lat = 1:2, long = 3:4)
  expect_true(is.data.frame(correction_coordinates(df)))
  
  dfcorr <- df
  dfcorr[1, 2] <- 401804
  expect_identical(dfcorr, correction_coordinates(df))
  
  
})
