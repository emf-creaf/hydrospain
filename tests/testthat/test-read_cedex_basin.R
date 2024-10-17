test_that("Read cedex basin data", {
  
  expect_error(read_cedex_basin(cs = "ws84"))
  expect_error(read_cedex_basin(basin = c("ebro", "aaa", "duero", "bbb")))
  expect_error(read_cedex_basin(table = "aaa"))
  
  
})
