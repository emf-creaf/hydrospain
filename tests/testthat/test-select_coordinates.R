test_that("multiplication works", {

  expect_no_condition(select_coordinates())
  df <- data.frame(file = "afliq", file_coords = "estaf", id_join = "indroea")
  expect_identical(select_coordinates("afliq"), df)
  
})
