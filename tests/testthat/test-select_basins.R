test_that("Check names for basins", {
  
  # Simple.
  expect_equal(select_basins("galicia"), data.frame(name = "galicia", nameceh = "GALICIA%20COSTA"))

  # More complicated.
  expect_equal(select_basins(c("galicia", "miño", "duero")), 
               data.frame(name = c("galicia", "miño", "duero"),
                                   nameceh = c("GALICIA%20COSTA", "MIÑO-SIL", "DUERO")))
  expect_error(select_basins(c("galicia", "mino", "duero")))
  expect_true(nrow(select_basins()) == 10)

})
