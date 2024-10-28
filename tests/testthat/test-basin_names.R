test_that("Check names for basins", {

  setwd("..\\..")
  
  # Simple.
  expect_equal(basin_names("galicia"), data.frame(name = "galicia", nameceh = "GALICIA%20COSTA"))

  # More complicated.
  expect_identical(basin_names(c("galicia", "miño", "duero")), 
               data.frame(name = c("galicia", "miño", "duero"),
                                   nameceh = c("GALICIA%20COSTA", "MIÑO-SIL", "DUERO")))
  
})

