test_that("Check names for basins", {
  
  # Simple.
  expect_equal(basin_name("galicia"), "GALICIA%20COSTA")
  expect_equal(basin_name("cantabrico"), "CANTABRICO")
  expect_equal(basin_name("duero"), "DUERO")
  expect_equal(basin_name("ebro"), "EBRO")
  expect_equal(basin_name("guadalquivir"), "GUADALQUIVIR")
  expect_equal(basin_name("guadiana"), "GUADIANA")
  expect_equal(basin_name("jucar"), "JUCAR")
  expect_equal(basin_name("miño"), "MIÑO-SIL")
  expect_equal(basin_name("segura"), "SEGURA")
  expect_equal(basin_name("tajo"), "TAJO")

  # More complicated.
  expect_equal(basin_name(c("galicia", "miño", "duero")), c("GALICIA%20COSTA", "MIÑO-SIL", "DUERO"))
  expect_error(basin_name(c("galicia", "mino", "duero")))
  
  
  
  
  
})
