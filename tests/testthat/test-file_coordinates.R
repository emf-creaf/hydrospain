test_that("Test files", {
  
  f <- c("afliq", "afliqc", "afliqci", "afliqe", "afliqi", "anual_a", "anual_c", "anual_e", 
         "canal", "cdr", "embalse", "estadis_a", "estadis_ca", "estadis_en", "estadis_re", "estadis_sa",  
         "estaf", "estev", "evap", "extremos_a", "extremos_a_v", "gr_cuenca", "hojas50", "mensual_a",
         "mensual_a_v", "mensual_c", "mensual_e", "muni", "prov")
  
  setwd("..\\..")
  expect_identical(f, file_coordinates()$file)
  
  
})
