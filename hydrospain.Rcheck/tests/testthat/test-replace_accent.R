test_that("Remove diacritics", {
  
  # French.
  x <- c("Búho búsqueda de piñatas exóticas con él",
        "Tráigaselo rápido al marqués con muchísimo azúcar",
        "Qué día más tonto")
  y <- c("Buho busqueda de pinatas exoticas con el",
         "Traigaselo rapido al marques con muchisimo azucar",
         "Que dia mas tonto")
  expect_identical(replace_accent(x), y)
  
  
  # Spanish.
  x <- c("Où est allé ce très gentil employé à l'élégant béret?",
         "Ça ne m'étonne pas, j'ai vu ce frêle musicien flâner près des quais, l'air rêveur",
         "Êtes-vous sûr?")
  y <- c("Ou est alle ce tres gentil employe a l'elegant beret?",
         "Ca ne m'etonne pas, j'ai vu ce frele musicien flaner pres des quais, l'air reveur",
         "Etes-vous sur?")
  expect_identical(replace_accent(x), y)
  
  
  x <- c("L’aïllament de l’ésser humà pot conduir a l'abús del güisqui",
         "Aquél búho, con su aguda visión, voló hacia el pingüino")
  y <- c("L'aillament de l'esser huma pot conduir a l'abus del guisqui",
         "Aquel buho, con su aguda vision, volo hacia el pinguino")
  expect_identical(replace_accent(x), y)
  
  # Spanish.
  x <- c("á", "é", "í", "ó", "ú", "ü", "ñ")
  y <- c("a", "e", "i", "o", "u", "u", "n")
  expect_identical(replace_accent(x), y)
  
  # Catalan.
  x <- c("à", "è", "é", "ì", "ï", "ò", "ú", "ü", "ç")
  y <- c("a", "e", "e", "i", "i", "o", "u", "u", "c")
  expect_identical(replace_accent(x), y)
  
  # French.
  x <- c("à", "â", "è", "é", "ê", "ë", "î", "ï", "ô", "ö", "û", "ù", "ü", "ÿ", "ç")
  y <- c("a", "a", "e", "e", "e", "e", "i", "i", "o", "o", "u", "u", "u", "y", "c")
  expect_identical(replace_accent(x), y)
  
})
