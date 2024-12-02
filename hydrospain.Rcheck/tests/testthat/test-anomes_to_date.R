test_that("Extract date from 'anomes'", {
  
  # Error month number.
  expect_error(anomes_to_date("199013"))
  
  # Error number of days in February of a leap year.
  expect_identical(anomes_to_date("199202", first_day = 29), as.Date("29/02/1992", "%d/%m/%Y"))
  expect_error(anomes_to_date(c("199202", "035002"), first_day = c(28, 29)))
  
  # Error 31 days in April.
  expect_error(anomes_to_date(c("200004", "200005"), first_day = 31))
  
  # Error negative days.
  expect_error(anomes_to_date(c("210004", "200005"), first_day = -1))
  
  # Error days in month other than February
  expect_error(anomes_to_date(c("210001", "200004"), first_day = 31))
  
  # Error first_day is character.
  expect_error(anomes_to_date(c("210001", "200004"), first_day = "31"))
  
  # Expected dates. Days can be anything.
  expect_identical(anomes_to_date(c("210001", "200009"), first_day = c(17, 4)),
                                  as.Date(c("17/01/2100", "4/9/2000"), "%d/%m/%Y"))
  
})
