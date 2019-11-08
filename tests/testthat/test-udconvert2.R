context("Unit conversion")

test_that("Unit conversion works as expected", {
  x <- 10
  ytrue <- x * biogas::molMass("C") / biogas::molMass("CO2")
  y <- ud_convert2(x, "kg [CO2]", "kg [C]")
  expect_identical(y, ytrue)
})
