context("Running scenarios")

test_that("Running basic scenario works", {
  out <- run_scenario("historical")
  out_ca <- subset(out, variable == "Ca")
  expect_true(all(out_ca$value > 200))
  expect_equivalent(range(out$year), c(1765, 2014))
})
