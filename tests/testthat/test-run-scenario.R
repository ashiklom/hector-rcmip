context("Running scenarios")

test_that("Running basic scenario works", {
  hc <- run_scenario("historical")
  start <- hector::startdate(hc)
  expect_equal(start, 1765)
  end <- hector::enddate(hc)
  expect_equal(end, 2014)
  out <- fetchvars2(hc, "Ca")
  expect_equivalent(range(out$year), c(1765, 2014))
  expect_true(all(out$value > 200))
})
