context("Running scenarios")

test_that("Running basic scenario works", {
  hc <- run_scenario("historical")
  start <- hector::startdate(hc)
  expect_equal(start, 1765)
  end <- hector::enddate(hc)
  expect_equal(end, 2014)
  out <- hector::fetchvars(hc, seq(start, end), "Ca")
  expect_true(all(out$value > 200))
})
