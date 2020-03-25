context("Running scenarios")

test_that("Running historical scenario works", {

  hc <- run_scenario("historical", return_core = TRUE)
  start <- hector::startdate(hc)
  end <- hector::enddate(hc)
  out <- fetchvars2(hc, "Ca")
  expect_true(all(out$value > 200))

  test_that("Reading outputs in RCMIP format works", {
    hc <- run_scenario("historical")
    rcout <- rcmip_outputs(hc)
    expect_true("Atmospheric Concentrations|CO2" %in% rcout$variable)
    expect_true("Carbon Sequestration" %in% rcout$variable)
    expect_true("Heat Uptake|Ocean" %in% rcout$variable)
  })
})
