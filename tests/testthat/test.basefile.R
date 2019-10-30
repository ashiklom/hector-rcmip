context("Default INI file")

test_that("Hector will run with default INI file", {
  f <- system.file("rcmip-default.ini", package = "hector.rcmip")
  expect_true(file.exists(f))
  hc <- hector::newcore(f, suppresslogging = TRUE)
  invisible(hector::run(hc))
  dates <- seq(hector::startdate(hc), hector::enddate(hc))
  result <- hector::fetchvars(hc, dates, hector::ATMOSPHERIC_CO2())
  expect_true(nrow(result) == length(dates))
  expect_true(all(result[["value"]] > 0))
})
