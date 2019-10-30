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

  # All emissions should be zero
  emiss_vars <- sprintf("%s_emissions", c(
    "ffi", "luc", "BC", "OC",
    "SO2", "CH4",
    #"NOX", "CO", "NMVOC",
    "N2O", "CF4", "C2F6", "HFC4310", "HFC125"
  ))
  emissions <- hector::fetchvars(hc, dates, c(emiss_vars, "SV"))
  expect_equivalent(sum(emissions$value ^ 2), 0)
})
