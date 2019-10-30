context("Set variable")

test_that("Set variable works", {
  hc <- rcmip_core()
  fake_input <- data.frame(
    Variable = "Emissions|CO2|Fossil and Industrial",
    year = seq(1900, length.out = 10),
    value = rep(1000, 10)
  )
  hc <- set_variable(hc, fake_input)
  invisible(hector::run(hc, max(fake_input$year)))
  result <- hector::fetchvars(hc, fake_input$year, hector::FFI_EMISSIONS())
  # Convert units from RCMIP to Hector for comparison
  vc <- readr::read_csv(system.file("variable-conversion.csv",
                                    package = "hector.rcmip"))
  vc_ffi <- subset(vc, hector_variable == "ffi_emissions")[1,]
  real_output <- udunits2::ud.convert(
    fake_input$value,
    vc_ffi$rcmip_udunits,
    vc_ffi$hector_udunits
  )
  expect_equivalent(result$value, real_output)
})
