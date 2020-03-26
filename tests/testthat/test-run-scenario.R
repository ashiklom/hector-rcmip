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


test_that("Cores set up properly",{
  
  # Compare one of the RCMIP RCP runs with RCP output from one of the 
  # default Hector RCP emissions.
  # RCMIP RCP RUN
  rcmip <- read.csv(run_scenario('rcp45'), stringsAsFactors = FALSE)
  rcmip$'rcmip' <- rcmip$value
  rcmip <- rcmip[ , names(rcmip) %in% c('year', 'variable', 'rcmip')]
  
  # Hector RCP Run
  core <- hector::newcore(system.file('input/hector_rcp45.ini', package = 'hector'))
  hector::run(core)
  true <- hector::fetchvars(core, 1850:2100, hector::GLOBAL_TEMP())
  true$'hector' <- true$value
  true <- true[ , names(true) %in% c('year', 'variable', 'hector')]

  # Format the comparion data. 
  comp_data      <- dplyr::left_join(rcmip, true)
  temp_comp_data <- na.omit(comp_data[comp_data$variable == hector::GLOBAL_TEMP(), ])
  temp_RMSE <- mean((temp_comp_data$rcmip - temp_comp_data$hector)^2)
  
  # Make sure that the results are within each other for some threshold. 
  # Make sure that the temp results are within 0.01 deg C of one anohter. 
  temp_threshold <- 0.01
  testthat::expect_true(temp_RMSE < temp_threshold)
  
})
