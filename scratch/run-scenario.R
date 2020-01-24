scenario <- "piControl"
model <- cmip6_params()[["model"]][1]

out <- run_scenario(scenario, model)
out_rcmip <- rcmip_outputs(out)

renv::update()


hc <- hector::newcore(system.file("input", "hector_rcp45.ini", package = "hector"))
invisible(hector::run(hc))
