## Create the rcmip_model_reported_metrics_test.csv for the RCMIP II submission. 
## See "Model Reported Metrics" on https://pyrcmip.readthedocs.io/en/latest/submitting_results.html. 

devtools::load_all()
renv::restore()
library(hector)

# Define directories. 
BASE_DIR <- here::here()
OUT_DIR  <- file.path(BASE_DIR, 'output', 'rcmipII')
dir.create(OUT_DIR, showWarnings = FALSE)

# Download the posterior results provided from UI collaborators for the zenodo link. 
posterior <- read.csv(url('https://zenodo.org/record/3990501/files/forcing_10k_noSLR_posteriorSamples.csv?download=1'), stringsAsFactors = FALSE)
names(posterior) <- c('S', 'diff', 'alpha')

# Download the example data submission from RCMIPII this will be used to set up 
# the data that will be submitted.
pyrcmip_template <- read.csv(url('https://gitlab.com/rcmip/pyrcmip/-/raw/master/tests/data/rcmip_model_reported_metrics_test.csv?inline=false'), stringsAsFactors = FALSE)

# Create a data frame with the dimensions of the ensemble. 
hector_pyrcmip  <- matrix(nrow = nrow(posterior), ncol = ncol(pyrcmip_template))
colnames(hector_pyrcmip) <- colnames(hector_pyrcmip)
hector_pyrcmip <- as.data.frame(hector_pyrcmip) 

# Fill out the pyrcmip metrics data table. 
hector_pyrcmip$unit <- getunits(ECS())
hector_pyrcmip$value <- posterior$S
hector_pyrcmip$ensemble_member <- 0:(nrow(hector_pyrcmip) - 1)
hector_pyrcmip$climate_model <- 'hector'
hector_pyrcmip$RCMIP.name <- unique(pyrcmip_template$RCMIP.name)

# Save the csv file. 
write.csv(x = hector_pyrcmip, file = file.path(OUT_DIR, 'rcmip_model_reported_metrics_test.csv'))


