# create the model meta data infromation for the  RCMIP II submission 
# https://gitlab.com/rcmip/pyrcmip/-/raw/master/tests/data/rcmip_model_metadata_test.csv?inline=false

devtools::load_all()
renv::restore()

# Define directories. 
BASE_DIR <- here::here()
OUT_DIR  <- file.path(BASE_DIR, 'output', 'rcmipII')
dir.create(OUT_DIR, showWarnings = FALSE)


# Download the example data submission from RCMIPII this will be used to set up 
# the data that will be submitted.
pyrcmip_template <- read.csv(url('https://gitlab.com/rcmip/pyrcmip/-/raw/master/tests/data/rcmip_model_metadata_test.csv?inline=false'), stringsAsFactors = FALSE)
pyrcmip_template$climate_model <- 'hector'
pyrcmip_template$climate_model_name <- 'hector'
pyrcmip_template$climate_model_version <- 'v2.5.beta'
pyrcmip_template$climate_model_configuration_label <- 'rcmipII'
pyrcmip_template$climate_model_configuration_description <- 'Hector 2.5 version participating in RCMIP II'
pyrcmip_template$project <- 'RCMIP II'
pyrcmip_template$name_of_person <- 'Kalyn Dorheim'
pyrcmip_template$literature_reference <- c('https://gmd.copernicus.org/articles/8/939/2015/gmd-8-939-2015.pdf and  https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2018EF001082')

# Save the csv file. 
write.csv(x = pyrcmip_template, file = file.path(OUT_DIR, 'rcmip_model_metadata_hector.csv'), row.names = FALSE)


