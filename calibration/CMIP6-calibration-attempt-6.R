# attempt 7 at the calibration 
# the standard calibration from CMIP5 
# 4 parameters, temp (hist + future) & heat flux (future only)
# Plus the non picontrol - idealized runs!! 


# Now we are going to remove the volscl 

# But now we are using the raw Hector and CMIP6 comp data in the optmization 
# routine because of the isses we were having to the calibrataion. 

# Note to self the next thing to try might be how we do the range thingy 
# is there is no idealized information so that we are not insanely far off.. 

library(dplyr)
library(tidyr)
devtools::load_all()
# DO NOT STANDARDIZE THE DATA! 
source(here::here("calibration", "hectorcal-functions2.R"))

# On the first attempt it turned out that the temp had not been converted from absoulte to anomaly 
# which is no bueno!
# attempt 2 let's take a look at what happens when we use the correct comparison data :( 

OUT_DIR <- here::here('calibration', 'attempt-6'); 
dir.create(OUT_DIR, showWarnings = FALSE)

all_esm_data <- read.csv(here::here('calibration', 'comparison-data', 'comparison-data.csv'),  
                         stringsAsFactors = FALSE)

future_scns      <- c("ssp126", "ssp245", "ssp370", "ssp585", "ssp119", "ssp434", "ssp460", "ssp534-over" )
idealized_scns   <- c("1pctCO2", "abrupt-4xCO2", "abrupt-0p5xCO2")
# Do this first set of calibration runs without the idealized runs, we need to think about 
# how to process the idealized runs so that it is consistent with the RCMIP scenarios.
calibration_scns <- c(future_scns, "historical", idealized_scns)


all_esm_data_subset <- dplyr::filter(all_esm_data, scenario %in% calibration_scns)


# What models have historical temperature data? 
all_esm_data_subset %>% 
  dplyr::filter(variable == hector::GLOBAL_TEMP() & scenario == 'historical') %>%  
  dplyr::pull(model) %>% 
  unique() -> 
  historical_temp_models

# What models have some future temp? ** May also need to explore 
# requring the 
all_esm_data_subset %>% 
  dplyr::filter(variable == hector::GLOBAL_TEMP() & scenario %in% future_scns) %>%  
  dplyr::pull(model) %>% 
  unique() ->
  future_temp_models

# We require that models have some future ocean team data. 
all_esm_data_subset %>% 
  dplyr::filter(variable == hector::HEAT_FLUX() & scenario %in% future_scns) %>%
  dplyr::pull(model) %>%  
  unique() -> 
  future_heat_flux

# Would we want to try calibrating only with the idealized runs? 
all_esm_data_subset %>%  
  dplyr::filter(scenario %in% idealized_scns) %>%  
  dplyr::pull(model) %>%  
  unique() -> 
  idealized_scn_models

# Which models meet our requirements? 
models <- intersect(intersect(intersect(historical_temp_models, future_temp_models), 
                              future_heat_flux), idealized_scn_models)


# Subset the esm data so that  it contains only the models we want to calibrate.  
actual_calibratoin_data <- dplyr::filter(all_esm_data_subset, model %in% models)


initial_param <- c('S' = 3.86, 'diff' = 1.07, 'alpha' = 1)

data_list <- split(actual_calibratoin_data, actual_calibratoin_data$model, drop = TRUE) 
data_list %>% 
  lapply(function(comp_data){
    
    model_name <- unique(comp_data$model)
    message(model_name)
    rslts <- hectorcal.calibrate_diagnostic(comp_data = comp_data, 
                                            maxit = 1000, inital_param = initial_param)
    
    file <- file.path(OUT_DIR, paste0(model_name, '.rds'))
    saveRDS(rslts, file = file)
  })



