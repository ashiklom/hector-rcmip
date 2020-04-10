# In attempt to figure out which calibration protocol was the best several rounds 
# of exerpimentation with calibration protocols were required. This script 
# parses out the calibration information and saves it as a csv file to be used in 
# the RCMIP framework.


# 0. Set Up --------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(hector)
library(gridExtra)
devtools::load_all()
source(here::here('calibration', 'hectorcal-functions.R'))

# 0.5 Define Functions ---------------------------------------------------------------------------
# This function removes the years from Hector output that are not relevant to certain scenarios.
clean_up_scenarios <- function(data){
  data %>%  
    mutate(keep = FALSE) %>%  
    mutate(keep = if_else(scenario %in% c("piControl", "1pctCO2", "1pctCO2-4xext", 
                                          "abrupt-4xCO2", "abrupt-2xCO2", "abrupt-0p5xCO2") & year <= 2015, 
                          TRUE, keep)) %>%  
    mutate(keep = if_else(grepl(pattern = 'ssp', x = scenario) & year > 2015, TRUE, keep)) %>% 
    mutate(keep = if_else(scenario == 'historical' & year <= 2015, TRUE, keep)) %>% 
    filter(keep) %>%  
    select(-keep) %>%  
    filter(year >= 1850)
}

# 1. Import RCMIP Hector Results --------------------------------------------------------------------------------------
# Import and format the most recent calibration results
extract_results <- function(attempt_name){
  files <- list.files(here::here('calibration', attempt_name), pattern = '.rds', full.names = TRUE)
  files <- files[!grepl(pattern = '_par.rds', x = files)]
  files %>% 
    lapply(function(input){
      
      name <- gsub(x = basename(input), pattern = '.csv|.rds|_par', replacement = '')
      rslt <- readRDS(input)
      params <- rslt$par
      df <- as.data.frame(params, row.names  = NULL)
      df$names <- names(params)
      df$model <- name
      df$convergence <- rslt$convergence
      tidyr::spread(df, names, params)
      
    }) %>% 
    dplyr::bind_rows() ->
    calibration_fits
  
  scns  <- c(
    "piControl", "esm-piControl", "1pctCO2", "1pctCO2-4xext",
    "abrupt-4xCO2", "abrupt-2xCO2", "abrupt-0p5xCO2",
    "historical", 
    "ssp126", "ssp245", "ssp370", "ssp585", "ssp119", "ssp434", 
    "ssp460" ,
    paste0("rcp", c("26", "45", "60", "85")))
  
  cores <- lapply(scns, function(file){
    core <- run_scenario(file, return_core = TRUE)
    hector::reset(core)
    core
  })
  calibration_fits %>% 
    select(-convergence) %>% 
    apply(1, function(fits){
      
      model <- fits[['model']]
      params <- fits[names(fits) != 'model']
      
      lapply(cores, function(hc){
        tryCatch({
          p <- as.numeric(params)
          names(p) <- names(params)
          hectorcal.parameterize_core(p, hc)
          hector::reset(hc)
          hector::run(hc, runtodate = 2100)
          hector::fetchvars(hc, 1850:2100, vars = c(GLOBAL_TEMP(), HEAT_FLUX()))
        }, error = function(e){
          data.frame(value =  NA, 
                     scenario = NA)
        })
        
        
      }) %>%  
        dplyr::bind_rows(.) %>% 
        mutate(model = model) 
      
    }) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(calibration = attempt_name) %>% 
    dplyr::left_join(calibration_fits, by ='model')}

attempt_list <- c('attempt-6')  

hector_results <- bind_rows(lapply(attempt_list, extract_results))
hector_results <- dplyr::rename(hector_results, hector_value = value)

unique(cmip6_paramter_fits$model)
hector_results %>% 
  filter(calibration == 'attempt-6') %>%   
  select(S, diff, alpha, volscl, convergence, model) %>%
  # Set the volcanic scalar to 1, this is the defaul value that will be used, 
  # to avoid issues from chaning the number of parameters expected by the hector cores. 
  mutate(volscl = 1) %>% 
  distinct() %>%   
  filter(model %in% c("CanESM5", "CESM2-WACCM", "CESM2", "CNRM-CM6-1", "CNRM-ESM2-1", "IPSL-CM6A-LR")) -> 
  new_fits

# Save the parameter fits. 
file <- file.path(here::here('data-raw'), "cmip6_paramter_fits.csv")
write.csv(new_fits, file = file, row.names = FALSE)
