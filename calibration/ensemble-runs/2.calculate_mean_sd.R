# Process the ensemble of Hector data to calculate the mean and sd 
# to use as the standard in the Hector calibration process. 
# 0. Set Up -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# Define the output. 
out_dir <-  here::here("calibration",  "ensemble-runs")

# Find the ensemble files. 
here::here("calibration",  "ensemble-runs", "raw-output") %>% 
  list.files(full.names = TRUE) -> 
  files

# 1. Select Runs To Process -------------------------------------------------
# In order to be consistent across the scenarios we have to use the runs that
# use the same parameters. 
lapply(files, function(file){
  # Import results and find identify the sucessful runs.
  data   <- read.csv(file, stringsAsFactors = FALSE)
  sucess <- na.omit(data)
  data.frame(scenario = unique(sucess$scenario), 
             runid =  unique(sucess$runid))
}) %>% 
  dplyr::bind_rows() -> 
  sucessful_run_ids

# Now that we have a data frame of sucessful runs per scenario 
# determine which runs are sucessfull across scenarios.
sucessful_run_ids %>% 
  dplyr::mutate(exists = TRUE) %>% 
  dplyr::distinct() %>%  
  tidyr::spread(scenario, exists) %>% 
  na.omit %>%  
  dplyr::pull(runid)->
  sucessfull_accross_scns

# Select a subset of the runs that are sucessful so that 
# all of the same runs are used in to create the mean 
# and sd across the scenarios.
selected_runs <- sucessfull_accross_scns[1:1000]

# 2. Process Runs ----------------------------------------------------------------
# Calculate the ensemble average and sd for the sucessful runs.
lapply(files, function(file){
  
  # Read in data and subset the data. 
  data        <- read.csv(file, stringsAsFactors = FALSE)
  subset_data <- data[data$runid %in% c(selected_runs), ]
  
  write.csv(subset_data, file = paste0(file, '-good-runs.csv'), row.names = FALSE)
  
  # Calculate the mean and sd for each year / variable.
  subset_data %>% 
    dplyr::group_by(scenario, year, variable) %>% 
    dplyr::summarise(mean = mean(value), 
                     sd = sd(value)) %>% 
    dplyr::ungroup()

}) %>% 
  dplyr::bind_rows() ->
  mean_sd
  
# Add the the historical scenario name to the standardized data. Otherwise the historical data 
# will not be used as inputs during the calibration routine. 
future_scns <- c("ssp119",  "ssp126",  "ssp245", "ssp370", "ssp434",        
                 "ssp460", "ssp585")
mean_sd %>% 
  dplyr::mutate(scenario = if_else(scenario %in% future_scns & year <= 2015, 'historical', scenario)) -> 
  mean_sd

# Save the results.
write.csv(x = mean_sd, file = file.path(out_dir, 'ensemble_mean_sd.csv'), row.names = FALSE)
  

  
