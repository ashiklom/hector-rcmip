# Format the comparison data 

library(dplyr)

out_dir <- here::here('calibration', 'comparison-data')

here::here('calibration', 'comparison-data', '1B-out') %>% 
  list.files(full.names = TRUE) %>% 
  lapply(read.csv, stringsAsFactors = FALSE) %>% 
  dplyr::bind_rows() -> 
  comparison_data
  

# For the idealized runs what is the temp dif relative to? 
scns <- c("ssp126", "ssp245",  "ssp370", "ssp585", "ssp119", "ssp434",        
          "ssp460", "ssp534-over", "1pctCO2", "historical", 
          "esm-piControl", "abrupt-4xCO2", "piControl", "abrupt-0p5xCO2", "1pctCO2")

idealized_scns <- c("1pctCO2", "abrupt-4xCO2", "abrupt-0p5xCO2")

comparison_data %>%  
  dplyr::filter(variable == 'tas' & experiment == 'historical') %>% 
  dplyr::filter(year %in% 1850:1860) %>% 
  dplyr::group_by(variable, model) %>%  
  dplyr::summarise(ref = mean(value)) %>%  
  dplyr::ungroup() -> 
  ref_temp 

comparison_data %>%  
  filter(variable == 'tas') %>%  
  dplyr::left_join(ref_temp, by = c("variable", "model")) %>%  
  dplyr::mutate(value = value - ref) %>%  
  dplyr::mutate(variable = if_else(variable == 'tas', hector::GLOBAL_TEMP(), variable)) -> 
  corrected_temp

comparison_data %>% 
  dplyr::filter(variable != 'tas') %>% 
  dplyr::bind_rows(corrected_temp) %>% 
  dplyr::select(year, value, units, variable, model, scenario = experiment, ensemble) -> 
  comparison_data

# In order for the idealized scenarios to be compared with the Hector results the 
# idealized runs must start at the year 1850. 
comparison_data %>%  
  filter(scenario %in% idealized_scns) %>%  
  dplyr::mutate(year = year + 1850) -> 
  absolute_time_idealized

# Replace the idealized results reported on the relative time series to the 
# absolute time series. 
comparison_data %>%  
  filter(!scenario %in% idealized_scns) %>%  
  bind_rows(absolute_time_idealized) %>% 
  write.csv(file = file.path(out_dir, 'comparison-data.csv'), row.names = FALSE)
