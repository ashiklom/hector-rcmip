# Subset and format the Hector ensemble results into a single csv file that 
# is small enough to store on a local machine, these csv files will be used to 
# create the netcdf files to submitto the RCMIP II. 
#
# After this script is run we recomend deleting the scenario sub directory in the 
# raw-ensemble directory. It takes quite a while to run this script, it recomended 
# to only run a single scenario at a time. 
#
# Lastly, if the netcdfs are missing some key information, it will need to be added to the 
# VARS_TO_SAVE vector. 

# 0. Set Up ------------------------------------------------------------------------------------------------
DATA_DIR <- here::here('output', 'rcmipII', 'raw-ensemble')
OUT_DIR  <- here::here('output', 'rcmipII', 'post_processed'); dir.create(OUT_DIR, showWarnings = FALSE)

# Load the required packages. 
renv::restore()

# A vector of the Hector output variable names to save from the ensemble of Hector runs. 
VARS_TO_SAVE <- c("Tgav", "Ftot", "FCO2", "heatflux", "Tgav_ocean_ST", "Tgav_ocean_air")
  
# 1. Format Data ------------------------------------------------------------------------------------------
# Select find all  10,000 files for asingle scenario that needs to be processed. 
# List of scnearios 
# "1pctCO2" #, "abrupt-4xCO2", "abrupt-2xCO2", "ssp119", "ssp126", "ssp585", "ssp370", "ssp434", "ssp460"
scn        <- '1pctCO2'
data_files <- list.files(file.path(DATA_DIR, scn), '.csv', full.names = TRUE)
assertthat::assert_that(length(data_files) == 10000)


# Import in each  file and subset the data frame so that they contain only the 
# variables of interest. Combine into a single file. 
lapply(data_files, function(f, vars = VARS_TO_SAVE){
  
  # Parse out the ensemble run number from the file name. 
  ens <- unlist(strsplit(gsub(basename(f), pattern = '.csv', replacement = ''), split = '_'))[[2]]
  
  dat <- read.csv(f, stringsAsFactors = FALSE) %>% 
    # Select the variables of intrest for the years of intrest. 
    dplyr::filter(variable %in% vars & year %in% 1750:2100) %>%  
    dplyr::select('year', 'variable', 'value') %>% 
    # Save only a selected number of significant digits to minimize the size of csv files. 
    dplyr::mutate(value = signif(value, digits = 6), 
                  ensemble = ens) %>% 
    # Format into a wide data frame (efficent in terms of space)
    # TODO if it is uncessary remove this step. 
    tidyr::pivot_wider(names_from = year, values_from = value)
  
  return(dat)
  
}) %>%  
  dplyr::bind_rows() ->
  hector_results 

# Save the results 
write.csv(hector_results, file = file.path(OUT_DIR, paste0(scn, '_subset_hector.csv')), row.names = FALSE)






