# Format the Hector ensmble results into a netcdf file formatted into RCMIP II 
# specifications. 
# 
# This script is set up only to process a single scenario at a time, although compared with 
# the L1 and L2 scripts the run time is very short, however it depends on the L2 script has 
# been used to process all of the scenarios. 
# 
# 0. Set Up -------------------------------------------------------------------------------------
# Load the required libraries
library(ncdf4)
library(magrittr)

# Define the input and output directories. 
DATA_DIR <- here::here('output', 'rcmipII', 'post_processed')
OUT_DIR  <-  here::here('output', 'rcmipII', 'netcdfs'); dir.create(OUT_DIR, showWarnings = FALSE)

# 1. Import Scenario Data -----------------------------------------------------------------------
# Select the specific scenario to process. 
files <- list.files(DATA_DIR, pattern = '_subset_hector.csv', full.names = TRUE)

# 2. Create the Netcdf File ----------------------------------------------------------------------
lapply(files, function(file){
  
  # Read in the ensemble of the Hector results.  
  data <- read.csv(file.path(file),stringsAsFactors = FALSE)
  scn_name <- gsub(pattern = '_subset_hector.csv', replacement = '', x = basename(file))
  message(scn_name)
  
  # Define some information about the dimensions of the expected data. 
  # For now this is hard coded but could 
  ensmble_members <- unique(data$ensemble)
  time_years <- as.integer(gsub(x = names(data)[grepl(pattern = 'X', x = names(data))], pattern = 'X', replacement = ''))

  # Set up the dimensions of the netcdf file
  dim_time <- ncdim_def('time', 'year', as.double(time_years))
  dim_ensemble_member <- ncdim_def('ensemble_member',  units = 'ensemble_member', vals = as.integer(ensmble_members))

  # Define variables
  tgav <- ncvar_def(name = "Surface Air Temperature Change", units =  "K",  dim = list(dim_time, dim_ensemble_member))
  ftot <- ncvar_def(name = "Effective Radiative Forcing", units =  "W/m^2", dim = list(dim_time, dim_ensemble_member))
  fco2 <- ncvar_def(name = "Effective Radiative Forcing|Anthropogenic|CO2", units =  "W/m^2", dim = list(dim_time, dim_ensemble_member))
  
  # create netCDF file and put arrays, but first make sure that the 
  # netcdf does not exist, if the ncdf4 commands try to manipulate 
  # or overwrite a nc the resulting nc will be corrupt.  
  nc_name <- file.path(OUT_DIR, paste0('rcmipII-', scn_name, '.nc'))
  if(file.exists(nc_name)){ file.remove(nc_name) }
  ncnew <- nc_create(nc_name,list(tgav,ftot,fco2),force_v4=TRUE)
  
  
  # Save the arrays of data. 
  data %>% 
    dplyr::filter(variable == 'Tgav') %>% 
    dplyr::select(-variable, -ensemble) %>% 
    as.matrix() %>% 
    t() ->
    temp_data
  
  data %>% 
    dplyr::filter(variable == 'Ftot') %>% 
    dplyr::select(-variable, -ensemble) %>% 
    as.matrix() %>% 
    t() ->
    ftot_data
  
  data %>% 
    dplyr::filter(variable == 'FCO2') %>% 
    dplyr::select(-variable, -ensemble) %>% 
    as.matrix() %>% 
    t() ->
    fco2_data
  
  # Add the arrays of data to the nc. 
  ncvar_put(ncnew, "Surface Air Temperature Change", vals = temp_data)
  ncvar_put(ncnew, "Effective Radiative Forcing", vals = ftot_data)
  ncvar_put(ncnew, "Effective Radiative Forcing|Anthropogenic|CO2", fco2_data)
  
  # Add the required attributes to the netcdf files. 
  ncatt_put(ncnew, "Surface Air Temperature Change", attname = 'climate_model', attval = 'hector')
  ncatt_put(ncnew, "Effective Radiative Forcing", attname = 'climate_model', attval = 'hector')
  ncatt_put(ncnew, "Effective Radiative Forcing|Anthropogenic|CO2", attname = 'climate_model', attval = 'hector')
  
  ncatt_put(ncnew, "Surface Air Temperature Change", attname = 'region', attval = 'World')
  ncatt_put(ncnew, "Effective Radiative Forcing", attname = 'region', attval = 'World')
  ncatt_put(ncnew, "Effective Radiative Forcing|Anthropogenic|CO2", attname = 'region', attval = 'World')
  
  ncatt_put(ncnew, "Surface Air Temperature Change", attname = '_is_metadata', attval = 0)
  ncatt_put(ncnew, "Effective Radiative Forcing", attname = '_is_metadata', attval = 0)
  ncatt_put(ncnew, "Effective Radiative Forcing|Anthropogenic|CO2", attname = '_is_metadata', attval = 0)
  
  # Add global attributes 
  ncatt_put(ncnew,  0, "created_at", as.character(Sys.time()))
  ncatt_put(ncnew, 0, "_scmdata_version", substr(start = 1,  stop = 6, x = system('git describe', intern = TRUE)))
  ncatt_put(ncnew, 0, "scenario", scn_name)
  ncatt_put(ncnew, 0, "model", 'hector')
  
  # Save the netcdf file. 
  nc_close(ncnew)
  
  
})


# 3. Format the nc files using python ------------------------------------------------------------
# The python and R ncdf4 packages are different from one another. The pyrcmip package used to 
# submit the RCMIP results depends on the netcdfs being consistent with the python ncdf4 pacakge. 
# Using the 3A.func_format_rcmipII_nc.py script reformat the netcdf files. 
file <- here::here('scripts', 'rcmipII',  '0.func_format_rcmipII_nc.py')
assertthat::assert_that(file.exists(file))

system2('python3', args = file)

