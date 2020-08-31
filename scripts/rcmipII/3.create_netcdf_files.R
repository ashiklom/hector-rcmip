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
files <- list.files(DATA_DIR, full.names = TRUE)

files <- files[grepl(pattern = 'ssp460', x = files)]
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
  dim_scneario <- ncdim_def("scenario", "", vals = 1, create_dimvar=FALSE )
  ncdim_def("nchar", "", 1:12, create_dimvar=FALSE )
 # dim_scenario <- ncdim_def(name = 'scenario', units = scn_name, vals = 'cd')
 # dim_scenario <- ncdim_def("nchar", "", 1, create_dimvar=FALSE )
  dim_ensemble_member <- ncdim_def('ensemble_member',  units = 'ensemble_member', vals = as.integer(ensmble_members))
  dimnchar <- ncdim_def("nchar", "", 1:2, create_dimvar=FALSE)
  
  # define variables
  fillvalue <- NA
  
  tgav <- ncvar_def(name = "Surface Air Temperature Change", units =  "degC",  dim = list(dim_time, dim_ensemble_member, dim_scenario))
  ftot <- ncvar_def(name = "Effective Radiative Forcing", units =  "W/m2", dim = list(dim_time, dim_ensemble_member, dim_scenario))
  fco2 <- ncvar_def(name = "Effective Radiative Forcing|Anthropogenic|CO2", units =  "W/m2", dim = list(dim_time, dim_ensemble_member, dim_scenario))
  
  # Required as columns by RCMIP II 
  mname <- ncvar_def(name = "model", units = '', create_dimvar=FALSE)
  munit <- ncvar_def(name = "unit", units = 'character', dim = list(dim_ensemble_member, dim_scenario))
  
  # create netCDF file and put arrays, but first make sure that the 
  # netcdf does not exist, if the ncdf4 commands try to manipulate 
  # or overwrite a nc the resulting nc will be corrupt.  
  nc_name <- file.path(OUT_DIR, paste0('rcmipII-', scn_name, '.nc'))
  if(file.exists(nc_name)){ file.remove(nc_name) }
  ncnew <- nc_create(nc_name,list(tgav,ftot,fco2, mname, munit, varcolors),force_v4=TRUE)
  
  
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
  
  # Add the RCMIP II required columns, the examples don't really make sense but needed to pass the 
  # validation tests. 
  model <- array('hector', dim  = length(ensmble_members) * length(scn_name))
  ncvar_put(ncnew, "model", vals  = model)
  ncatt_put(ncnew, "model", attname = '_is_metadata', attval = 1)
  
  ncvar_put(ncnew, "unit", vals  = model)
  ncatt_put(ncnew, "unit", attname = '_is_metadata', attval = 1)
  
  # Put additional attributes into dimension and data variables
  ncatt_put(ncout,"lon","axis","X") 
  ncatt_put(ncout,"lat","axis","Y")
  ncatt_put(ncout,"time","axis","T")  

  # Add global attributes 
  ncatt_put(ncnew,  0, "created_at", as.character(Sys.time()))
  ncatt_put(ncnew, 0, "_scmdata_version", system('git describe', intern = TRUE))


  # Save the netcdf file. 
  nc_close(ncnew)
  
  
})




