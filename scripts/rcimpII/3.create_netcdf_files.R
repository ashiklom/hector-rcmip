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
scn_to_process <- 'ssp370'
files <- list.files(DATA_DIR)
file <- files[grepl(pattern = scn_to_process, x = files)]

# Read in the ensemble of the Hector results.  
data <- read.csv(file.path(DATA_DIR, file),stringsAsFactors = FALSE)
scn_name <- gsub(pattern = '_subset_hector.csv', replacement = '', x = file)

# 2. Create the Netcdf File ----------------------------------------------------------------------
# Define some information about the dimensions of the expected data. 
# For now this is hard coded but could 
ensmble_members <- unique(data$ensemble)
time_years <- as.integer(gsub(x = names(data)[grepl(pattern = 'X', x = names(data))], pattern = 'X', replacement = ''))
  
# Define the dimensions of the netcdf file based on the contents of the csv file.  
ensemble <- ncdim_def("ensemble_member", "character", as.integer(ensmble_members))
time <- ncdim_def("time", "Year", time_years, unlim=TRUE)
scn  <- ncdim_def("scenario", scn_name, 1, unlim=TRUE)

# Define the variables to save. 
# TODO make sure that these names match the phase II names.
tgav <- ncvar_def(name = "Global_Mean_Temperature", units =  "degC",  dim = list(time, ensemble, scn), missval = NaN)
ftot <- ncvar_def(name = "Total_Radiative_Forcing", units =  "W/m2",  dim = list(time, ensemble, scn), missval = NaN)
fco2 <- ncvar_def(name = "CO2_Radiative_Forcing", units =  "W/m2",  dim = list(time, ensemble, scn), missval = NaN)
heat <- ncvar_def(name = "Heat_Flux", units =  "W/m2",  dim = list(time, ensemble, scn), missval = NaN)

# Save the empty netcdf file on the disk. 
nc_name <- file.path(OUT_DIR, paste0('rcmipII-', scn_name, '.nc'))
ncnew <- nc_create(nc_name, vars = list(tgav, ftot, fco2, heat))

# Open the empty netcdf to add attributes and values. 
nc <-  nc_open(nc_name, write = TRUE)

# Add the required attributes to the netcdf files. 
ncatt_put(nc, "Global_Mean_Temperature", attname = 'climate_model', attval = 'hector')
ncatt_put(nc, "Total_Radiative_Forcing", attname = 'climate_model', attval = 'hector')
ncatt_put(nc, "CO2_Radiative_Forcing", attname = 'climate_model', attval = 'hector')
ncatt_put(nc, "Heat_Flux", attname = 'climate_model', attval = 'hector')

ncatt_put(nc, "Global_Mean_Temperature", attname = 'region', attval = 'World')
ncatt_put(nc, "Total_Radiative_Forcing", attname = 'region', attval = 'World')
ncatt_put(nc, "CO2_Radiative_Forcing", attname = 'region', attval = 'World')
ncatt_put(nc, "Heat_Flux", attname = 'region', attval = 'World')

# Add the temp data to the netcdf file. 
data %>% 
  dplyr::filter(variable == 'Tgav') %>% 
  dplyr::select(-variable, -ensemble) %>% 
  as.matrix() %>% 
  t() ->
  temp_data

ncvar_put(nc, "Global_Mean_Temperature", vals = temp_data)

# Add the total raditaive forcing to the netcdf file. 
data %>% 
  dplyr::filter(variable == 'Ftot') %>% 
  dplyr::select(-variable, -ensemble) %>% 
  as.matrix() %>% 
  t() ->
  ftot_data

ncvar_put(nc, "Total_Radiative_Forcing", vals = ftot_data)

# Add the RF CO2 values. 
data %>% 
  dplyr::filter(variable == 'FCO2') %>% 
  dplyr::select(-variable, -ensemble) %>% 
  as.matrix() %>% 
  t() ->
  fco2_data

ncvar_put(nc, "CO2_Radiative_Forcing", fco2_data)

# Add heat flux data values. 
data %>% 
  dplyr::filter(variable == 'heatflux') %>% 
  dplyr::select(-variable, -ensemble) %>% 
  as.matrix() %>% 
  t() ->
  heatflux_data
ncvar_put(nc, "Heat_Flux", heatflux_data)

# Save the netcdf file. 
nc_close(nc)
