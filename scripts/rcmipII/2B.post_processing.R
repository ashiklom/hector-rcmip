# RCMIP II is interested in the what they refer to as the Surface Air Ocean Blended Temperature Change 
# aka in other words the GMST instead of the GMAT which require a bit of post processing  
# prior to creating the netcdf files. 

# 0.Set Up -------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)

# Load the required packages. 
renv::restore()

# Define the directories input and output
BASE_DIR  <- here::here()
INPUT_DIR <- file.path(BASE_DIR, 'output', 'rcmipII', 'post_processed')
OUT_DIR   <- INPUT_DIR


# 1. Process Output Variables  -------------------------------------------------------------------------

files <- list.files(INPUT_DIR, '2A.', full.names = TRUE) 

lapply(files, function(f){
  
  message(f)
  
  # Read in data and format the data. 
  raw_data <- data.table::as.data.table(read.csv(f, stringsAsFactors = FALSE))
  cols     <- names(raw_data)[which(!names(raw_data) %in% c("variable", "ensemble"))]
  long_df  <- melt(raw_data, measure = cols, variable.name = 'year', value.name = 'value') 
  
  
  # Calculate total areosol forcing 
  # See https://rdrr.io/github/ashiklom/hector-rcmip/f/scratch/notes.md 
  # (BC + OC + SO2) * alpha 
  
  # Alpha was one of the tuned parameters and will need to be read in from the 
  # posterior csv file.
  posterior <- read.csv(url('https://zenodo.org/record/3990501/files/forcing_10k_noSLR_posteriorSamples.csv?download=1'), stringsAsFactors = FALSE)
  names(posterior) <- c('S', 'diff', 'alpha')
  posterior$ensemble <- 0:(nrow(posterior) - 1)
  posterior <- as.data.table(posterior)[ , list(ensemble, alpha)]
  
  assertthat::assert_that(all(c('FBC', 'FSO2d', 'FSO2i', 'FOC') %in% long_df$variable))
  sub <- long_df[variable %in% c('FBC', 'FSO2d', 'FSO2i', 'FOC'), ]
  aerosol_wide  <- dcast(sub, ensemble + year ~ variable)
  aerosol_wide <- aerosol_wide[posterior, on = "ensemble"]
  total_aerosol <- aerosol_wide[ , list(ensemble, year)]
  total_aerosol$value    <- aerosol_wide$alpha * (aerosol_wide$FBC + aerosol_wide$FOC + aerosol_wide$FSO2d + aerosol_wide$FSO2i)
  total_aerosol$variable <- 'F_aerosol'
  
  # Calculate the GMST from Hector output, the flnd and bsi are Hector parameters. 
  flnd <- 0.29; 
  bsi <- 1.3; 
  
  # Extract the two different types of air temps 
  assertthat::assert_that(all(c('Tgav_land', 'Tgav_ocean_air') %in% long_df$variable))
  temp_landair  <- long_df[variable =="Tgav_land", value]
  temp_oceanair <- long_df[variable == "Tgav_ocean_air", value]
  
  # Convert the ocean air temp back to sst. 
  temp_sst <- temp_oceanair / bsi
  
  # Calculate GMST by taking the average of sea surface temperature 
  # with the land air temperature, weighted by area.
  GMST_values <- flnd * temp_landair + (1 - flnd) * temp_sst
  GMST <- long_df[variable == "Tgav_land", ] # Copy over the data frame strucutre 
  GMST$value <- GMST_values                  # Replace the values 
  GMST$variable <- 'GMST'
  
  assertthat::assert_that(all(c("Tgav", "FCO2", "FN2O") %in% long_df$variable))
  
  # Now calculate the 
  ocean_heat_flux_value <- long_df[variable == 'heatflux', ]
  
  
  flnd <- 0.29                                  #fractional land area
  secs.per.year <- 31556926.0
  earth.area <- 5100656 * 10^8                  #m2
  ocean.area <- ( 1-flnd ) * earth.area 
  powtoheat <- ocean.area*secs.per.year / 10^22 #convert flux to total ocean heat
  
  
  list_df <- split(ocean_heat_flux_value, ocean_heat_flux_value$ensemble)
  lapply(list_df, function(x){
    
    val_sum <- cumsum(x[['value']]) * powtoheat #10^22 J
    x[['value']] <- val_sum
    return(x)
    
  }) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(variable = 'heat content') -> 
    heat_content
    
  
  # Create a data frame of the variables to be processed and format into the netcdf submissions.  
  # FYI this data frame has to be saved as a specific format for the L3 script. 
  dplyr::bind_rows(long_df[variable %in% c("Tgav", "FCO2", "FN2O", "Ftot"), ], total_aerosol, GMST, heat_content) %>%  
    dplyr::mutate(value = signif(value, digits = 6)) %>% 
    # Format into a wide data frame (efficent in terms of space)
    tidyr::pivot_wider(names_from = year, values_from = value) -> 
    out
  
  # Save the csv file.
  out_file <- paste0('2B.', gsub(pattern = '2A.',replacement = '', x = basename(f)))
  write.csv(out, file = file.path(OUT_DIR, out_file), row.names = FALSE)
  
})

# End

