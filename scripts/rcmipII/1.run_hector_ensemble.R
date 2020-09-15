# Run Hector 10,000 times using parameters from the UI calibration for the different RCMIP II scenarios
# using the hector-rcmip package can take a while and can use a lot of disk space. 
# So unfortunately this script can only be run for a single scenario at a time. 

# 0. Set Up --------------------------------------------------------------------------------------
# Load the correct version of the different R package dependencies. 
renv::restore() 
# Load the hector rcmip package to be able to use the run scneario functions and so on.
devtools::load_all() 

OUT_DIR <- here::here('output', 'rcmipII', 'raw-ensemble'); dir.create(OUT_DIR, showWarnings = FALSE)
assertthat::assert_that(dir.exists(OUT_DIR))

# 1. Import Parameter Combinations and Run Hector  -----------------------------------------------
# First start by downloading and formatting the ensemble information from the 
# the UI posterior sample. 
posterior <- read.csv(url('https://zenodo.org/record/3990501/files/forcing_10k_noSLR_posteriorSamples.csv?download=1'), stringsAsFactors = FALSE)
names(posterior) <- c('S', 'diff', 'alpha')
posterior$ensemble_member <- 0:(nrow(posterior) - 1)
posterior$ensemble_member <- sprintf(paste0('%0', nchar(max(posterior$ensemble_member)), 'd' ), posterior$ensemble_member )

# List of scnearios 
# "1pctCO2" #, "abrupt-4xCO2", "abrupt-2xCO2", "ssp119", "ssp126", "ssp585", "ssp370", "ssp434", "ssp460"
scenarios <- "XXX"

# Define a function that can run an ensemble of Hector 
#
# scenario: character string of the scenario to run 
# ensemble: a data frame of the Hector parameter combinations to run,  must contain the following columns,
#   ensemble_member, S, diff, and  alpha. 
run_ensemble <- function(scenario, ensemble){
  
  # Apply the ensemble information to a hector core using the rcmip  hector run_with_param function. 
  files <- apply(X = ensemble, MARGIN = 1, FUN = function(input){
    
    # Make sure that the ensmble input contains all of the required parameters. Write a message of 
    # the ensemble member to give the user an idea run status. 
    assertthat::assert_that(all(c('ensemble_member', 'S', 'diff', 'alpha') %in% names(input)))
    message(input[['ensemble_member']]) 
    
    # Define the location and the file to write out to.
    dir   <- file.path(OUT_DIR, scenario); dir.create(dir, showWarnings = FALSE)
    out_f <- file.path(dir, paste0(scenario, '_', input[['ensemble_member']], '.csv'))
    
    # Run the hector scenario with the inputs. 
    # tryCatch({ 
    # The try catch can be useful when debugging, or it can obsure problems. 
    # Leave the try catch infrastructure here incase it is needed.  
      run_with_param(scenario = scenario, 
                     # Make sure that the parameters read into Hector are numeric otherwise an error will be thrown.
                     pS = as.numeric(input[['S']]), 
                     pdiff = as.numeric(input[['diff']]), 
                     palpha = as.numeric(input[['alpha']]), outfile = out_f)
      # }, error = function(e){NA})
    
    
    return(out_f)
  })
  return(files)
}

# Execute the run ensemble funciton! 
lapply(scenarios, function(scn){
  
  run_time <- system.time({run_ensemble(scn, ensemble = posterior)})
  message(scn,'\n')
  message(run_time)
  message('------\n')
  
})
