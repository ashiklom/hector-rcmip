# Run Hector the different RCMIP CMIP6 scenarios to generate a large ensemble of
# Hector runs to be used  to standardize the climate data during the calibration 
# process. 
# 0. Set Up ------------------------------------------------------------------------------
# Load  the rquired libs
devtools::load_all()
library(hector)
library(dplyr)
library(tidyr)
library(assertthat)
source(here::here('calibration', 'hectorcal-functions.R'))

# Define the dirs
OUT_DIR <- here::here('calibration', 'ensemble-runs', 'raw-output');
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Define the prior hyper-parameters, these should be the same as the hyper-parameters defined in
# the likelihood function. These will be used as default inputs into functions defined
# in section 1 to create the prior distirbtuion.
hyper_params <- list(ecsmu = 3,.0,
                     ecssig = 3.0,
                     aeromu = 1.0,
                     aerosig = 1.4,
                     volcanomu = 1.0,
                     volcanosig = 1.4,
                     kappamu = 2.3,
                     kappasig = 2.0,
                     betamu = 0.3,
                     betasig = 0.7,
                     q10mu = 2.0,
                     q10sig = 2.0,
                     c0mu = 285,
                     c0sig = 14.0)

# Make the random generator reproducible
set.seed(867-5309)

# Define the number of runs to do and the number of years to save data for.
n_runs <- 2500
years  <- 1745:2500


# 1. Define Functions ------------------------------------------------------------------------------
# TODO this is a relic code from early hectorcal days may be able to clean up.
# Randomly generate carbon values based on the prior distribution. These are the
# parameters used in the emission driven runs. Will be used in the hector_sample function.
#
# Args
#   ecsmu: the climate sensitivity mean, default is set to the value in the hyper parameter defined in section 0
#   ecssig: the climate sensitivity  standard deviation
#   aeromu: the aerosol scalar mean
#   aerosig: the aerosol standard deviation
# Returns: a list of functions that will sample the climate sensitivity, aero scaler, and ocean diffusivity
# parameter spaces. Each function in the list will sample the prior distribution n times.
make_concen_dist <- function(ecsmu = hyper_params$ecsmu, ecssig = hyper_params$ecssig,
                             aeromu = hyper_params$aeromu, aerosig = hyper_params$aerosig,
                             volcanomu = hyper_params$volcanomu, volcanosig = hyper_params$volcanosig,
                             kappamu = hyper_params$kappamu, kappasig = hyper_params$kappasig){
  
  list <- list(function(n) {rlnorm(n = n, meanlog = log(ecsmu), sdlog = log(ecssig))},
               function(n) {rnorm(n = n, mean = aeromu, sd = aerosig)},
               function(n) {rnorm(n = n, mean = volcanomu, sd = volcanosig)},
               function(n) {rnorm(n = n, mean = kappamu, sd = kappasig)})
  # Name the functions returned in the list by hector parameter
  names(list) <- c(ECS(), AERO_SCALE(), VOLCANIC_SCALE(), DIFFUSIVITY())
  
  list
}

# Generate an ensemble of Hector runs
#
# Args
#   params: a data frame of the parameters to be perturbed
#   core: the hector cores to use
#   name: the core name
#   years: the years of data to keep
#   out_dir: the name of the directory where to write the Hecctor results to
# Returns: This function writes a csv file for the enesemble of runs, including those that
# did not work.
sample_hector <- function(params, core, name, years = 1750:2300, out_dir = OUT_DIR){
  apply(params, MARGIN = 1, function(p){
    # Select the hector parameters from the list of params (it inslucde the param run name)
    hector_params <- p[names(p) != 'indx']
    # Try to run hector with the parameters, however if an error occurs (which may happen)
    # when poor combinations of hector parameters are used then return an empty data frame.
    tryCatch({
      hectorcal.parameterize_core(params=hector_params, core = core)
      reset(core)
      run(core, runtodate = max(years))
      fetchvars(core, years, vars = c(GLOBAL_TEMP(), HEAT_FLUX())) %>%
        mutate(runid = p[['indx']]) 
    }, error = function(e){
      data.frame(runid = p[['indx']],
                 value = NA,
                 variable = NA,
                 scenario = NA,
                 year = NA)
      
    })
    
  }) %>%
    bind_rows() %>%
    left_join(params, by = c('runid' = "indx")) %>%
    write.csv(file = file.path(out_dir, paste0(name, '.csv')), row.names = FALSE)
}

# 2. Run Hector to Generate the Ensembles --------------------------------------------------------------
# Format the parameter combinations to use
concentration_dists <- make_concen_dist()
params <- as.data.frame(lapply(concentration_dists, function(f){f(n_runs)}))
params$indx <- 1:nrow(params)

scenarios <- c("piControl", "esm-piControl", "1pctCO2", "1pctCO2-4xext",
               "abrupt-4xCO2", "abrupt-2xCO2", "abrupt-0p5xCO2",
               "ssp119", "ssp585", 'ssp245', 'ssp126', 'ssp370', 
               'ssp434', 'ssp460', 'ssp370')

# Run Hector!
total_runs <- length(scenarios) * n_runs
message(cat('Hector will be run ', total_runs, ' times'))

# Start by generating the Hector cores
rcmip_hector_cores <- lapply(scenarios, function(scn){
  core <- run_scenario(scn, return_core = TRUE)
  reset(core)
}) 

# Run Hector using the different parameters
mapply(function(x1, x2){
  sample_hector(params = params, core = x1, years = 1745:2100, out_dir = OUT_DIR, name = x2)
}, 
x1 = rcmip_hector_cores, 
x2 = scenarios)
