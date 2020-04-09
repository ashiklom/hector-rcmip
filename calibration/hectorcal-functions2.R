devtools::load_all()
library(assertthat)
library(ggplot2)

#' Set parameters into a hector core
#'
#' Set hector parameters by name.  Parameters not mentioned in the input vector
#' will be left at their current values.  After the new parameters are set, the
#' core is reset.
#'
#' @param params A named vector of Hector parameters.
#' @param core A Hector core.
#' @return The input core, with parameters reset and spinup rerun.
#' @export
#' @author Kalyn Dorheim
hectorcal.parameterize_core <- function(params, core) {
  
  pnames <- names(params)
  assertthat::assert_that(!is.null(pnames), msg = 'params must be named.')
  
  punits <- hector::getunits(pnames)
  assert_that(!any(is.na(punits)), msg='Bogus parameter names')
  
  mapply(function(x, y, z){hector::setvar(core = core, dates = NA, var = x, values = y, unit = z)},
         x = pnames, y = params, z = punits)
  
  hector::reset(core = core)
}


#' Calculate the error between the hector and comparison data
#'
#' @param comp_data a data frame of the standardized comparison data. 
#' @param hector_data a data frame of the standardized hector data. 
#' @return The error which is a weighted MSE by variable / scenario
#' @export
#' @author Kalyn Dorheim
hectorcal.calculate_MSE <- function(comp_data, hector_data){
  
  req_names <- c('year', 'variable', 'scenario')
  assertthat::assert_that(all(c(req_names, 'comp_value') %in% names(comp_data)))
  assertthat::assert_that(all(c(req_names, 'hec_value') %in% names(hector_data)))
  
  dplyr::inner_join(comp_data, hector_data,  by = c("year", "variable", "scenario")) %>% 
    dplyr::mutate(SE = (comp_value - hec_value)^2) %>% 
    dplyr::group_by(variable, scenario) %>%  
    dplyr::summarise(MSE = mean(SE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario) %>%
    dplyr::summarise(value = mean(MSE)) %>%
    dplyr::ungroup() %>%
    dplyr::pull(value) %>%
    mean()
  
}


# Args 
#   comp_data: a data frame of the ESM comparison data 
# Returns: an error value, measured as MSE. Note that if an error occurs here the function will return a large value. 
hectorcal.make_min_fn <- function(comp_data){
  
  # Use the comparison data to set up the cores
  req_cols <- c('year', 'value', 'units', 'model', 'variable', 'scenario', 'ensemble')
  assertthat::assert_that(all( req_cols %in% names(comp_data)))
  
  scns  <- unique(comp_data$scenario)
  scns  <- scns[!scns %in% c('esm-hist')] # the esm-hist core was not set up to run 
  suppressWarnings(cores <- lapply(scns, run_scenario, return_core = TRUE))
  
  # Now standardize the the comparison data. 
  comp_data %>% 
    dplyr::mutate(comp_value = value) -> 
    comp_data
  
  # Define the function that will be used my optim to minimize
  function(param, diagnostic_mode = FALSE){
    
    # Use a try Catch to catch errors, so that an error will not break the 
    # optim routine.
    tryCatch({
      # Run each of the Hector cores with the parameter values. 
      lapply(cores, function(hc){
        
        hectorcal.parameterize_core(params = param, core = hc)
        hector::reset(core = hc)
        hector::run(core = hc, runtodate = 2100)
        hector::fetchvars(core = hc, dates = 1850:2100, vars = c(hector::GLOBAL_TEMP(), hector::HEAT_FLUX())) 
        
      }) %>% 
        dplyr::bind_rows() -> 
        hector_results
      
      hector_results %>% 
        dplyr::select(year, variable, scenario, units, hec_value = value) -> 
        hector_data
      
      # If running in the diagnostic mode then return the intermediate outputs. 
      if(diagnostic_mode){
        
        req_names <- c('year', 'variable', 'scenario')
        assertthat::assert_that(all(c(req_names, 'comp_value') %in% names(comp_data)))
        assertthat::assert_that(all(c(req_names, 'hec_value') %in% names(hector_data)))
        
        dplyr::inner_join(comp_data, hector_data,  by = c("year", "variable", "scenario")) %>% 
          dplyr::mutate(SE = (comp_value - hec_value)^2) %>% 
          dplyr::select(-units.y, -units.x) %>% 
          na.omit()
        
      } else {
        # Now calucalte the MSE 
        MSE <- hectorcal.calculate_MSE(hector_data = na.omit(hector_data), comp_data = na.omit(comp_data))
        MSE
      }
      
      
    }, error = function(e){8675309})
    
  }
  
}



# Args 
#   comp_data: a data frame of ESM comparison data to use in the calibration process
#   stand: a data frame of the Hector ensemble mean/sd to use to standardize the climate data. 
#   inital_param: a vector a guess of parameter values 
#   maxit: the max number of itteractions for the solver, set to the default value of 500
# Returns: and object from optim
hectorcal.calibrate <- function(comp_data, inital_param, maxit = 500){
  # Check the input data 
  assertthat::assert_that(length(unique(comp_data$model)) == 1)
  assertthat::assert_that(sum(is.factor(comp_data)) == 0, msg = 'comp_data df cannot contain factors')
  
  # This function can take a while because of the way that the run_scenarios 
  # is used. 
  fn <- hectorcal.make_min_fn(comp_data = comp_data)
  
  # Use optim to minimize the MSE between Hector and ESM output data
  stats::optim(par = initial_param, fn = fn, control = list('maxit' = maxit))
  
}




hectorcal.calibrate_diagnostic <- function(comp_data, inital_param, maxit = 500){
  # Check the input data 
  assertthat::assert_that(length(unique(comp_data$model)) == 1)
  assertthat::assert_that(sum(is.factor(comp_data)) == 0, msg = 'comp_data df cannot contain factors')
  
  # This function can take a while because of the way that the run_scenarios 
  # is used. 
  fn <- hectorcal.make_min_fn(comp_data = comp_data)
  
  # Use optim to minimize the MSE between Hector and ESM output data
  rslt <- stats::optim(par = initial_param, fn = fn, control = list('maxit' = maxit))
  
  if(rslt$convergence == 0){
    
    plots <- list()
    data  <- list()
    
    data[['standardized_data']] <- fn(param = rslt$par, diagnostic_mode = TRUE)
    
    model <- unique(comp_data$model)
    ggplot(data = data$standardized_data) + 
      geom_point(aes(year, comp_value, color = 'ESM model')) + 
      geom_point(aes(year, hec_value, color = 'Hector')) + 
      facet_grid(variable~scenario) + 
      theme_bw() + 
      labs(y = 'standardized variable values', 
           title = 'Standardized Data Used to Calulcate the SE', 
           subtitle = model)-> 
      plots[['standardized']]
    
    ggplot(data = data$standardized_data) + 
      geom_point(aes(year, SE)) + 
      theme_bw() +
      facet_grid(variable~scenario, scales = 'free') + 
      labs(title = 'The Squared Error Between Standardized Data') -> 
      plots[['SE']]
    
    # Use a try Catch to catch errors, so that an error will not break the 
    # optim routine.
    tryCatch({
      
      scns  <- unique(comp_data$scenario)
      scns  <- scns[!scns %in% c('esm-hist')] # the esm-hist core was not set up to run 
      suppressWarnings(cores <- lapply(scns, run_scenario, return_core = TRUE))
      
      # Run each of the Hector cores with the parameter values. 
      lapply(cores, function(hc){
        
        hectorcal.parameterize_core(params = rslt$par, core = hc)
        hector::reset(core = hc)
        hector::run(core = hc, runtodate = 2100)
        hector::fetchvars(core = hc, dates = 1850:2100, vars = c(hector::GLOBAL_TEMP(), hector::HEAT_FLUX()))
        
      }) %>% 
        dplyr::bind_rows() -> 
        hector_results
    }, 
    error = function(e){
      data.frame(value = NA, 
                 year = NA, 
                 variable = NA, 
                 scenario = NA, 
                 units = NA, 
                 model = NA)}) -> 
      hector_results
    
    # Now that we have the Hector results add the comparison information 
    hector_results %>%  
      dplyr::rename(hector_value = value) %>%  
      dplyr::full_join(comp_data %>% 
                         dplyr::select(year, ESM_value = value, scenario, variable, model, variable), 
                       by = c("scenario", "year", "variable")) -> 
      data[['hector_comparison_data']]
    
    
    ggplot(data = data[['hector_comparison_data']]) + 
      geom_point(aes(year, ESM_value, color = 'ESM model')) + 
      geom_line(aes(year, hector_value, color = 'Hector')) + 
      facet_grid(variable~scenario, scales = 'free') + 
      theme_bw() + 
      labs(y = 'Variable Values', 
           title = 'Calibration Data vs. Hector', 
           subtitle = model) + 
      scale_color_manual(values = c('ESM model' = 'grey', 
                                    'Hector' = 'blue')) ->
      plots[['hector_comparison_data']]
    
    # rslt$data <- data 
    rslt$plots <- plots 
    
  } 
  
  rslt
  
}

# TODO 
# need some way to do the optmization routine decently, do you think we can start from default parameters? 
# as the inital guess? If not then we will need some way to generate the inital parameter guesses.
# which could use the -goog-runs.csv file but that might also take a long time to run, which is annoying. 
# Then it should be a matter of taking the results from the optim routine and using them to plot! the restuls, 
# now is that something I would want to do inside of the optmize hector results? 
# I think that could be it because that would make things easier than always having to regenerate them. 
# Would also want to save a copy of the actual results... 



