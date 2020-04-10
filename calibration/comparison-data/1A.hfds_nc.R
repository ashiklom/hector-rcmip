## 1A.hfds_nc.R
## This script processes the cmip6 ocean heatflux data as comparison data for Hector.

# 0. Set Up ---------------------------------------------------------------------------------------------------
# Load the required libraries
devtools::load_all('/pic/projects/GCAM/Dorheim/cdoR')
library(dplyr)
library(tidyr)
library(ncdf4)

# Create directories
BASE_DIR  <- here::here('data-raw', 'comparison-data', 'cmip6')
INTER_DIR <- file.path(BASE_DIR, 'hfds'); dir.create(INTER_DIR, showWarnings = FALSE)
OUT_DIR   <- file.path(BASE_DIR, '1A-out')
dir.create(OUT_DIR, showWarnings = FALSE)

# A true/false indicator to determine if the intermediate csv files should be removed or not.
cleanUp <- TRUE

# 1. Find Files ---------------------------------------------------------------------------------------------
# Find the ocean heatflux and ocean cell area (it is important to note that using the ocean heat flux with the
# ocean heat flux because Omon and Amon variables have different dimensions).
cmip6_archive <- readr::read_csv(url("https://raw.githubusercontent.com/JGCRI/CMIP6/master/cmip6_archive_index.csv"))

cmip6_archive %>%
  dplyr::filter(variable == 'hfds' & domain == 'Omon') ->
  hfds_data

# Since we are intrerested in the ocean heat flux and the Omon hfds is reported only over
# the cell area can be processed without masking over the ocean or land.
cmip6_archive %>%
  dplyr::filter(variable %in% c('areacello')) %>%
  dplyr::select(-time, -domain, -type) %>%
  tidyr::spread(variable, file) %>%
  na.omit  ->
  meta_data

to_process <- dplyr::inner_join(hfds_data, meta_data, by = c("model", "experiment", "ensemble", "grid"))
to_process <- to_process[1:3,]
# 2. Get the Global Mean ---------------------------------------------------------------------------------------------

to_process %>%
  apply(1, function(x){

  # TODO functionalize this some how.
  input <- tibble::tibble(file = x[["file"]],
                          type = x[["type"]],
                          variable = x[["variable"]],
                          domain = x[["domain"]],
                          model = x[["model"]],
                          experiment = x[["experiment"]],
                          ensemble = x[["ensemble"]],
                          grid = x[["grid"]],
                          time = x[["time"]],
                          areacello = x[["areacello"]])

  info <- parse_cmip_info(input)
  base_name <- paste0(paste(info, collapse = '_'), x[['time']])
  outfile   <- file.path(OUT_DIR, paste0(base_name, '-hfdsMean.csv'))

  print('-----------------------------------------------')
  print(base_name)

  if(!file.exists(outfile)){

    out <- tryCatch({

      # Because the different modeling groups treat leap years and such differently use
      # cdo to calcualte the annual average weighted by the number of days in a month.
      # The modeling groups also may use different calanders, some assume 30 days in each
      # month where as others assume 31 days in each month.
      annual_nc <- cdo_yearmonmean(name = base_name, input[['file']], intermed_dir = INTER_DIR)

      # Calculate the weighted global average from the annual data.
      fldmean_area(info, in_nc = annual_nc, area_nc = input[['areacello']], area_var = 'areacello')

    }, error = function(e){
      cbind(info,
            problem = TRUE)
    })

    write.csv(out, file = outfile, row.names = FALSE)

  }

  outfile

}) ->
  files

print('done processing')

out     <- dplyr::bind_rows(lapply(files, read.csv, stringsAsFactors = FALSE))
outfile <- file.path(OUT_DIR, '1A.hfds-output.csv')
write.csv(out, file = outfile, row.names = FALSE)

print('done saving')

if(cleanUp) {
  file.remove(list.files(INTER_DIR, full.names = TRUE))
  file.remove(files)
  }
print('done! done!')
