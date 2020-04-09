## 1A.tas_nc.R
## This script processes the cmip6 temperature data as comparison data for Hector.
# TODO this script needs to be updated to be relative to hector-rcmip dir. 
# 0. Set Up ---------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
#library(cdoR) # This pacakge should have been recently built or install from devt
devtools::load_all('/pic/projects/GCAM/Dorheim/cdoR')

BASE_DIR <- here::here('data-raw', 'comparison-data', 'cmip6')
OUT_DIR  <- file.path(BASE_DIR, '1A-out')
dir.create(OUT_DIR, showWarnings = FALSE)

# A true/false indicator to determine if the intermediate csv files should be removed or not.
cleanUp <- FALSE


# 1. Find Files to Process -------------------------------------------------------------------------------------
# Find the temperature and cell area data to process based on the contents of the cmip6 archive.
cmip6_archive <- readr::read_csv(url("https://raw.githubusercontent.com/JGCRI/CMIP6/master/cmip6_archive_index.csv"))

cmip6_archive %>%
  dplyr::filter(variable == 'tas' & domain == 'Amon') ->
  tas_data

# Find the required meta data, since we only calculating global mean temperature the area
# weight that will be use is the areacella file.
cmip6_archive %>%
  dplyr::filter(variable %in% c('areacella')) %>%
  dplyr::select(-time, -domain, -type) %>%
  tidyr::spread(variable, file) %>%
  na.omit  ->
  meta_data

to_process <- dplyr::inner_join(tas_data, meta_data, by = c("model", "experiment", "ensemble", "grid"))

# 2. Get Global Mean ---------------------------------------------------------------------------------------------------

apply(to_process, 1, function(x){

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
                          areacella = x[["areacella"]])

  info <- parse_cmip_info(input)
  base_name <- paste0(paste(info, collapse = '_'), x[['time']])
  outfile <- file.path(OUT_DIR, paste0(base_name, '-tasMean.csv'))

  if(!file.exists(outfile)){

    out <- tryCatch({

      # Because the different modeling groups treat leap years and such differently use
      # cdo to calcualte the annual average weighted by the number of days in a month.
      # The modeling groups also may use different calanders, some assume 30 days in each
      # month where as others assume 31 days in each month.
      annual_nc <- cdo_yearmonmean(name = base_name, input[['file']], intermed_dir = OUT_DIR)

      # Calculate the weighted global average from the annual data.
      fldmean_area(info, in_nc = annual_nc, area_nc = input[['areacella']])

    }, err = function(e){
      cbind(info,
            problem = TRUE)
    })

    write.csv(out, file = outfile, row.names = FALSE)

  }


  outfile

}) ->
  files


# 3. Save and Clean Up ----------------------------------------------------------------------------
out     <- dplyr::bind_rows(lapply(files, read.csv, stringsAsFactors = FALSE))
outfile <- file.path(OUT_DIR, '1A.tas-output.csv')
write.csv(out, file = outfile, row.names = FALSE)

if(cleanUp) {file.remove(files)}

