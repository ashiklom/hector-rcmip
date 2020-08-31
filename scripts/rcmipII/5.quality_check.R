# Plot the results that will be submitted to RCMIP II. 

# 0. Set Up ---------------------------------------------------------------------------------
# Load the required libraries. 
renv::restore()
library(ggplot2)
library(magrittr)
library(ncdf4)
library(tidyr)

# Define the directories.
OUT_DIR  <- here::here('output', 'rcmipII', 'figs'); dir.create(OUT_DIR, showWarnings = FALSE) 
COMP_DIR <- here::here('data-raw', 'cmip6_comp_data')

# 1. Set Up ---------------------------------------------------------------------------------
# Find the files to plot. 
nc_files <- list.files(here::here('output', 'rcmipII', 'netcdfs'), full.names = TRUE)

# Generate the plots. 
plot_list <- lapply(nc_files, function(f){
  
  sn_name <- gsub(pattern = '.nc', replacement = '',  x = gsub(x = basename(f), pattern = 'rcmipII-', replacement = ""))
  message(sn_name)
  nc <- nc_open(f)
  temp <- ncvar_get(nc,  'Global_Mean_Temperature')
  time <- ncvar_get(nc, 'time')
  
  colnames(temp) <- 0:(ncol(temp)-1)
  df <- as.data.frame(temp)
  df$year <- time
  long_df <- pivot_longer(df, cols = as.character(0:9999), values_to = "value", names_to  = 'ensemble')
  long_df <- dplyr::filter(long_df, year  %in% seq(from = 1750, to = 2100, by = 5))
  
  ggplot(data = long_df) + 
    geom_line(aes(year, value, group = ensemble), alpha = 0.05) + 
    theme_bw() + 
    labs(y  = 'Global Mean Temp Deg C', 
         title = sn_name) -> 
    plot1
  
    ggsave(plot1, filename = file.path(OUT_DIR, paste0('tseries-', sn_name, '.pdf')))
  
  
  long_df %>% 
    dplyr::filter(year == 2100) %>% 
    ggplot() + 
    geom_density(aes(value)) +
    theme_bw() + 
    labs(x = '2100 Global Mean Temp Deg C',  
           title = sn_name) -> 
    plot2 
  
    ggsave(plot2, filename = file.path(OUT_DIR, paste0('2100density-', sn_name, '.pdf')))
    
    return(list('ts' = plot1, 'yr2100' = plot2))
  
})
names(plot_list) <- gsub(pattern = '.nc', replacement = '', x = basename(nc_files))

saveRDS(plot_list, file = file.path(OUT_DIR, 'plot_list.rds'))
