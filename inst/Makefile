all: inst/rcmip-inputs.fst

inst/rcmip-inputs.fst: data-raw/rcmip-concentrations-annual-means-v1-0-0.csv data-raw/rcmip-emissions-annual-means-v1-0-0.csv data-raw/rcmip-radiative-forcing-annual-means-v1-0-0.csv
	Rscript data-raw/process-inputs.R
