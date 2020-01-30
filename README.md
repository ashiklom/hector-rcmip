# RCMIP 2019: Hector model

Code and data for [Hector](jgcri.github.io/hector) entry to the [Reduced Complexity Model Intercomparison Project (RCMIP)](https://www.rcmip.org/).

To reproduce the Tier 1 analysis:

- Install the required versions of all packages with `renv::restore()` (or `Rscript -e 'renv::restore()` from the shell).
  - `renv` should install itself, but if that fails, install it manually with `install.packages("renv")`.
- Run the `scripts/01-run-simulations.R` script to run the Hector simulations. This produces a _lot_ (~50 GB) of output, mostly because the probability runs are so large. The outputs are stored in the `output/zz-raw-output` directory, with the following subdirectories:
    - `probability` stores the raw probability simulations (one CSV file per run). This directory is huge (~45 GB), but is not required for post-processing.
    - `probability-processed` stores aggregated probability files (one [`fst`](https://www.fstpackage.org/) file per scenario). These files _are_ used for post-processing (because they are much faster to read), so if you want to do post-processing on a local machine, you will need to download this directory.
    - `single-run` stores the individual simulations corresponding to different CMIP6 models (one CSV file per simulation). This directory is also used for post-processing, so it should be synced to any machine where you want to do the post-processing.
- Run the `scripts/02-process-outputs.R` script to post-process the outputs.
