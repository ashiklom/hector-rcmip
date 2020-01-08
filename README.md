# RCMIP 2019: Hector model

Code and data for [Hector](jgcri.github.io/hector) entry to the [Reduced Complexity Model Intercomparison Project (RCMIP)](https://www.rcmip.org/).

To reproduce the Tier 1 analysis:

- Install the required versions of all packages with `renv::restore()` (or `Rscript -e 'renv::restore()` from the shell).
  - `renv` should install itself, but if that fails, install it manually with `install.packages("renv")`.
- Run the `scripts/tier-1-scenarios.R` script (`source("scripts/tier-1-scenarios.R")`, or `Rscript scripts/tier-1-scenarios.R` from command line)
