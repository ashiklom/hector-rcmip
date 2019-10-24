# Notes

- Aerosols
    - In Hector, aerosol forcing is calculated as RF~BC~ + RF~OC~ + RF~SO2d~ + RF~SO2i~, multiplied by aerosol scaling factor, where:
        - BC -- Black carbon. This comes from BC emissions * 
        - OC -- Organic carbon
        - SO2d -- Direct contribution of SO2 forcing
        - SO2i -- Indirect contribution of SO2 forcing
        
- Hector's inputs:
    - `ffi_emissions` -- Fossil-fuel CO2 emissions (PgC year^-1^).
        - Use `Emissions|CO2`
    - `luc_emissions` -- Land-use change emissions.
        - Not provided
        
## Questions

- What to do about natural CH4 emissions (CH4N)?
- What to do about natural N2O emissions?
- In RCMIP, what is the interpretation of "Mt NOx year-1"? Specifically, how to convert to Tg N for Hector?
- Is RCMIPs VOC the same as Hector's NMVOC?

## Misc

How to run Hector with abrupt CO2 forcing (from Adria, by way of Kalyn):

> "We ran Hector-doeclim with few changes to the default configuration file settings. We enabled the `[temp_doeclim]` component and the `volcanic_forcing` component. We changed two model time steps in Hector-doeclim: (1) the `carbon-cycle-solver.cpp` time step from `dt(0.3)` to `dt(0.1)` and (2) the `ocean_component.hpp` `OCEAN_MIN_TIMESTEP` from 0.3 to 0.01 to allow for the carbon cycle, in particular the ocean carbon cycle to better respond to these perturbations."
