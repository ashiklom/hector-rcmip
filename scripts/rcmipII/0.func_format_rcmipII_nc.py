import datetime as dt
import os

import numpy as np
import tqdm
import xarray as xr


def convert_to_rcmip_nc(fname):
    ds = xr.load_dataset(fname)

    # convert years to datetimes
    ds["time"] = np.array([dt.datetime(y, 1, 1) for y in ds["time"][:]], dtype="datetime64[s]").astype("float")

    for v in ds.variables:
        ds_variable = ds.variables[v]

        if v not in ["time", "ensemble_member"]:
            # rename units
            ds_variable.attrs["unit"] = ds_variable.attrs["units"]
            del ds_variable.attrs["units"]

            # Add scenario and model info
            ds_variable.attrs["scenario"] = ds.attrs["scenario"]
            ds_variable.attrs["model"] = ds.attrs["model"]
    # Write out as rcmipII-{sce}-converted.nc
    ds.to_netcdf(os.path.join(root_dir, "{}-converted.nc".format(fname[:-3])))


if __name__ == "__main__":
    root_dir = "/Users/dorh012/Documents/2020/hector-rcmip/output/rcmipII/netcdfs"

    scenarios = [
       "1pctCO2", "ssp370",  "abrupt-2xCO2", "abrupt-4xCO2", "ssp119", "ssp126", "ssp434", "ssp460", "ssp585", "ssp245"
    ]

    for sce in tqdm.tqdm(scenarios):
        fname = os.path.join(root_dir, "rcmipII-{}.nc".format(sce))
        convert_to_rcmip_nc(fname)
