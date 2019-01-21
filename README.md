# R-hts2
A modified version of Rob Hyndman's hts package, called hts2, which enables extra information
to be extracted from the forecast output.  This includes model information and
prediction intervals for each node in the hierarchy.
These are enabled by setting keep.intervals=TRUE and keep.model=TRUE arguments to forecast.gts.

It also prevents by default seasonal models being used at the lower levels
if the top level model is not seasonal.
This can be overridden by setting the do.season=TRUE in the arguments to forecast.gts.

Note that the only function to be modified in this version is forecast.gts and
so you can use this version even if you have the standard hts package installed,
by using a namespace prefix to the function, e.g.  hts2::forecast.gts(...)

This has been updated to hts version 5.1.4 on 6-Nov-2017, by copying the v5.1.4 files to here.
However it has version number 5.1

Further updates in Jan-2019 include additional arguments to forecast.gts:
allow.negative - (default TRUE), if FALSE will truncate all negative forecasts to zero
		as these affect higher level hts values leading to weird results.  This
		is included as using box-cox transformation with lambda=0 seems to be unreliable.
allow.reduced - (default FALSE), if TRUE will remove duplicate forecasts from the set
		of forecasts and then copy results from the related forecast back into the output
Other changes include changing fmodel = "rw" so that it uses arima with model=c(0,1,0) rather than
calling rwf() so that we can get the same residual, interval, fitted and model information
back from the forecast loop in forecast.gts function
