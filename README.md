# R-hts2
A modified version of Rob Hyndman's hts package, called hts2, which enables extra information to be extracted
from the forecast output.  This includes model information and prediction intervals for each node in the hierarchy.
These are enabled by setting keep.intervals=TRUE and keep.model=TRUE in the forecast.gts argument list.
It also prevents by default seasonal models being used at the lower levels if the top level model is not seasonal.
This can be overridden by setting the do.season=TRUE in the arguments to forecast.gts.

Note that the only function to be modified in this version is forecast.gts and you can use this version
even if you have the standard hts package installed, by using a namespace prefix to the function, e.g.
hts2::forecast.gts(...)

This has been updated to hts version 5.1.4 on 6-Nov-2017, by copying the v5.1.4 files to here.
