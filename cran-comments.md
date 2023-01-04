## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


This follows the transfer of functionalists of old GIS packages to terra, with rgdal, rgeos scheduled to be retired in 2023: <https://r-spatial.org/r/2022/04/12/evolution.html>. I removed dependencies on gDistance, raster, sp, rgeos, and rgdal.

Energy landscapes are now calculated using a zonal (kernel) based method, implemented in C++ functions. This is faster that previous versions, but it does not return the transition matrix or the conductance matrix. Because of this, least cost paths functions are not supported any more. Instead, use circuitscape or omniscape; see circuitscape_skeleton() and omniscape_skeleton() to generate the initialization files to run in Julia.
