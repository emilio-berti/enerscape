This follows the transfer of functionalists of old GIS packages to terra, with rgdal, rgeos scheduled to be retired in 2023: <https://r-spatial.org/r/2022/04/12/evolution.html>. I removed dependencies on gDistance, raster, sp, rgeos, and rgdal.

Energy landscapes are now calculated using a zonal (kernel) based method, implemented in C++ functions. This is faster that previous versions, but it does not return the transition matrix or the conductance matrix. Because of this, least cost paths functions are not supported any more. Instead, use circuitscape or omniscape; see circuitscape_skeleton() and omniscape_skeleton() to generate the initialization files to run in Julia.

## R CMD check results

Status: OK

## R CMD check --as-cran results

Status: OK

## R-hub results

### Windows Server 2022, R-devel, 64 bit

Build ID: enerscape_1.0.0.tar.gz-8f5975f993184b029bf3cb6ccebf56e6

Status: OK

### Fedora Linux, R-devel, clang, gfortran

Status: 1 Note

NOTES:

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found

### Ubuntu Linux 20.04.1 LTS, R-release, GCC

Status: OK
