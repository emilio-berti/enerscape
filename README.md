
<!-- README.md is generated from README.Rmd. Please edit that file -->

# enerscape

Calculate Energy Landscapes For Terrestrial Animals.

<!-- badges: start -->

[![R-CMD-check](https://github.com/emilio-berti/enerscape/workflows/R-CMD-check/badge.svg)](https://github.com/emilio-berti/enerscape/actions)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7445654.svg)](https://doi.org/10.5281/zenodo.7445654)

<!-- badges: end -->

# Changelog

## 1.0.0

This follows the transfer of functionalists of old GIS packages to _terra_, with _rgdal_, _rgeos_ scheduled to be retired in 2023: <https://r-spatial.org/r/2022/04/12/evolution.html>. I removed dependencies on _gDistance_, _raster_, _sp_, _rgeos_, and _rgdal_.

Energy landscapes are now calculated using a zonal (kernel) based method, implemented in C++ functions.
This is faster that previous versions, but it does not return the transition matrix or the conductance matrix.
Because of this, least cost paths functions are not supported any more.
Instead, use circuitscape or omniscape; see *circuitscape_skeleton()* and *omniscape_skeleton()* to generate the initialization files to run in Julia.

The cyclist model is also not supported any more; custom models must be written in the C++ functions.
I plan to add a customizable function soon to do that.

## Installation

You can install the released version of enerscape from CRAN with:

```r
install.packages("enerscape")
```

And the development version from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("emilio-berti/enerscape")
```

To start with enerscape, check the vignette:
```r
vignette("enerscape")
```

## Releases

All releases are also archived on GitHub at <https://github.com/emilio-berti/enerscape/releases>.

From release 1.0.0, enerscape is also archived in Zenodo: <https://doi.org/10.5281/zenodo.7445654>.

