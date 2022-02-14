
<!-- README.md is generated from README.Rmd. Please edit that file -->

# enerscape

<!-- badges: start -->

[![R-CMD-check](https://github.com/emilio-berti/enerscape/workflows/R-CMD-check/badge.svg)](https://github.com/emilio-berti/enerscape/actions)
<!-- badges: end -->

The goal of enerscape is to calculate energy landscapes for terrestrial
animals.

## Installation

You can install the released version of enerscape from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("enerscape")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("emilio-berti/enerscape")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(raster)
#> Loading required package: sp
library(enerscape)
data("volcano")
dem <- raster(volcano)
en <- enerscape(dem, 100, "kcal")
#>  - Raster cells are assumed to have same horizontal and vertical resolution and with planar coordinate reference system (e.g. UTM)
#>   | Calculating slope
#>   | Calculating work
#>   | Calculating conductance (1 / work)
#>  - Do not use slope with negative values for distance calculations
en
#> $neighbors
#> [1] 16
#> 
#> $mass
#> [1] 100
#> 
#> $rasters
#> class      : RasterStack 
#> dimensions : 87, 61, 5307, 4  (nrow, ncol, ncell, nlayers)
#> resolution : 0.01639344, 0.01149425  (x, y)
#> extent     : 0, 1, 0, 1  (xmin, xmax, ymin, ymax)
#> crs        : NA 
#> names      :           DEM,         Slope,   EnergyScape,   Conductance 
#> min values :  9.400000e+01, -8.947524e+01,  4.775944e-04,  8.519589e-02 
#> max values :     195.00000,      89.58103,      18.56610,     654.32092 
#> 
#> 
#> $cond_tr
#> class      : TransitionLayer 
#> dimensions : 87, 61, 5307  (nrow, ncol, ncell)
#> resolution : 0.01639344, 0.01149425  (x, y)
#> extent     : 0, 1, 0, 1  (xmin, xmax, ymin, ymax)
#> crs        : NA 
#> values      : conductance 
#> matrix class: dgCMatrix 
#> 
#> attr(,"class")
#> [1] "enerscape"
```
