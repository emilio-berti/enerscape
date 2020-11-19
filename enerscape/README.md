
<!-- README.md is generated from README.Rmd. Please edit that file -->

# enerscape

<!-- badges: start -->

<!-- badges: end -->

The goal of enerscape is to …

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
library(enerscape)
library(terra)
#> terra version 0.8.6 (beta-release)

dem <- rast("../data/dem-abruzzi.tif")
plot(dem)
```

<img src="man/figures/README-simple example-1.png" width="100%" />

``` r
en <- enerscape(dem, m = 140, units = "kcal") #typical female Marsican bear
#> Calculating slope
#> Warning in .fromRasterLayerBrick(from): changed NA value ignored
#> Calculating work
plot_enerscape(en, what = "work", n_contour = 5)
```

<img src="man/figures/README-simple example-2.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!

``` r
library(raster)
#> Loading required package: sp
library(enerscape)
library(rasterVis)
#> Loading required package: lattice
#> Loading required package: latticeExtra

s <- readRDS("../data/stack3D.rds")
r <- raster("../data/cs_cum_curmap.asc")
r[r == 0] <- NA
r <- log10(r)
r <- r - min(values(r), na.rm = TRUE)

plot(s$dem.abruzzi)
```

<img src="man/figures/README-3D plot-1.png" width="100%" />

``` r
plot(s$work, col = topo.colors(100))
```

<img src="man/figures/README-3D plot-2.png" width="100%" />

``` r
# plot3D(s$dem.abruzzi, 
#        shininess = 100, 
#        specular = "black", 
#        fog = FALSE)
# 
# plot3D(s$dem.abruzzi, 
#        drape = log10(s$work), 
#        col = topo.colors(100), 
#        shininess = 100, 
#        specular = "black",
#        fog = FALSE)
```

``` r
library(enerscape)
library(raster)
library(gdistance)
#> Loading required package: igraph
#> 
#> Attaching package: 'igraph'
#> The following object is masked from 'package:raster':
#> 
#>     union
#> The following objects are masked from 'package:stats':
#> 
#>     decompose, spectrum
#> The following object is masked from 'package:base':
#> 
#>     union
#> Loading required package: Matrix
#> 
#> Attaching package: 'Matrix'
#> The following objects are masked from 'package:terra':
#> 
#>     expand, pack
#> 
#> Attaching package: 'gdistance'
#> The following object is masked from 'package:igraph':
#> 
#>     normalize

s <- readRDS("../data/stack3D.rds")
p <- data.frame(x = c(863047, 872700), y = c(4667429, 4693600))
origin <- p[1, ]
destination <- p[2, ]
conduct <- 1 / s$work
shortest <- list()
lcp <- transition(conduct, function(x) min(x), 16)
shortest <- shortestPath(lcp, 
                         as.numeric(origin),
                         as.numeric(destination), 
                         output = "SpatialLines")
# least cost path
plot(s$dem.abruzzi)
lines(shortest)
points(p, pch = 20)
```

<img src="man/figures/README-case study 1-1.png" width="100%" />

``` r
# circuitscape cumulative current
conn <- raster("../data/cs_cum_curmap.asc")
plot(log10(conn), col = heat.colors(100), 
     main = expression("log"[10]*"(connectivity)"))
points(p, pch = 20, col = "steelblue")
contour(s$dem.abruzzi, add = TRUE)
```

<img src="man/figures/README-case study 1-2.png" width="100%" />

Here, we derive the energy landscape and then remove presence point of a
species outside the 95% quantile to mask the habitat suitability.

``` r
library(enerscape)
library(raster)

# data
set.seed(123456789)
dem <- raster("../data/dem-abruzzi.tif")
ext <- Polygon(extent(dem))
p <- spsample(ext, 25, "random")
plot(dem)
points(p)
```

<img src="man/figures/README-example 3-1.png" width="100%" />

``` r
# get energy landscape
en <- enerscape(dem, m = 140, units = "kcal")
#> Calculating slope
#> Calculating work
# mask threshold
w_val <- extract(en$work, p)
w_mask <- en$work
w_mask[w_mask <= quantile(w_val, 0.95)] <- NA
w_mask[!is.na(w_mask)] <- 1
plot(dem)
plot(w_mask, add = TRUE, col = rgb(0.2, 0.2, 0.8, 0.5), legend = FALSE)
```

<img src="man/figures/README-example 3-2.png" width="100%" /> where the
blue areas show the energetically unsuitable cells.
