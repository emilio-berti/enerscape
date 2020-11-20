
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

#dem <- rast("../data/dem-abruzzi.tif")
#plot(dem)
#en <- enerscape(dem, m = 140, units = "kcal") #typical female Marsican bear
#plot_enerscape(en, what = "work", n_contour = 5)
```

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!

``` r
library(raster)
#> Loading required package: sp
library(enerscape)
library(rasterVis)
#> Loading required package: lattice
#> Loading required package: latticeExtra

#s <- readRDS("../data/stack3D.rds")
#r <- raster("../data/cs_cum_curmap.asc")
#r[r == 0] <- NA
#r <- log10(r)
#r <- r - min(values(r), na.rm = TRUE)
#plot(s$dem.abruzzi)
#plot(s$work, col = topo.colors(100))
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

#s <- readRDS("../data/stack3D.rds")
#p <- data.frame(x = c(863047, 872700), y = c(4667429, 4693600))
#origin <- p[1, ]
#destination <- p[2, ]
#conduct <- 1 / s$work
#shortest <- list()
#lcp <- transition(conduct, function(x) min(x), 16)
#shortest <- shortestPath(lcp, 
#                         as.numeric(origin),
#                         as.numeric(destination), 
#                         output = "SpatialLines")
# least cost path
#plot(s$dem.abruzzi)
#lines(shortest)
#points(p, pch = 20)
# circuitscape cumulative current
#conn <- raster("../data/cs_cum_curmap.asc")
#plot(log10(conn), col = heat.colors(100), 
#     main = expression("log"[10]*"(connectivity)"))
#points(p, pch = 20, col = "steelblue")
#contour(s$dem.abruzzi, add = TRUE)
```

Here, we derive the energy landscape and then remove presence point of a
species outside the 95% quantile to mask the habitat suitability.

``` r
library(enerscape)
library(raster)

# data
set.seed(123456789)
#dem <- raster("../data/dem-abruzzi.tif")
#ext <- Polygon(extent(dem))
#p <- spsample(ext, 25, "random")
#plot(dem)
#points(p)
# get energy landscape
#en <- enerscape(dem, m = 140, units = "kcal")
# mask threshold
#w_val <- extract(en$work, p)
#w_mask <- en$work
#w_mask[w_mask <= quantile(w_val, 0.95)] <- NA
#w_mask[!is.na(w_mask)] <- 1
#plot(dem)
#plot(w_mask, add = TRUE, col = rgb(0.2, 0.2, 0.8, 0.5), legend = FALSE)
```

where the blue areas show the energetically unsuitable cells.
