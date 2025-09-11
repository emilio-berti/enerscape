# Energy Landscapes With Enerscape

Energy landscapes are spatially explicit projections of the energy cost
of travel (kcal or J) for an animal. In enerscape, energy landscapes
depend on the body mass of the animal and the incline between locations,
following the ARC model from Pontzer (2016):
<https://doi.org/10.1098/rsbl.2015.0935>. For a general overview of
energy landscapes, refer to Berti et al. (2021):
<https://doi.org/10.1111/2041-210X.13734>.

    library(terra)
    library(enerscape)

Enerscape comes with the default dataset *sirente*; this is a matrix
with values the digital elevation model (DEM) for Monte Sirente in
Central Italy:

    data("sirente")
    dem <- rast(
      sirente, 
      crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
      extent = ext(879340, 885280, 4672880, 4676810)
    )
    plot(dem)

![](doc/enerscape_files/figure-markdown_strict/sirente-1.png)

I provided the correct coordinate reference system (CRS) and the extent
of the DEM; in most cases, this will already be present in downloaded
raster layers.

Enerscape assumes that the coordinate system is in meters:

    crs(dem, proj = TRUE)
    #> [1] "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"

You **have** to check this by yourself to make sure *+units=m* or
enerscape results will not be meaningful. Enerscape checks if this is
the case and returns a warning if *+units* does not match *m*, but it
will still return its output.

The energy landscapes can be obtained with the *enerscape()* function.
There are two mandatory inputs: *dem*, which is the SpatRaster of the
DEM, and *mass*, which is the body mass of the animal (kg).

    en <- enerscape(dem, 10, neigh = 4, unit = "kcal")
    #> DEM is assumed to have planar CRS in meters.

Before visualizing it, it is worth nothing that, from my experience,
energy landscape values are usually right-skewed, i.e. with most cells
having low values and few cells having high values. In such cases, it is
useful to log-transform energy landscape for visualization:

    en_log <- log(en)
    plot(en_log)
    terra::contour(dem, add = TRUE, nlevels = 5)

![](doc/enerscape_files/figure-markdown_strict/visualize-1.png)

    en_log <- crop(en_log, ext(881306, 882853, 4674928, 4675984))
    plot(en_log)
    terra::contour(dem, add = TRUE, nlevels = 20)

![](doc/enerscape_files/figure-markdown_strict/zoom-1.png)

# Initialization files for Circuitscape/Omniscape

Circuitscape and Omniscape are circuit theory approaches to calculate
connectivity in landscapes. The main idea of these two algorithm is that
resistances in the landscape (e.g. due to energy costs) behave like
resistances in electric circuit. For instance, parallel resistances will
give a lower overall resistance than each of the individual resistances
(<https://en.wikipedia.org/wiki/Series_and_parallel_circuits#Resistance_units_2>).

The major advantage of a circuit theory approach is that it consider all
possible paths of travel, instead of focusing only on one, e.g. a least
cost path. Circuity theory has, thus, more realistic assumptions on how
animals behave and I preferred Circuitscape/Omniscape over least-cost
path analysis. Following *rgeos* and *rgdal*, I rewrote the core of the
package in C++, removing the dependency also on *gdistance*. In doing
so, I also removed the function to calculate least-cost paths using the
transition matrix approach from *gdistance*. If you want to use these
functions, check older releases on enerscape
(<https://github.com/emilio-berti/enerscape/releases>; until 0.1.3,
included).

Currently, there are no implementations of Circuitscape/Omniscape in R.
There are, however, two great Julia libraries that are extremely easy to
use:

- Circuitscape.jl: <https://www.juliapackages.com/p/circuitscape>.
- Omniscape.jl: <https://www.juliapackages.com/p/omniscape>

Both are one-liners in Julia, e.g. `run_omniscape("omniscape.ini")`. The
major obstacle in using these libraries is to create the initialization
file (*.ini*).

## Circuitscape

To create the initialization file for Circuitscape, enerscape has the
dedicated function *circuitscape\_skeleton()*:

    # two random points
    p <- spatSample(en, 2, xy = TRUE)[, c("x", "y")]
    circuitscape_skeleton(en, path = tempdir(), points = p)

Circuitscape requires two locations (*points*) to calculate the
connectivity among them. A *circuitscape.ini* file will be write to the
*path* directory. From within this directory, start Julia and run:

    julia> using Circuitscape
    julia> compute("circuitscape.ini")

    [ Info: 2022-12-16 10:05:18 : Precision used: Double
    [ Info: 2022-12-16 10:05:18 : Reading maps
    [ Info: 2022-12-16 10:05:18 : Resistance/Conductance map has 231472 nodes
    [ Info: 2022-12-16 10:05:21 : Solver used: AMG accelerated by CG
    [ Info: 2022-12-16 10:05:22 : Graph has 231472 nodes, 2 focal points and 1 connected components
    [ Info: 2022-12-16 10:05:22 : Total number of pair solves = 1
    [ Info: 2022-12-16 10:05:24 : Time taken to construct preconditioner = 2.510170134 seconds
    [ Info: 2022-12-16 10:05:24 : Time taken to construct local nodemap = 0.021110616 seconds
    [ Info: 2022-12-16 10:05:27 : Solving pair 1 of 1
    [ Info: 2022-12-16 10:05:27 : Time taken to solve linear system = 0.512068804 seconds
    [ Info: 2022-12-16 10:05:28 : Time taken to calculate current maps = 0.82549585 seconds
    [ Info: 2022-12-16 10:05:29 : Time taken to complete job = 11.5802219
    3×3 Matrix{Float64}:
     0.0  1.0       2.0
     1.0  0.0       0.559379
     2.0  0.559379  0.0

Output is in the same folder as the *circuitscape.ini* file.

## Omniscape

Omniscape basically runs omniscape between all location within a window
radius, repeating this step for all raster cells. Omniscape does not
require input points, but it needs the radius of the moving window:

    omniscape_skeleton(en, path = tempdir(), radius = 10)

A *omniscape.ini* file will be write to the *path* directory. From
within this directory, start Julia and run:

    julia> using Oircuitscape
    julia> run_omniscape("circuitscape.ini")

    [ Info: Starting up Omniscape with 1 workers and double precision
    [ Info: Using Circuitscape with the cg+amg solver...
    [ Info: Solving moving window targets...
     days,Progress: 100%|                                | Time: 0:08:08
    [ Info: Time taken to complete job: 490.2733 seconds
    [ Info: Outputs written to /tmp/RtmpXZb2Hv//tmp/RtmpXZb2Hv/omniscape
    (Union{Missing, Float64}[missing missing … missing missing; missing 3.4477693535486376 … 4.462051188132685 missing; … ; missing 10.365746449782868 … 4.186837622531591 missing; missing missing … missing missing], Union{Missing, Float64}[missing missing … missing missing; missing 0.652059842432581 … 0.743080786816455 missing; … ; missing 0.7753872880414857 … 0.7333620170587554 missing; missing missing … missing missing])

Output is in the folder *omniscape* within the folder specified in
*path*.

# Energy Lanscapes for Walking People

As of enerscape 1.2 I implemented the energy cost model for walking
people from Looney et al. (2019). This can be calculated using the
`humanscape()` function. The arguments are the same as for
`enerscape()`, except for an additional argument `v`, which is the
walking speed (m/s). If `v` is not specified, it is assumed to be
`v = 1.39`, which optimize the energy costs.

![](doc/enerscape_files/figure-markdown_strict/human%20speed%20costs-1.png)

    en <- lapply(
      seq(0.5, 2.5, by = .5),
      \(v) humanscape(dem, 10, v = v, neigh = 4, unit = "kcal")
    )
    #> DEM is assumed to have planar CRS in meters.
    #> DEM is assumed to have planar CRS in meters.
    #> DEM is assumed to have planar CRS in meters.
    #> DEM is assumed to have planar CRS in meters.
    #> DEM is assumed to have planar CRS in meters.
    en <- rast(en)
    names(en) <- paste0("v = ", seq(0.5, 2.5, by = .5))
    en_log <- log(en)
    panel(en_log, nc = 3)

![](doc/enerscape_files/figure-markdown_strict/humanscape-1.png)

# Changelog

## 1.2.0

Add `humanscape()` to calculate energy costs of walking people.

## 1.1.0

This follows a correction on the Pontzer model:
<https://doi.org/10.1098/rsbl.2023.0492>.

## 1.0.0

This follows the transfer of functionalists of old GIS packages to
*terra*, with *rgdal*, *rgeos* scheduled to be retired in 2023:
<https://r-spatial.org/r/2022/04/12/evolution.html>. I removed
dependencies on *gDistance*, *raster*, *sp*, *rgeos*, and *rgdal*.

Energy landscapes are now calculated using a zonal (kernel) based
method, implemented in C++ functions. This is faster that previous
versions, but it does not return the transition matrix or the
conductance matrix. Because of this, least cost paths functions are not
supported any more. Instead, use circuitscape or omniscape; see
*circuitscape\_skeleton()* and *omniscape\_skeleton()* to generate the
initialization files to run in Julia.

The cyclist model is also not supported any more; custom models must be
written in the C++ functions. I plan to add a customizable function soon
to do that.

# References

Pontzer, H. (2016). A unified theory for the energy cost of legged
locomotion. Biology letters, 12(2), 20150935.

Berti, E., Davoli, M., Buitenwerf, R., Dyer, A., Hansen, O. L., Hirt,
M., … & Vollrath, F. (2022). The r package enerscape: A general energy
landscape framework for terrestrial movement ecology. Methods in Ecology
and Evolution, 13(1), 60-67.

Looney, D. P., Santee, W. R., Hansen, E. O., Bonventre, P. J., Chalmers,
C. R., & Potter, A. W. (2019). Estimating energy expenditure during
level, uphill, and downhill walking. Med. Sci. Sports Exerc., 51(9),
1954-1960.
