dem <- raster("w46585_s10/w46585_s10.tif")
slope <- raster("w46585_s10/w46585_s10_slope.tif")
e <- extent(c(880841.3, 887445.7, 4669335,4677260))
dem <- crop(dem, e)
slope <- crop(slope, e)
plot(log10(dem), col = RColorBrewer::brewer.pal(9, "Blues"))
slope[slope < 50] <- NA
plot(slope, add = TRUE, col = RColorBrewer::brewer.pal(9, "Reds"))

dem_file <- "w46585_s10/w46585_s10.tif"
slope_output <- "w46585_s10/w46585_s10_slope.tif"

source("R/dem_to_slope.R")
slope <- dem_to_slope(dem_file, slope_output)
slope <- crop(slope, e)
plot(slope)
