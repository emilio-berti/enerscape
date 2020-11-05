library(terra)
library(enerscape)
library(rasterVis)


myPal <- paste0(RColorBrewer::brewer.pal(9, "Blues"), "90") #adding alpha
dem <- rast("../data/dem-abruzzi.tif", overwrite = TRUE)
en <- enerscape(dem, m = 140000) #typical female Marsican bear
writeRaster(en$work, "../data/work-abruzzi.tif", overwrite = TRUE)
plot(dem)
a <- dismo::randomPoints(raster::raster(dem), 50)
points(a, pch = 20)
p_r <- raster::rasterize(a, raster::raster(dem))
p_r <- rast(p_r)
p_r <- buffer(p_r, 150)
plot(dem)
points(a)
plot(p_r, add = TRUE)
writeRaster(p_r, "../data/abruzzi-origins.tif")
# ext <- dem@ptr$extent$vector
# centroid <- data.frame(x = ext[2] - (ext[2] - ext[1]) / 2,
#                        y = ext[4] - (ext[4] - ext[3]) / 2)
# cr <- buffer(vect(centroid), 10000)
# area <- crop(en, cr)
# dem <- crop(dem, cr)

plot_enerscape(en, what = "work", contour = FALSE)


buff <- buffer(vect(centroid), 200000)
range <- mask(en$work, buff)
range <- aggregate(range, 50)
range[range >= 7000] <- NA
range <- buffer(range, 1)
IEP <- rasterToPolygons(raster(range), dissolve = TRUE)

plot(en$dem, axes = FALSE, col = terrain.colors(100))
plot(IEP, add = TRUE, col = c(rgb(0, 0, 0, 0), rgb(0, 0, 1, 0.25)))
points(centroid, pch = 20, cex = 3)
