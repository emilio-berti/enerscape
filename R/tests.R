# library(raster)
# library(enerscape)
#
# # First example with a bowl DEM ------
#
# test_raster <- function() {
#   r <- raster::raster(matrix(0, 100, 100),
#                       xmn = -500, xmx = 500,
#                       ymn = -500, ymx = 500)
#   raster::crs(r) <- "+proj=utm +datum=WGS84 +units=m"
#   for (x in 1:nrow(r)) {
#     for (y in 1:ncol(r)) {
#       r[x, y] <- exp(-((x - 50) ^ 2 + (y - 50) ^ 2) / 2500) * 250 -
#         exp(-((x - 50) ^ 2 + (y - 50) ^ 2) / 500) * 250
#     }
#   }
#   return(r)
# }
#
# dem <- test_raster()
# rasterVis::plot3D(dem, specular = "black")
# en4 <- enerscape(dem, 1, "kcal", 4)
# en8 <- enerscape(dem, 1, "kcal", 8)
# en16 <- enerscape(dem, 1, "kcal", 16)
# par(mfrow = c(3, 4), mar = c(2, 2, 2, 2))
# for (x in names(en4$rasters)) {
#   plot(en4$rasters[[x]], main = paste(x, "4 nighbors"))
# }
# for (x in names(en4$rasters)) {
#   plot(en8$rasters[[x]], main = paste(x, "8 nighbors"))
# }
# for (x in names(en4$rasters)) {
#   plot(en16$rasters[[x]], main = paste(x, "16 nighbors"))
# }
#
# rasterVis::plot3D(dem,
#                   drape = en16$rasters$Work,
#                   specular = "black",
#                   col = viridis::plasma(100))
# rasterVis::plot3D(dem,
#                   drape = en16$rasters$Conductance,
#                   specular = "black",
#                   col = viridis::viridis(100))
#
# dev.off()
# lcp <- en_lcp(en16,
#               or = c(-400, -400),
#               dest = c(400, 400))
#
# lcp <- en_lcp(en16,
#               or = c(-400, 0),
#               dest = c(400, 0))
#
# lcp <- en_lcp(en16,
#               simulate_random_points = TRUE,
#               rep = 50,
#               plot = FALSE)
#
# plot(en16$rasters$DEM, axes = FALSE, box = FALSE, main = "Elevation (m)")
# points(lcp$Origins, pch = 20, col = adjustcolor("black", alpha.f = 0.5))
# points(lcp$Destinations, pch = 20, col = adjustcolor("black", alpha.f = 0.5))
# lines(lcp$Paths, lt = 2, col = adjustcolor("black", alpha.f = 0.5))
#
# r <- en16$rasters$DEM
# p1 <- rasterize(lcp$Origins, r)
# p2 <- rasterize(lcp$Destinations, r)
# paths <- rasterize(lcp$Paths, r)
# r[!is.na(p1)] <- 200
# r[!is.na(p2)] <- 200
# r[!is.na(paths)] <- 200
# rasterVis::plot3D(en16$rasters$DEM,
#                   drape = r,
#                   specular = "black",
#                   col = c(terrain.colors(999), "black"))
#
#
# plot(en16$rasters$Work)
# for (rep in 1:10) {
#   or <- sample((-10:10), 2)
#   dest <- sample((200:500), 2)
#   lcp <- en_lcp(en16, or, dest, plot = FALSE)
#   lines(lcp$Path)
# }
#
# plot(en16$rasters$Conductance)
# for (rep in 1:25) {
#   or <- c(0, 0)
#   dest <- sample((-25:25) * 20, 2)
#   lcp <- en_lcp(en16, or, dest, plot = FALSE)
#   lines(lcp$Path)
# }
#
# plot(log10(crop(en16$rasters$Conductance, extent(-200, 200, -200, 200))))
# for (rep in 1:20) {
#   or <- c(0, 0)
#   dest <- sample((-25:25) * 20, 2)
#   lcp <- en_lcp(en16, or, dest, plot = FALSE)
#   lines(lcp$Path)
# }
#
# plot(en16$rasters$Work)
# for (rep in 1:50) {
#   or <- sample(-500:500, 2)
#   dest <- sample(-500:500, 2)
#   lcp <- en_lcp(en16, or, dest, plot = FALSE)
#   lines(lcp$Path, col = adjustcolor("black", 0.5))
# }
#
# par(mfrow = c(2, 2))
# for (theta in (1:4) / 2) {
#   walk <- en_passage(en,
#                      theta = theta,
#                      or = c(-400, -400),
#                      dest = c(400, 400),
#                      plot = FALSE)
#   plot(log10(walk), col = topo.colors(100), main = paste("Theta =", theta))
#   contour(dem, add = TRUE, col = "grey20", lt = 2, nlevels = 5)
# }
#
# par(mfrow = c(2, 2))
# for (theta in (1:4) / 2) {
#   walk <- en_passage(en,
#                      theta = theta,
#                      or = c(-400, 400),
#                      dest = c(400, -400),
#                      plot = FALSE)
#   plot(log10(walk), col = topo.colors(100), main = paste("Theta =", theta))
#   contour(dem, add = TRUE, col = "grey20", lt = 2, nlevels = 5)
# }
#
# par(mfrow = c(2, 2))
# for (theta in (1:4) / 2) {
#   walk <- en_passage(en,
#                      theta = theta,
#                      or = c(-400, 0),
#                      dest = c(400, 0),
#                      plot = FALSE)
#   plot(log10(walk), col = topo.colors(100), main = paste("Theta =", theta))
#   contour(dem, add = TRUE, col = "grey20", lt = 2, nlevels = 5)
# }
#
# #First example with a peak DEM ------
# dem <- 1000 - test_raster()
# rasterVis::plot3D(dem, specular = "black")
# en4 <- enerscape(dem, 1, "kcal", 4)
# en8 <- enerscape(dem, 1, "kcal", 8)
# en16 <- enerscape(dem, 1, "kcal", 16)
# par(mfrow = c(3, 4), mar = c(2, 2, 2, 2))
# for (x in names(en4$rasters)) {
#   plot(en4$rasters[[x]], main = paste(x, "4 nighbors"))
# }
# for (x in names(en4$rasters)) {
#   plot(en8$rasters[[x]], main = paste(x, "8 nighbors"))
# }
# for (x in names(en4$rasters)) {
#   plot(en16$rasters[[x]], main = paste(x, "16 nighbors"))
# }
#
# rasterVis::plot3D(dem, drape = en16$rasters$Work, specular = "black")
# rasterVis::plot3D(dem, drape = en16$rasters$Conductance, specular = "black")
#
# dev.off()
# lcp <- en_lcp(en16,
#               or = c(-400, -400),
#               dest = c(400, 400))
#
# lcp <- en_lcp(en16,
#               or = c(-400, 0),
#               dest = c(400, 0))
#
# lcp <- en_lcp(en16,
#               simulate_random_points = TRUE,
#               rep = 50)
#
# r <- en16$rasters$DEM
# p1 <- rasterize(lcp$Origins, r)
# p2 <- rasterize(lcp$Destinations, r)
# paths <- rasterize(lcp$Paths, r)
# r[!is.na(p1)] <- 800
# r[!is.na(p2)] <- 800
# r[!is.na(paths)] <- 800
# rasterVis::plot3D(en16$rasters$DEM,
#                   drape = r,
#                   specular = "black",
#                   col = c("black", terrain.colors(999)))
#
# plot(en16$rasters$Work)
# for (rep in 1:10) {
#   or <- sample((-10:10), 2)
#   dest <- sample((200:500), 2)
#   lcp <- en_lcp(en16, or, dest, plot = FALSE)
#   lines(lcp$Path)
# }
#
# plot(en16$rasters$Conductance)
# for (rep in 1:25) {
#   or <- c(0, 0)
#   dest <- sample((-25:25) * 20, 2)
#   lcp <- en_lcp(en16, or, dest, plot = FALSE)
#   lines(lcp$Path)
# }
#
# plot(log10(crop(en16$rasters$Conductance, extent(-200, 200, -200, 200))))
# for (rep in 1:20) {
#   or <- c(0, 0)
#   dest <- sample((-25:25) * 20, 2)
#   lcp <- en_lcp(en16, or, dest, plot = FALSE)
#   lines(lcp$Path)
# }
#
# plot(en16$rasters$Work)
# for (rep in 1:50) {
#   or <- sample(-500:500, 2)
#   dest <- sample(-500:500, 2)
#   lcp <- en_lcp(en16, or, dest, plot = FALSE)
#   lines(lcp$Path, col = adjustcolor("black", 0.5))
# }
#
# par(mfrow = c(2, 2))
# for (theta in (1:4) / 2) {
#   walk <- en_passage(en,
#                      theta = theta,
#                      or = c(-400, -400),
#                      dest = c(400, 400),
#                      plot = FALSE)
#   plot(log10(walk), col = topo.colors(100), main = paste("Theta =", theta))
#   contour(dem, add = TRUE, col = "grey20", lt = 2, nlevels = 5)
# }
#
# par(mfrow = c(2, 2))
# for (theta in (1:4) / 2) {
#   walk <- en_passage(en,
#                      theta = theta,
#                      or = c(-400, 400),
#                      dest = c(400, -400),
#                      plot = FALSE)
#   plot(log10(walk), col = topo.colors(100), main = paste("Theta =", theta))
#   contour(dem, add = TRUE, col = "grey20", lt = 2, nlevels = 5)
# }
#
# par(mfrow = c(2, 2))
# for (theta in (1:4) / 2) {
#   walk <- en_passage(en,
#                      theta = theta,
#                      or = c(-400, 0),
#                      dest = c(400, 0),
#                      plot = FALSE)
#   plot(log10(walk), col = topo.colors(100), main = paste("Theta =", theta))
#   contour(dem, add = TRUE, col = "grey20", lt = 2, nlevels = 5)
# }
#
# # cyclist ----------
# dem <- aggregate(raster("~/Documents/dem.tif"), 5)
# plot(dem)
# par(mfrow = c(2, 2))
# en <- enerscape(dem, 50, "kcal", 16)
# plot(en$rasters$Work)
# for (v in c(10, 30, 50)) {
#   cyclist <- enerscape(dem, 50, "kcal", 16, "cycling", v)
#   plot(cyclist$rasters$Work)
# }
# plot(en$rasters$Work)
# for (m in c(40, 50, 60)) {
#   cyclist <- enerscape(dem, m, "kcal", 16, "cycling", 25)
#   plot(cyclist$rasters$Work)
# }
#
# plot(en$rasters$Conductance)
# for (v in c(10, 30, 50)) {
#   cyclist <- enerscape(dem, 50, "kcal", 16, "cycling", v)
#   plot(cyclist$rasters$Conductance)
# }
# plot(en$rasters$Conductance)
# for (m in c(50, 60, 70)) {
#   cyclist <- enerscape(dem, m, "kcal", 16, "cycling", 25)
#   plot(cyclist$rasters$Conductance)
# }
#
# plot(en$rasters$DEM)
# for (v in c(10, 30, 50)) {
#   cyclist <- enerscape(dem, 50, "kcal", 16, "cycling", v)
#   en_lcp(cyclist, simulate_random_points = TRUE)
# }
