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
# en4 <- enerscape(r, 1, "kcal", 4)
# en8 <- enerscape(r, 1, "kcal", 8)
# en16 <- enerscape(r, 1, "kcal", 16)
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
# lcp <- en_lcp(en,
#               or = c(-400, -400),
#               dest = c(400, 400),
#               neigh = 16)
#
# lcp <- en_lcp(en,
#               or = c(-400, 0),
#               dest = c(400, 0),
#               neigh = 16)
#
# lcp <- en_lcp(en16,
#               simulate_random_points = TRUE,
#               rep = 10)
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
#
#
