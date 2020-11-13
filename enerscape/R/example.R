# library(terra)
# library(enerscape)
# library(gdistance)
#
# myPal <- paste0(RColorBrewer::brewer.pal(9, "Blues"), "90") #adding alpha
# dem <- rast("../data/dem-abruzzi.tif", overwrite = TRUE)
# en <- enerscape(dem, m = 140, units = "kcal") #typical female Marsican bear
# plot_enerscape(en, n_contour = 5)
#
# # data for circuitscape ----------
# dem_circ <- aggregate(dem, 5)
# work_circ <- aggregate(en$work, 5)
# #resistance matrix
# writeRaster(work_circ, "../data/abruzzi-work.tif", overwrite = TRUE)
# #the catode/anode of the circuit
# p <- data.frame(x = c(863047, 872700), y = c(4667429, 4693600))
# plot(dem_circ)
# points(p, pch = 20)
# rm(en, dem)
# gc()
# #polygons of short circuit
# p_r <- raster::rasterize(p, raster::raster(dem_circ), fun = "last")
# p_r <- rast(p_r)
# p_r <- buffer(p_r, 150)
# writeRaster(p_r, "../data/abruzzi-origins.tif", overwrite = TRUE)
#
# # circuitscape results -----------
# dem <- rast("../data/dem-abruzzi.tif")
# dem <- aggregate(dem, 5)
# cur_sum <- rast("../data/cs_cum_curmap.asc")
# hist(log10(cur_sum))
# cur_sum[cur_sum < 10^-4] <- NA
#
# pdf("../Figures/circuitscape-full.pdf", width = 6, height = 5)
# plot(log10(cur_sum), col = topo.colors(100), axes = FALSE)
# axis(side = 1, at = seq(850290, 891290, length.out = 5))
# axis(side = 2, at = seq(4664250, 4699350, length.out = 5))
# contour(dem, add = TRUE, size = 5)
# dev.off()
#
# cur_sum[cur_sum < 10^-2.7] <- NA
# cur_sum[!is.na(cur_sum)] <- 1
# cur_sum <- as.polygons(cur_sum)
#
# pdf("../Figures/circuitscape-polygon.pdf", width = 6, height = 5)
# plot(dem, col = terrain.colors(100), axes = FALSE, legend = TRUE)
# axis(side = 1, at = seq(850290, 891290, length.out = 5))
# axis(side = 2, at = seq(4664250, 4699350, length.out = 5))
# contour(dem, add = TRUE)
# plot(cur_sum, col = paste0(topo.colors(100), "90"), add = TRUE)
# dev.off()
#
# # gradient -----------
# # origin <- p[1, ]
# # destination <- p[2, ]
# # origin_col <- terra::colFromX(dem_circ, origin$x)
# # origin_row <- terra::rowFromY(dem_circ, origin$y)
# #
# # g <- as.array(dem_circ)
# # g <- matrix(g, ncol = ncol(g))
#
# # least-cost path --------
# work <- rast("../data/abruzzi-work.tif")
# origin <- p[1, ]
# destination <- p[2, ]
# cond <- 1 / work
# shortest <- list()
# plot(work)
# for (n in c(4, 8, 16)) {
#   lcp <- transition(raster::raster(cond), function(x) min(x), n)
#   shortest[[n]] <- shortestPath(lcp, as.numeric(origin), as.numeric(destination), output = "SpatialLines")
#   lines(shortest[[n]])
# }
#
# # circuitscape + LCP --------------
# dem <- rast("../data/dem-abruzzi.tif")
# dem <- aggregate(dem, 5)
# cur_sum <- rast("../data/cs_cum_curmap.asc")
# cur_sum[cur_sum < 10^-4] <- NA
#
# pdf("../Figures/circuitscape-lcp.pdf", width = 6, height = 5)
# plot(log10(cur_sum), col = topo.colors(100), axes = FALSE)
# axis(side = 1, at = seq(850290, 891290, length.out = 5))
# axis(side = 2, at = seq(4664250, 4699350, length.out = 5))
# contour(dem, add = TRUE, size = 5)
# lines(shortest[[16]], lw = 4, col = "tomato")
# dev.off()
#
#
# # iso-energy polygons (IPE) --------------
# # IEP are polygons that delimit the distance from which an animal can move from
# # a starting point given limited time T and limited energy E
#
#
# # openmap -------------
# m <- mapmisc::openmap(raster::raster(dem), fact = 5)
# plot(m)
# lines(extent(raster::raster(dem)))
