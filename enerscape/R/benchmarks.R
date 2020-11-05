# # terra vs raster -----------
# r1 <- terra::rast("w46585_s10/w46585_s10_slope.tif")
# r2 <- raster::raster("w46585_s10/w46585_s10_slope.tif")
# terra_time <- c()
# raster_time <- c()
# for (reps in seq_len(10) * 10){
#   terra_time <- append(terra_time,
#                        rbenchmark::benchmark(enerscape(r1, 1),
#                                              replications = reps)$elapsed)
#   raster_time <- append(raster_time,
#                         rbenchmark::benchmark(enerscape(r2, 1),
#                                               replications = reps)$elapsed)
# }
#
# ans <- data.frame(Replication = seq_len(10) * 10,
#                   Time = c(terra_time, raster_time),
#                   Package = rep(c("terra", "raster"),
#                                 each = length(terra_time)))
#
# plot(ans$Replication, ans$Time, col = factor(ans$Package), pch = 20)
