#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#' @param slope is the slope raster
#' @param m is the body size of the species
#' @return raster file of the energetic landscape of locomotory costs

enerscape <- function(slope, m) {
  g <- 9.80665 #gravitational pull
  eff <- m ^ 0.15 #efficiency of locomotion - filler, to be determined
  h <- sin(0.01745329 * slope) #0.01745329 = degrees to radians conversion
  W <- m * g * h * eff #work (Joule)
  return(W)
}

# don't run
par(mfrow = c(2, 2))
for (x in 10^seq_len(4)) {
  r <- enerscape(slope, x)
  r[r >= x * 10] <- NA
  plot(r)
}
