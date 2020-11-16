# library(tidyverse)
# library(cowplot)
#
# vect_cot <- function(m, slope, deg_to_rad = 0.01745329) {
#   ans <- 8 * m^(-0.34) + 100 * (1 + sin(deg_to_rad * 2 * slope - deg_to_rad * 74)) * m^(-0.12) #joule / (kg * m)
#   return(ans)
# }
#
# vect_work <- function(cot, m, dx, slope, deg_to_rad = 0.01745329) {
#   ans <- cot * m * dx / cos(deg_to_rad * slope)
#   return(ans)
# }
#
# d <- tibble(Slope = rep(0:8 * 10, 10),
#                 Mass = rep(10^(1:10), each = 9)) %>%
#   mutate(COT = vect_cot(Mass, Slope),
#          Work = vect_work(COT, Mass, 1, Slope))
#
# plot_grid(
# d %>%
#   ggplot() +
#   aes(Mass, Work, col = Slope, group = Slope) +
#   geom_point() +
#   geom_line() +
#   scale_x_log10() +
#   scale_y_log10() +
#   scale_color_gradient(breaks = unique(d$Slope),
#                        low = "steelblue", high = "tomato") +
#   theme_bw(),
# d %>%
#   ggplot() +
#   aes(Slope, Work, col = log10(Mass), group = log10(Mass)) +
#   geom_point() +
#   geom_line() +
#   scale_y_log10() +
#   scale_color_gradient(breaks = log10(unique(d$Mass)),
#                        low = "chocolate1", high = "chocolate4") +
#   theme_bw()
# )
